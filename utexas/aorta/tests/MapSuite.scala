// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import scala.collection.mutable.Stack
import scala.collection.mutable.{HashSet => MutableHashSet}

import java.io.File

import utexas.aorta.map.make.Builder
import utexas.aorta.map.{Graph, Traversable}
import utexas.aorta.cfg

// TODO Use ScalaTests or another framework.

object MapSuite {
  def main(args: Array[String]) = {
    for (fn <- maps) {
      println(s"Building $fn...")
      val output = Builder.convert(fn)
      println(s"Loading $output...")
      // TODO add tests to make sure the XML encoding still yields the same map
      val graph = Graph.load(output)
      println(s"Checking $output...")
      check_map(graph)
      println("--------------------------------------------------")
    }
  }

  // Must run tests from the root project directory
  def maps = new File("osm/").listFiles.map(_.getPath)

  def check_map(g: Graph) = {
    check_emptiness(g)
    check_connectivity(g)
    check_turn_conflicts(g)
    check_geometry(g)
  }

  // Make sure things aren't empty
  def check_emptiness(g: Graph) = {
    for (r <- g.roads) {
      if (r.all_lanes.isEmpty) {
        throw new Exception(s"$r has no lanes")
      }
    }

    for (v <- g.vertices) {
      if (v.turns.isEmpty) {
        throw new Exception(s"$v has no turns")
      }
    }
  }

  // Every edge should be reachable from every other edge, barring approaches
  // that try to lane-change in too-short areas.
  def check_connectivity(g: Graph) = {
    // This is a weak check that just looks at individual edges, not connected
    // components of the whole graph.
    for (e <- g.edges) {
      if (e.next_turns.isEmpty) {
        throw new Exception(s"$e leads nowhere")
      }
      if (e.prev_turns.isEmpty) {
        throw new Exception(s"Nothing leads to $e")
      }
    }

    // This is another weak check that just floods from some random edge and
    // sees how many edges it reaches. It doesn't guarantee total connectedness,
    // but we'll trust Tarjan's algorithm in pass 3 of construction for now.
    val visit_me = new Stack[Traversable]
    val visited = new MutableHashSet[Traversable]
    val start = g.edges.head
    visit_me.push(start)
    visited += start
    
    while (visit_me.nonEmpty) {
      val current = visit_me.pop
      for (next <- current.leads_to if !visited.contains(next)) {
        visited += next
        visit_me.push(next)
      }
    }

    // Did we see everything?
    val size1 = visited.size
    val size2 = g.traversables.size
    if (size1 != size2) {
      throw new Exception(
        s"Reached $size1 traversables from $start, but should have found $size2"
      )
    }
  }

  // Conflicts should be symmetric.
  def check_turn_conflicts(g: Graph) = {
    for (e <- g.edges) {
      for (t1 <- e.next_turns) {
        for (t2 <- t1.conflicts) {
          if (!t2.conflicts_with(t1)) {
            throw new Exception(s"Asymmetric turn conflict between $t1 and $t2")
          }
        }
      }
    }
  }

  // OSM leads to short edges and long turns, but impose some limits.
  def check_geometry(g: Graph) = {
    for (e <- g.edges) {
      if (e.length <= cfg.epsilon || e.length.isNaN) {
        throw new Exception(s"$e has length ${e.length}")
      }
    }

    // Make sure cached length is correct.
    g.traversables.foreach(t => {
      val l = t.lines.foldLeft(0.0)((a, b) => a + b.length)
      if (l != t.length) {
        println(s"$t has recorded length ${t.length} and computed $l")
      }
    })
  }
}
