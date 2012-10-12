// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.map.make.{Builder, Reader}
import utexas.aorta.map.Graph

import java.io.File

// TODO Use ScalaTests or another framework.

object MapSuite {
  def main(args: Array[String]) = {
    for (fn <- maps) {
      println(s"Building $fn...")
      val output = Builder.convert(fn)
      println(s"Loading $output...")
      val graph = (new Reader(output)).load_map
      println(s"Checking $output...")
      check_map(graph)
    }
  }

  // Must run tests from the root project directory
  def maps = new File("dat/").listFiles.map(_.getPath).filter(_.endsWith(".osm"))

  def check_map(g: Graph) = {
    check_connectivity(g)
    check_turn_conflicts(g)
    check_geometry(g)
  }

  // This is a weak check that just looks at individual edges, not connected
  // components of the whole graph.
  def check_connectivity(g: Graph) = {
    for (e <- g.edges) {
      if (e.next_turns.size == 0) {
        throw new Exception(s"$e leads nowhere")
      }
      if (e.prev_turns.size == 0) {
        throw new Exception(s"Nothing leads to $e")
      }
    }
  }

  // Conflicts should be symmetric.
  def check_turn_conflicts(g: Graph) = {
    for (e <- g.edges) {
      for (t1 <- e.next_turns) {
        for (t2 <- t1.conflicts) {
          if (!t2.conflicts(t1)) {
            throw new Exception(s"Asymmetric turn conflict between $t1 and $t2")
          }
        }
      }
    }
  }

  // OSM leads to short edges and long turns, but impose some limits.
  def check_geometry(g: Graph) = {
    // Long turns causes agents to "float" over the map as they chase a distant edge
    val max_turn_length = 50.0  // TODO cfg
    for (t <- g.turns if t.length > max_turn_length) {
      throw new Exception(s"$t is excessively long")
    }

    for (e <- g.edges) {
      if (e.length == 0.0 || e.length.isNaN) {
        throw new Exception(s"$e has length ${e.length}")
      }
    }
  }
}
