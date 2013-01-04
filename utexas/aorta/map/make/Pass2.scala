// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.map.Coordinate

class Pass2(old_graph: PreGraph1) {
  val graph = new PreGraph2(old_graph)

  def run(): PreGraph2 = {
    // TODO this isn't working yet.
    return graph

    // Find vertices with exactly two roads involved. They're redundant.
    val verts = new HashMap[Coordinate, MutableSet[PreEdge2]] with MultiMap[Coordinate, PreEdge2]
    for (e <- graph.edges) {
      verts.addBinding(e.from, e)
      verts.addBinding(e.to, e)
    }

    var changed = true
    while (changed) {
      val slice = verts.filter(v => v._2.size == 2)
      if (slice.nonEmpty) {
        val roads = slice.head._2
        val r1 = roads.head
        val r2 = roads.tail.head
        val replace = merge_roads(r1, r2)

        graph.edges = graph.edges.filter(e => e != r1 && e != r2)
        graph.edges += replace
        //println(s"$r1 and $r2 became $replace")

        verts.removeBinding(r1.from, r1)
        verts.removeBinding(r1.to, r1)
        verts.removeBinding(r2.from, r2)
        verts.removeBinding(r2.to, r2)
        verts.addBinding(replace.from, replace)
        verts.addBinding(replace.to, replace)
      } else {
        changed = false
      }
    }

    return graph
  }

  // What's the internal vertex? Destroy it.
  def merge_roads(r1: PreEdge2, r2: PreEdge2): PreEdge2 =
    if (r1.from == r2.from)
      new PreEdge2(
        r1.to, r2.to, r1.points.reverse ++ r2.points.tail,
        r1.merge_dat(r2)
      )
    else if (r1.from == r2.to)
      new PreEdge2(
        r2.from, r1.to, r2.points ++ r1.points.tail,
        r1.merge_dat(r2)
      )
    else if (r1.to == r2.from)
      new PreEdge2(
        r2.from, r1.to, r1.points ++ r2.points.tail,
        r1.merge_dat(r2)
      )
    else if (r1.to == r2.to)
      new PreEdge2(
        r1.from, r2.from, r1.points ++ r2.points.tail.reverse,
        r1.merge_dat(r2)
      )
    else
      throw new Exception(s"$r1 and $r2 not mergable!")
}
