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
    // TODO this is getting moved to pass3, but it actually seems to be useful
    // to run here too. maybe keep for now!
    //return graph

    // Find vertices with exactly two roads involved. They're redundant.
    val verts = new HashMap[Coordinate, MutableSet[PreEdge2]] with MultiMap[Coordinate, PreEdge2]
    for (e <- graph.edges) {
      verts.addBinding(e.from, e)
      verts.addBinding(e.to, e)
    }

    var changed = true
    while (changed) {
      verts.find(v => v._2.size == 2) match {
        case Some((_, roads)) => {
          val r1 = roads.head
          val r2 = roads.tail.head
          // TODO if type, lanes, or oneway doesnt match, bail out
          val replace = merge_roads(r1, r2)

          graph.edges = graph.edges.filter(e => e != r1 && e != r2)
          graph.edges += replace

          verts.removeBinding(r1.from, r1)
          verts.removeBinding(r1.to, r1)
          verts.removeBinding(r2.from, r2)
          verts.removeBinding(r2.to, r2)
          verts.addBinding(replace.from, replace)
          verts.addBinding(replace.to, replace)
        }
        case None => changed = false
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
        r1.from, r2.to, r1.points ++ r2.points.tail,
        r1.merge_dat(r2)
      )
    else if (r1.to == r2.to)
      new PreEdge2(
        r1.from, r2.from, r1.points ++ r2.points.reverse.tail,
        r1.merge_dat(r2)
      )
    else
      throw new Exception(s"$r1 and $r2 not mergable!")
}
