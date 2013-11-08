// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.immutable.TreeMap
import scala.collection.mutable.{TreeSet => MutableSet}

import utexas.aorta.map.Coordinate

import utexas.aorta.common.{Util, cfg}

class Pass2(old_graph: PreGraph1) {
  val graph = new PreGraph2(old_graph)

  def run(): PreGraph2 = {
    merge_short_roads()

    // Do this second
    fix_degenerate_verts()

    return graph
  }

  // Find vertices with exactly two roads involved. They're redundant.
  private def fix_degenerate_verts() = {
    Util.log("Collapsing degenerate vertices...")
    var verts = TreeMap.empty[Coordinate, MutableSet[PreEdge2]]
    def addBinding(pt: Coordinate, e: PreEdge2) = {
      if (!verts.contains(pt)) {
        verts += ((pt, new MutableSet[PreEdge2]()))
      }
      verts(pt) += e
    }
    def removeBinding(pt: Coordinate, e: PreEdge2) = {
      if (verts.contains(pt)) {
        verts(pt) -= e
        if (verts(pt).isEmpty) {
          verts -= pt
        }
      }
    }

    for (e <- graph.edges) {
      addBinding(e.from, e)
      addBinding(e.to, e)
    }

    // TODO whys this a fixpoint?
    var changed = true
    while (changed) {
      verts.find(v => v._2.size == 2) match {
        case Some((_, roads)) => {
          val r1 = roads.head
          val r2 = roads.tail.head
          removeBinding(r1.from, r1)
          removeBinding(r1.to, r1)
          removeBinding(r2.from, r2)
          removeBinding(r2.to, r2)
          graph.edges = graph.edges.filter(e => e != r1 && e != r2)

          // TODO if type, lanes, or oneway doesnt match, bail out
          val replace = merge_roads(r1, r2)
          graph.edges += replace
          addBinding(replace.from, replace)
          addBinding(replace.to, replace)
        }
        case None => changed = false
      }
    }
  }

  // What's the internal vertex? Destroy it.
  private def merge_roads(r1: PreEdge2, r2: PreEdge2): PreEdge2 =
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

  private def merge_short_roads() = {
    Util.log("Merging short roads...")
    // Length of roads doesn't actually change by this process, but non-cul-de-sacs could become
    // cul-de-sacs, so make one pass, but be careful.
    val shorties = graph.edges.filter(r => r.length < cfg.min_road_len && !r.is_culdesac)
    for (shorty <- shorties) {
      // Make sure this shorty is still not a cul-de-sac
      if (!shorty.is_culdesac) {
        // TODO throw away the metadata on shorty. :(
        graph.edges = graph.edges.filter(e => e != shorty)
        // A weird approach that seems to work: delete the road, but magically
        // make other roads connected to shorty.from instead connect to
        // shorty.to. aka, delete the vertex shorty.from.
        Util.assert_ne(shorty.from, shorty.to)
        val nuke_vert = shorty.from
        val replace_vert = shorty.to
        for (e <- graph.edges) {
          if (e.from == nuke_vert) {
            e.from = replace_vert
          }
          if (e.to == nuke_vert) {
            e.to = replace_vert
          }
        }
      }
    }
  }
}
