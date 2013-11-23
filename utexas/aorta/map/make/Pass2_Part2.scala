// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.Coordinate

import utexas.aorta.common.{Util, cfg}

class Pass2_Part2(graph: PreGraph2) {
  def run() {
    Util.log("Merging short roads...")

    // This temporary map lets us avoid tons of repeated scans
    val edges_from_vert = new mutable.HashMap[Coordinate, mutable.MutableList[PreEdge2]]()
    edges_from_vert ++= graph.edges.groupBy(_.from)
    val edges_to_vert = new mutable.HashMap[Coordinate, mutable.MutableList[PreEdge2]]()
    edges_to_vert ++= graph.edges.groupBy(_.to)
    val nil = new mutable.MutableList[PreEdge2]()

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
        for (e <- edges_from_vert.getOrElse(nuke_vert, nil)) {
          e.from = replace_vert
          if (!edges_from_vert.contains(replace_vert)) {
            edges_from_vert(replace_vert) = new mutable.MutableList[PreEdge2]()
          }
          edges_from_vert(replace_vert) += e
        }
        for (e <- edges_to_vert.getOrElse(nuke_vert, nil)) {
          e.to = replace_vert
          if (!edges_to_vert.contains(replace_vert)) {
            edges_to_vert(replace_vert) = new mutable.MutableList[PreEdge2]()
          }
          edges_to_vert(replace_vert) += e
        }
      }
    }
  }
}
