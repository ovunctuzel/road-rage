package utexas.map.make

import scala.collection.mutable.{HashSet => MutableSet}

import utexas.map.Coordinate

import utexas.{Util, cfg}

class Pass2(old_graph: PreGraph1) {

  def run(): PreGraph2 = {
    // This splits the edges
    val graph = new PreGraph2(old_graph)

    // TODO take a commandline to disable this
    // TODO this process is non-deterministic due to sets!

    Util.log("Extending short edges")
    var cnt = 0

    // We want to have guarantees about minimum edge length for the sake of not
    // having weird intersections clumped together.

    // Roads are always a bit longer than edges, due to trimming edge lines
    // back. So to be conservative, trim a longer road size to achieve the min
    // edge len requirement.
    val short = cfg.min_edge_len * 2.0   // TODO units, choice?
    def is_shorty(e: PreEdge2) = e.length < short
    def find_shorties(ls: Iterable[PreEdge2]) = ls.filter(is_shorty)
    val shorties = new MutableSet[PreEdge2]
    shorties ++= find_shorties(graph.edges)

    while (!shorties.isEmpty) {
      cnt += 1

      // I'd really name this "metric_space_challenged", but it makes the lines too long
      val shorty = shorties.head

      // Choose the vertex we're connected to that has the least number of
      // shorties right now.
      // (TODO We could arbitrarily choose to nuke the other one instead, but
      // then we mess with the geometry of more decent roads.)
      val from_sz = find_shorties(graph.verts(shorty.from)).size
      val to_sz   = find_shorties(graph.verts(shorty.to)).size
      val nuke_vert = if (from_sz <= to_sz) shorty.from else shorty.to

      // Get rid of this vertex by making everything it connects end at the
      // other side of 'shorty' instead.
      val fuse_vert = if (shorty.from == nuke_vert) shorty.to else shorty.from
      assert(nuke_vert != fuse_vert)
      val tweak_edges = graph.verts(nuke_vert)

      // 'shorty' itself is actually DISAPPEARING!
      // TODO so save any metadata from it, if we need to.
      shorties -= shorty
      graph.edges = graph.edges.filter(bad_e => bad_e != shorty)
      //Util.log("Nixing shorty edge " + shorty.id)

      for (e <- tweak_edges if e != shorty) {
        // remove it if it was there
        shorties -= e

        // TODO somehow we can create cul-de-sacs... avoid doing that.
        if ((e.from == nuke_vert && e.to   == fuse_vert) ||
            (e.to   == nuke_vert && e.from == fuse_vert))
        {
          //Util.log("Due to fusion, removing new cul-de-sac " + e.id)
          graph.edges = graph.edges.filter(bad_e => bad_e != e)
        } else {
          // It's easier to modify the PreEdge. Oh well.

          // Stitch together which side?
          // TODO you can toggle these styles... both are geometrically bunk.
          if (e.from == nuke_vert) {
            //e.points = fuse_vert :: e.points
            e.points(0) = fuse_vert
          } else {
            assert(e.to == nuke_vert)
            e.points(e.points.size - 1) = fuse_vert
            //e.points :+= fuse_vert
          }
          
          if (is_shorty(e)) {
            // Still have more work to do on this guy
            shorties += e
          }
        }
      }
    }

    Util.log("Fused " + cnt + " pairs of vertices")

    return graph
  }
}
