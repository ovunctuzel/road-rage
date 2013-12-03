// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Road, Edge, Vertex, Turn, Line, DirectedRoad, Direction}

import utexas.aorta.common.{Util, cfg, TurnID, DirectedRoadID}

class Pass3_Part4(graph: PreGraph3) {
  def run() {
    Util.log("Tidying up geometry...")
    // TODO don't! it results in terribly short places.
    //graph.vertices.foreach(v => adjust_lines(v))

    // Another hack to restore directed roads, which get set to None when we first create the road
    // before its lanes.
    val dr_id = new DirectedRoadID(-1) // the IDs don't matter
    for (r <- graph.roads) {
      if (r.pos_lanes.nonEmpty) {
        r.pos_group = Some(new DirectedRoad(r, dr_id, Direction.POS))
      }
      if (r.neg_lanes.nonEmpty) {
        r.neg_group = Some(new DirectedRoad(r, dr_id, Direction.NEG))
      }
    }
  }

  private def adjust_lines(v: Vertex) = {
    val shortest_line = new mutable.HashMap[Edge, Line]
    for (in <- v.in_edges) {
      for (out <- v.out_edges) {
        // Correct for shifting the UI does by preventing intersections when
        // shifted over. Has a side effect of making cars stop a bit far back,
        // which is fine.
        val l1 = in.lines.last.perp_shift(0.5)
        val l2 = out.lines.head.perp_shift(0.5)

        // Just to be safe, don't allow ourselves to ever extend a line
        if (!shortest_line.contains(in)) {
          shortest_line(in) = l1
        }
        if (!shortest_line.contains(out)) {
          shortest_line(out) = l2
        }

        l1.segment_intersection(l2) match {
          case Some(pt) => {
            val possible1 = new Line(l1.x1, l1.y1, pt.x, pt.y)
            val possible2 = new Line(pt.x, pt.y, l2.x2, l2.y2)

            // This line will intersect many -- store the one that gets
            // trimmed the most.
            if (!shortest_line.contains(in) || possible1.length < shortest_line(in).length) {
              shortest_line(in) = possible1
            }
            if (!shortest_line.contains(out) || possible2.length < shortest_line(out).length) {
              shortest_line(out) = possible2
            }
          }
          case _ =>
        }
      }
    }

    // Go back and mod the line to its shortest length.
    for ((e, best_line) <- shortest_line) {
      val l = if (e.from == v)
                e.lines.head
              else
                e.lines.last
      val use = best_line.perp_shift(-0.5)
      if (use.length <= cfg.epsilon && l.length > cfg.epsilon) {
        Util.log(s"Don't shorten line of $e to 0!")
      } else {
        /*l.x1 = use.x1
        l.y1 = use.y1
        l.x2 = use.x2
        l.y2 = use.y2*/
        // make sure length is correct when we finally re-enable this.
      }
    }
  }
}
