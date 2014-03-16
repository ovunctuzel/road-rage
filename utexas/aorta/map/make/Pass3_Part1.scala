// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.{Coordinate, Vertex, Edge, Direction, Turn, Line, Road}

import utexas.aorta.common.{Util, cfg, Physics, VertexID, EdgeID, RoadID}

class PreGraph3(old_graph: PreGraph2) {
  Util.log("Multiplying and directing " + old_graph.edges.length + " edges")
  var vertices = new mutable.ArrayBuffer[Vertex]

  // create vertices lazily!
  private val vert_lookup = new mutable.HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new mutable.MutableList[Edge]
  var roads: List[Road] = Nil
  var turns = new mutable.ListBuffer[Turn]
  val other_side = new mutable.HashMap[Road, Road]()

  for (old <- old_graph.edges) {
    add_road(old)
  }

  // support Tarjan's. Each of these expensive things should only be called once
  def traversables() = edges ++ turns

  def add_road(old_edge: PreEdge2) {
    // Always make the positive direction
    val r_pos = new Road(
      new RoadID(road_id_cnt), Direction.POS, Road.road_len(old_edge.points),
      old_edge.dat.name, old_edge.dat.road_type, speed_limit(old_edge.dat.road_type),
      old_edge.dat.orig_id, get_vert(old_edge.from).id, get_vert(old_edge.to).id,
      old_edge.points.toArray, Array(), Array()
    )
    r_pos.setup(vertices.toArray)
    roads :+= r_pos
    road_id_cnt += 1

    // Make lanes
    val lanes = 
      if (ok_to_lc_multi_lanes(r_pos))
        how_many_lanes(old_edge.dat)
      else
        1
    for (l <- 0 until lanes) {
      add_edge(r_pos, l, lanes - l)
    }

    // Negative direction too
    if (!old_edge.dat.oneway) {
      val r_neg = new Road(
        new RoadID(road_id_cnt), Direction.NEG, Road.road_len(old_edge.points),
        old_edge.dat.name, old_edge.dat.road_type, speed_limit(old_edge.dat.road_type),
        old_edge.dat.orig_id, get_vert(old_edge.to).id, get_vert(old_edge.from).id,
        old_edge.points.reverse.toArray, Array(), Array()
      )
      r_neg.setup(vertices.toArray)
      roads :+= r_neg
      road_id_cnt += 1
      other_side(r_neg) = r_pos
      other_side(r_pos) = r_neg

      for (l <- 0 until lanes) {
        add_edge(r_neg, l, lanes - l)
      }
    }
  }

  private def ok_to_lc_multi_lanes(r: Road): Boolean = {
    // A driver should be able to enter at the speed limit and still have room
    // to finish the lane-change before the end.
    val min_len =
      cfg.lanechange_dist + cfg.end_threshold + Physics.worst_entry_dist(r.speed_limit)
    // TODO this length gets trimmed later when we clean up geometry. cant
    // really get that this early, so just conservatively over-estimate. it's
    // always safe to reduce things to 1 lane.
    return (r.length * .80) >= min_len
  }

  private def get_vert(at: Coordinate): Vertex =
    if (vert_lookup.contains(at)) {
      vert_lookup(at)
    } else {
      val v = new Vertex(at, new VertexID(vertices.length))
      vertices += v
      vert_lookup += ((at, v))
      v
    }

  private def add_edge(r: Road, lane_num: Int, lane_offset: Int) {
    // pre-compute lines constituting the edges
    // the -0.5 lets there be nice lane lines between lanes
    val lines = r.points.zip(r.points.tail).map(
      tupled((from, to) => Line(from, to).perp_shift(lane_offset - 0.5))
    )
    // force line segments to meet up on the inside
    /*for (e <- r.all_lanes; (l1, l2) <- e.lines.zip(e.lines.tail)) {
      l1.segment_intersection(l2) match {
        case Some(pt) => {
          l1.x2 = pt.x
          l1.y2 = pt.y
          l2.x1 = pt.x
          l2.y1 = pt.y
        }
        case _ =>
      }
    }*/
    val e = new Edge(new EdgeID(edges.length), r.id, lane_num, lines)
    e.setup(roads.toArray)  // TODO inefficient?
    edges += e
  }

  // osm types are inconsistent and wacky, but do our best
  private def how_many_lanes(dat: PreEdge1): Int = (dat.lanes, dat.road_type) match {
    // OSM actually told us!
    // TODO This number should be total, but I'm seeing examples of '3 lanes'...
    // how do we split those in each direction?
    case (Some(n), _)            => n
    case (None, "residential")   => 1
    // not all types of links between highways are one-laners, but most seem to
    // be.
    case (None, "motorway_link")  => 1
    case (None, "trunk_link")     => 1
    case (None, "primary_link")   => 1
    case (None, "secondary_link") => 1
    case (None, "tertiary_link")  => 1
    case (None, "service")        => 1   // I don't know what these are supposed to be
    case _                        => 2
  }

  private def speed_limit(road_type: String) = Physics.mph_to_si(road_type match {
    case "residential"    => 30
    case "motorway"       => 80
    // Actually these don't have a speed limit legally...  35 is suggested, but NOBODY does that
    case "motorway_link"  => 70
    case "trunk"          => 70
    case "trunk_link"     => 60
    case "primary"        => 65
    case "primary_link"   => 55
    case "secondary"      => 55
    case "secondary_link" => 45
    case "tertiary"       => 45
    case "tertiary_link"  => 35
    //case "unclassified"   => 40
    //case "road"           => 40
    case "living_street"  => 20
    // TODO some of these we filter out in Pass 1... cross-ref with that list
    case "service"        => 10 // This is apparently parking-lots basically, not feeder roads
    case "services"       => 10
    //case "track"          => 35
    // I feel the need.  The need for speed.  Where can we find one of these?
    //case "raceway"        => 300
    //case "null"           => 30
    //case "proposed"       => 35                                                               
    //case "construction"     => 20                                                             

    case _                => 35 // Generally a safe speed, right?
  })
}
