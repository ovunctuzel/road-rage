// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.{Coordinate, Vertex, Edge, Direction, Turn, Line, GraphLike, DirectedRoad}

import utexas.aorta.common.{Util, cfg, Physics, VertexID, EdgeID, DirectedRoadID}

// TODO we should really subclass the real Graph, but not sure yet.
class PreGraph3(old_graph: PreGraph2) extends GraphLike {
  Util.log("Multiplying and directing " + old_graph.edges.length + " edges")
  var vertices = new mutable.MutableList[Vertex]

  // create vertices lazily!
  private val vert_lookup = new mutable.HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new mutable.MutableList[Edge]           // a directed lane
  var directed_roads: List[DirectedRoad] = Nil

  for (old <- old_graph.edges) {
    add_road(old)
  }

  override def get_v(id: VertexID): Vertex = {
    val v = vertices(id.int)
    Util.assert_eq(v.id, id)
    return v
  }
  override def get_e(id: EdgeID): Edge = {
    val e = edges(id.int)
    Util.assert_eq(e.id, id)
    return e
  }
  override def get_dr(id: DirectedRoadID): DirectedRoad = {
    val dr = directed_roads(id.int)
    Util.assert_eq(dr.id, id)
    return dr
  }

  // support Tarjan's. Each of these expensive things should only be called once
  def turns() = vertices.foldLeft(List[Turn]())((l, v) => v.turns ++ l)
  def traversables() = edges ++ turns

  def add_road(old_edge: PreEdge2) {
    // Always make the positive direction
    val dr_pos = new DirectedRoad(
      new DirectedRoadID(road_id_cnt), Direction.POS, DirectedRoad.road_len(old_edge.points),
      old_edge.dat.name, old_edge.dat.road_type, old_edge.dat.orig_id,
      get_vert(old_edge.from).id, get_vert(old_edge.to).id, old_edge.points.toArray
    )
    dr_pos.setup(this)
    directed_roads :+= dr_pos
    road_id_cnt += 1

    // Make lanes
    val lanes = 
      if (ok_to_lc_multi_lanes(dr_pos))
        how_many_lanes(old_edge.dat)
      else
        1
    for (l <- 0 until lanes) {
      add_edge(dr_pos, l, lanes - l)
    }

    // Negative direction too
    if (!old_edge.dat.oneway) {
      val dr_neg = new DirectedRoad(
        new DirectedRoadID(road_id_cnt), Direction.NEG, DirectedRoad.road_len(old_edge.points),
        old_edge.dat.name, old_edge.dat.road_type, old_edge.dat.orig_id,
        get_vert(old_edge.from).id, get_vert(old_edge.to).id, old_edge.points.toArray
      )
      dr_neg.setup(this)
      directed_roads :+= dr_neg
      road_id_cnt += 1
      dr_neg.other_side = Some(dr_pos)
      dr_pos.other_side = Some(dr_neg)

      for (l <- 0 until lanes) {
        add_edge(dr_neg, l, lanes - l)
      }
    }
  }

  private def ok_to_lc_multi_lanes(r: DirectedRoad): Boolean = {
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

  private def add_edge(dr: DirectedRoad, lane_num: Int, lane_offset: Int) {
    // pre-compute lines constituting the edges
    // the -0.5 lets there be nice lane lines between lanes
    val lines = dr.dir match {
      case Direction.POS => dr.points.zip(dr.points.tail).map(
        tupled((from, to) => new Line(from, to).perp_shift(lane_offset - 0.5))
      )
      case Direction.NEG => dr.points.zip(dr.points.tail).map(
        tupled((from, to) => new Line(to, from).perp_shift(lane_offset - 0.5))
      ).reverse
    }
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
    val e = new Edge(new EdgeID(edges.length), dr.id, lane_num, lines)
    e.setup(this)
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
}
