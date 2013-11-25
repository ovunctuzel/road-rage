// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable

import utexas.aorta.map.{Coordinate, Vertex, Road, Edge, Direction, Turn, Line, GraphLike}

import utexas.aorta.common.{Util, cfg, Physics, RoadID, VertexID, EdgeID, DirectedRoadID}

// TODO we should really subclass the real Graph, but not sure yet.
class PreGraph3(old_graph: PreGraph2) extends GraphLike {
  Util.log("Multiplying and directing " + old_graph.edges.length + " edges")
  var vertices = new mutable.MutableList[Vertex]

  // create vertices lazily!
  private val vert_lookup = new mutable.HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new mutable.MutableList[Edge]           // a directed lane
  var roads: List[Road] = Nil

  for (old <- old_graph.edges) {
    add_road(old)
  }

  // TODO binary search or direct lookup possible?
  override def get_r(id: RoadID): Road = {
    val r = roads(id.int)
    Util.assert_eq(r.id, id)
    return r
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

  // support Tarjan's. Each of these expensive things should only be called once
  def turns() = vertices.foldLeft(List[Turn]())((l, v) => v.turns ++ l)
  def traversables() = edges ++ turns

  def add_road(old_edge: PreEdge2) {
    val r = new Road(
      new RoadID(road_id_cnt), Road.road_len(old_edge.points),
      old_edge.dat.name, old_edge.dat.road_type, old_edge.dat.orig_id,
      get_vert(old_edge.from).id, get_vert(old_edge.to).id,
      old_edge.points.toArray
    )
    r.setup(this)
    roads :+= r
    road_id_cnt += 1

    // now make the directed edges too
    val lanes = 
      if (ok_to_lc_multi_lanes(r))
        how_many_lanes(old_edge.dat)
      else
        1

    // v1 -> v2 lanes
    for (l <- 0 until lanes) {
      add_edge(r, Direction.POS, l, lanes - l)
    }

    // v2 -> v1 lanes unless we're oneway, in which case we keep the original
    // osm order
    if (!old_edge.dat.oneway) {
      for (l <- 0 until lanes) {
        add_edge(r, Direction.NEG, l, lanes - l)
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

  private def add_edge(r: Road, dir: Direction.Value, lane_num: Int, lane_offset: Int) {
    // pre-compute lines constituting the edges
    // the -0.5 lets there be nice lane lines between lanes
    val lines = dir match {
      case Direction.POS => r.pairs_of_points.map(
        pair => new Line(pair._1, pair._2).perp_shift(lane_offset - 0.5)
      )
      case Direction.NEG => r.pairs_of_points.map(
        pair => new Line(pair._2, pair._1).perp_shift(lane_offset - 0.5)
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

    val e = new Edge(new EdgeID(edges.length), r.id, dir, lane_num, lines)
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
