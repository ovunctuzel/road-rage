// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.aorta.map.{Coordinate, Vertex, Road, Edge, Direction, Turn,
                         GraphLike}

import utexas.aorta.common.{Util, cfg, Physics}

// TODO we should really subclass the real Graph, but not sure yet.

class PreGraph3(old_graph: PreGraph2) extends GraphLike {
  var vertices = new MutableList[Vertex]

  // create vertices lazily!
  private val vert_lookup = new HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new MutableList[Edge]           // a directed lane
  var roads: List[Road] = Nil

  for (old <- old_graph.edges) {
    add_road(old)
  }

  // TODO binary search or direct lookup possible?
  def get_r(id: Int): Road = {
    val r = roads(id)
    Util.assert_eq(r.id, id)
    return r
  }
  def get_v(id: Int): Vertex = {
    val v = vertices(id)
    Util.assert_eq(v.id, id)
    return v
  }
  def get_e(id: Int): Edge = {
    val e = edges(id)
    Util.assert_eq(e.id, id)
    return e
  }

  // support Tarjan's. Each of these expensive things should only be called once
  def turns() = vertices.foldLeft(List[Turn]())((l, v) => v.turns ++ l)
  def traversables() = edges ++ turns

  def add_road(old_edge: PreEdge2) {
    // do addEdges too, once we decide how many lanes to do
    val r = new Road(
      road_id_cnt, Road.road_len(old_edge.points),
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
      add_edge(r, Direction.POS, l)
    }

    // v2 -> v1 lanes unless we're oneway, in which case we keep the original
    // osm order
    if (!old_edge.dat.oneway) {
      for (l <- 0 until lanes) {
        add_edge(r, Direction.NEG, l)
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
      val v = new Vertex(at, vertices.length)
      vertices += v
      vert_lookup += ((at, v))
      v
    }

  private def add_edge(r: Road, dir: Direction.Value, lane_num: Int) = {
    val e = new Edge(edges.length, r.id, dir, lane_num)
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

  def remove_turns_to_bad_edges(bad_edges: Set[Edge]) = {
    for (v <- vertices) {
      v.turns = v.turns.filter(
        t => !bad_edges.contains(t.from) && !bad_edges.contains(t.to)
      )
    }
  }

  // After eating away at elements from edges/vertices/roads, cleanup references
  // to those deleted objects elsewhere.
  def fix_map() = {
    val good_edges = edges.toSet

    // TODO refactor this: squeeze together lane numbers
    // This will end up looking weird (gaps in roads)
    for (r <- roads) {                                              
      val pos_lanes = r.pos_lanes.filter(e => good_edges.contains(e))
      val neg_lanes = r.neg_lanes.filter(e => good_edges.contains(e))
      r.pos_lanes.clear
      r.pos_lanes ++= pos_lanes
      r.neg_lanes.clear
      r.neg_lanes ++= neg_lanes
      for ((lane, idx) <- r.pos_lanes.zipWithIndex) {
        lane.lane_num = idx
      }
      for ((lane, idx) <- r.neg_lanes.zipWithIndex) {
        lane.lane_num = idx
      }
    }

    // See what vertices now have no turns or lanes left
    vertices.partition(v => v.turns.isEmpty) match {
      case (bad, good) => {
        vertices = good
        val bad_set = bad.toSet
        roads = roads.filter(
          r => !bad_set.contains(r.v1) && !bad_set.contains(r.v2) && r.all_lanes.nonEmpty
        )
      }
    }

    fix_ids
  }

  // clean up ids of all edges, roads, verts.
  def fix_ids() = {
    // TODO a better way, and one without reassigning to 'val' id? if we just
    // clone each structure and have a mapping from old to new... worth it?
    for ((e, id) <- edges.zipWithIndex) {
      e.id = id
      e.next_turns.foreach(t => t.from_id = id)
      e.prev_turns.foreach(t => t.to_id = id)
    }
    for ((v, id) <- vertices.zipWithIndex) {
      v.id = id
    }
    // Get rid of directed roads with no lanes.
    var cnt = 0
    for ((r, id) <- roads.zipWithIndex) {
      r.id = id

      if (r.pos_group.isDefined && r.pos_group.get.edges.isEmpty) {
        r.pos_group = None
      }
      if (r.neg_group.isDefined && r.neg_group.get.edges.isEmpty) {
        r.neg_group = None
      }
      List(r.pos_group, r.neg_group).flatten.foreach(r => {
        r.id = cnt
        cnt += 1
      })
    }
    Road.num_directed_roads = cnt
  }
}
