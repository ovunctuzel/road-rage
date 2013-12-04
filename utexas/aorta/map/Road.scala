// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{Util, StateReader, Physics, RoadID, VertexID, DirectedRoadID}

class Road(
  val id: RoadID, val length: Double, val name: String, val road_type: String,
  val osm_id: String, v1_id: VertexID, v2_id: VertexID, val points: Array[Coordinate]
) {
  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  var v1: Vertex = null
  var v2: Vertex = null

  // TODO move the lanes to be part of the D.R.
  var pos_group: Option[DirectedRoad] = Some(new DirectedRoad(
    this, new DirectedRoadID(Road.next_directed_id), Direction.POS,
    length, name, road_type, osm_id, v1_id, v2_id, points
  ))
  var neg_group: Option[DirectedRoad] = Some(new DirectedRoad(
    this, new DirectedRoadID(Road.next_directed_id), Direction.NEG,
    length, name, road_type, osm_id, v1_id, v2_id, points
  ))

  // + lanes go from v1->v2; - lanes go from v2->v1
  val pos_lanes = new mutable.MutableList[Edge]
  val neg_lanes = new mutable.MutableList[Edge]

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
  }

  def setup(g: GraphLike) {
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def all_lanes() = pos_lanes ++ neg_lanes
  def directed_roads = List(pos_group, neg_group).flatten

  def is_oneway = pos_lanes.length == 0 || neg_lanes.length == 0
  // TODO assert is_oneway, maybe. or maybe even case class...
  def oneway_lanes = if (pos_lanes.length == 0) neg_lanes else pos_lanes

  def num_lanes = pos_lanes.length + neg_lanes.length

  override def toString = name + " [R" + id + "]"
  
  // TODO don't ask for vertex that isn't either v1 or v2.
  def incoming_lanes(v: Vertex) = if (v == v1) neg_lanes else pos_lanes
  def outgoing_lanes(v: Vertex) = if (v == v1) pos_lanes else neg_lanes

  def pairs_of_points = points.zip(points.tail)
}

object Road {
  def unserialize(r: StateReader): Road = {
    return null
  }

  def road_len(pts: Iterable[Coordinate]) =
    pts.zip(pts.tail).map(tupled((p1, p2) => new Line(p1, p2).length)).sum

  private var num_directed_roads = 0
  def next_directed_id(): Int = {
    val id = num_directed_roads
    num_directed_roads += 1
    return id
  }
}
