// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.MutableList
import java.io.Serializable

import utexas.aorta.ui.Renderable

import utexas.aorta.Util

// TODO enum for type. also, it's var because of tarjan's...
// TODO var id due to tarjan
@SerialVersionUID(1)
class Road(var id: Int, val length: Double, val name: String,
           var road_type: String, val osm_id: Int, val v1: Vertex,
           val v2: Vertex) extends Renderable with Serializable
{
  var points: Array[Coordinate] = null

  def set_points(pts: Array[Coordinate]) = {
    points = pts

    // check invariants
    Util.assert_eq(v1.location, points.head)
    Util.assert_eq(v2.location, points.last)
  }

  val pos_group = new DirectedRoad(this, Road.next_directed_id, Direction.POS)
  val neg_group = new DirectedRoad(this, Road.next_directed_id, Direction.NEG)

  // + lanes go from v1->v2; - lanes go from v2->v1
  val pos_lanes = new MutableList[Edge]
  val neg_lanes = new MutableList[Edge]

  def all_lanes() = pos_lanes ++ neg_lanes
  def other_vert(v: Vertex) = if (v == v1) v2 else v1

  def is_oneway = pos_lanes.length == 0 || neg_lanes.length == 0
  // TODO assert is_oneway, maybe. or maybe even case class...
  def oneway_lanes = if (pos_lanes.length == 0) neg_lanes else pos_lanes

  def num_lanes = pos_lanes.length + neg_lanes.length

  override def toString = name + " [R" + id + "]"
  
  // TODO don't ask for vertex that isn't either v1 or v2.
  def incoming_lanes(v: Vertex) = if (v == v1) neg_lanes else pos_lanes
  def outgoing_lanes(v: Vertex) = if (v == v1) pos_lanes else neg_lanes

  def pairs_of_points = points zip points.tail

  lazy val speed_limit = mph_to_si(road_type match {
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
    case "raceway"        => 300
    //case "null"           => 30
    //case "proposed"       => 35
    //case "construction"     => 20
    
    case _                => 35 // Generally a safe speed, right?
  })
  // to meters/sec
  def mph_to_si(r: Double) = r * 0.44704

  def debug = {
    Util.log(this + " is a " + road_type + " of length " + length + " meters")
  }

  // For debug only
  def doomed = all_lanes.find(e => e.doomed).isDefined

  // TODO better heuristic, based on how much this extended road touches other
  // roads
  def is_major = road_type != "residential"
}

object Road {
  def road_len(pts: Iterable[Coordinate]) =
    pts.zip(pts.tail).map(p => new Line(p._1, p._2)).foldLeft(0.0)(
      (a, b) => a + b.length
    )

  // PreGraph3's fix_ids also mods us.
  var num_directed_roads = 0
  def next_directed_id(): Int = {
    val id = num_directed_roads
    num_directed_roads += 1
    return id
  }
}
