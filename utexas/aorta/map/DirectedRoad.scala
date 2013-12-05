// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.ui.Renderable
import utexas.aorta.sim.LinkAuditor
import utexas.aorta.common.{Util, DirectedRoadID, VertexID, Physics, StateReader}

// TODO cleanup everything here after the port to road-less...

// Represent a group of directed edges on one road
// TODO var id because things get chopped up
class DirectedRoad(
  var id: DirectedRoadID, val dir: Direction.Value, val length: Double, val name: String,
  val road_type: String, val osm_id: String, v1_id: VertexID, v2_id: VertexID,
  val points: Array[Coordinate]
) extends Ordered[DirectedRoad] with Renderable
{
  var v1: Vertex = null
  var v2: Vertex = null
  val lanes = new mutable.ListBuffer[Edge]()

  var other_side: Option[DirectedRoad] = None
  def incoming_lanes(v: Vertex) = if (v == v2) lanes else other_side.map(_.lanes).getOrElse(Nil)
  def outgoing_lanes(v: Vertex) = if (v == v1) lanes else other_side.map(_.lanes).getOrElse(Nil)

  // TODO lets figure out how to build immutable stuff.
  val houses = new mutable.ListBuffer[Coordinate]()
  val shops = new mutable.ListBuffer[Coordinate]()

  // TODO like queues for traversables and intersections for vertices... bad dependency to have.
  val auditor = new LinkAuditor(this)

  // TODO move this table. actually, store speed limit
  val speed_limit = Physics.mph_to_si(road_type match {
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


  def serialize(w: MapStateWriter) {
    w.int(id.int)
    w.int(dir.id)
    w.double(length)
    w.string(name)
    w.string(road_type)
    w.string(osm_id)
    w.int(w.vertices(v1.id).int)
    w.int(w.vertices(v2.id).int)
    w.int(points.size)
    points.foreach(pt => pt.serialize(w))
    w.int(shops.size)
    shops.foreach(pt => pt.serialize(w))
    w.int(houses.size)
    houses.foreach(pt => pt.serialize(w))
  }

  def setup(g: GraphLike) {
    v1 = g.get_v(v1_id)
    v2 = g.get_v(v2_id)

    // check invariants of points -- oops, not true anymore since we merge short
    // roads
    //Util.assert_eq(v1.location, points.head)
    //Util.assert_eq(v2.location, points.last)
  }

  override def toString = "%s's %s lanes (DR %s)".format(name, dir, id)
  override def compare(other: DirectedRoad) = id.int.compare(other.id.int)

  def edges = lanes
  def rightmost = edges.head

  // TODO just rename em?
  def from = v1
  def to = v2

  def start_pt = edges.head.from.location
  def end_pt = edges.head.to.location
  // TODO dont assume some edge being lane-changeable means others are too
  // TODO could even predict/take into account the current distance to see if
  // there's room left
  def naive_leads_to = edges.flatMap(_.succs).map(_.directed_road).toSet
  def leads_to(from: Edge) = if (from.ok_to_lanechange)
                               naive_leads_to
                             else
                               from.succs.map(_.directed_road).toSet

  def freeflow_time = length / speed_limit

  def succs = edges.flatMap(e => e.next_turns.map(t => t.to.directed_road))
  def preds = edges.flatMap(e => e.prev_turns.map(t => t.from.directed_road))

  def next_roads = edges.flatMap(e => e.next_roads).toSet

  /////////// TODO tmpish stuff that'll get moved for real from Road to here.
  def lines = dir match {
    case Direction.POS => points.zip(points.tail).map(p => shift_line(p, 1))
    case Direction.NEG => points.zip(points.tail).map(p => shift_line(p, -1))
  }

  // For debug only
  def doomed = edges.exists(e => e.doomed)

  def num_lanes = edges.size

  override def debug() {
    Util.log(this + " is a " + road_type + " of length " + length + " meters")
    Util.log(s"  Originally OSM id = $osm_id")
  }

  private def shift_line(pair: (Coordinate, Coordinate), side: Int) =
    new Line(pair._1, pair._2).perp_shift(side * num_lanes / 4.0)

  // TODO better heuristic, based on how much this extended road touches other
  // roads
  def is_major = road_type != "residential"
}

object DirectedRoad {
  def unserialize(r: StateReader): DirectedRoad = {
    val road = new DirectedRoad(
      new DirectedRoadID(r.int), Direction(r.int), r.double, r.string, r.string, r.string,
      new VertexID(r.int), new VertexID(r.int),
      Range(0, r.int).map(_ => Coordinate.unserialize(r)).toArray
    )
    Range(0, r.int).foreach(_ => road.shops += Coordinate.unserialize(r))
    Range(0, r.int).foreach(_ => road.houses += Coordinate.unserialize(r))
    return road
  }

  def road_len(pts: Iterable[Coordinate]) =
    pts.zip(pts.tail).map(tupled((p1, p2) => new Line(p1, p2).length)).sum
}
