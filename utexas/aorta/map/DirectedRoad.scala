// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import utexas.aorta.common.{DirectedRoadID, Price}

// Represent a group of directed edges on one road
// TODO var id because things get chopped up
class DirectedRoad(val road: Road, var id: DirectedRoadID, val dir: Direction.Value)
  extends Ordered[DirectedRoad]
{
  // TODO lets figure out how to build immutable stuff.
  val houses = new mutable.ListBuffer[Coordinate]()
  val shops = new mutable.ListBuffer[Coordinate]()

  override def toString = "%s's %s lanes (DR %s)".format(road, dir, id)
  override def compare(other: DirectedRoad) = id.int.compare(other.id.int)

  def edges = if (dir == Direction.POS)
                road.pos_lanes
              else
                road.neg_lanes
  def rightmost = edges.head

  def from: Vertex = edges.head.from
  def to: Vertex = edges.head.to

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

  def length = road.length
  def freeflow_time = road.length / road.speed_limit

  def succs = edges.flatMap(e => e.next_turns.map(t => t.to.directed_road))
  def preds = edges.flatMap(e => e.prev_turns.map(t => t.from.directed_road))

  def next_roads = edges.flatMap(e => e.next_roads).toSet

  // We're congested if any of our lanes are.
  def is_congested = edges.exists(e => e.queue.is_congested)

  // Worst-case of any constituent lanes
  def freeflow_capacity = edges.map(_.queue.freeflow_capacity).min
  def freeflow_percent_full = edges.map(_.queue.percent_freeflow_full).max

  // TODO decorate dir roads with something to add these

  // Free until 50% freeflow capacity, then $1 per % full. Should range from $0-$50 until
  // congestion.
  // Also, ignore roads with absurdly low capacity. Those're always free.
  def toll =
    if (freeflow_capacity >= 3)
      new Price(math.max(0, freeflow_percent_full - 50))
    else
      new Price(0)
}
