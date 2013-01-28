// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import utexas.aorta.ui.Renderable

import utexas.aorta.{cfg, Util}

// TODO subclass Edge for pos/neg.. seems easier for lots of things

// TODO var id due to tarjan
class Edge(var id: Int, val road: Road, val dir: Direction.Direction)
  extends Traversable with Renderable with Ordered[Edge]
{
  var lane_num: Int = -1  // TODO needs to be initialized to be defined.. bleh.

  override def compare(other: Edge) = id.compare(other.id)

  // no lane-changing
  //def leads_to = next_turns
  // with lane-changing
  def leads_to = next_turns ++ List(shift_left, shift_right).flatten
  def speed_limit = road.speed_limit

  def directed_road = if (dir == Direction.POS)
                        road.pos_group
                      else
                        road.neg_group
  def turns_leading_to(group: DirectedRoad) =
    next_turns.filter(t => t.to.directed_road == group)

  def other_lanes = if (dir == Direction.POS) road.pos_lanes else road.neg_lanes
  def other_vert(v: Vertex) = road.other_vert(v)
  def opposite_lanes = if (dir == Direction.POS) road.neg_lanes else road.pos_lanes
  def rightmost_lane = other_lanes.head
  def leftmost_lane  = other_lanes.last

  def shift_left: Option[Edge]  = if (is_leftmost)  None else Some(other_lanes(lane_num + 1))
  def shift_right: Option[Edge] = if (is_rightmost) None else Some(other_lanes(lane_num - 1))
  def adjacent_lanes: List[Edge] = List(shift_left, shift_right, Some(this)).flatten
  def best_adj_lane(to_reach: Edge)
    = adjacent_lanes.sortBy(e => math.abs(to_reach.lane_num - e.lane_num)).head

  def next_turns = to.turns_from(this)
  def prev_turns = from.turns_to(this)
  def succs: List[Edge] = next_turns.map(t => t.to)
  def preds: List[Edge] = prev_turns.map(t => t.from)

  def is_rightmost = lane_num == 0
  def is_leftmost  = lane_num == other_lanes.size - 1

  // not for one-ways right now. but TODO it'd be cool to put that here.
  def lane_offset = other_lanes.length - lane_num

  override def toString = "Lane %s%d of %s (%d)".format(dir, lane_num, road.name, id)
  //override def toString = "Lane %d".format(id)

  def from: Vertex = if (dir == Direction.POS) road.v1 else road.v2
  def to: Vertex   = if (dir == Direction.POS) road.v2 else road.v1

  def to_xml(out: FileWriter) = {
    out.write(
      "  <edge id=\"" + id + "\" road=\"" + road.id + "\" dir=\"" + dir
      + "\" laneNum=\"" + lane_num + "\" length=\"" + length + "\">\n"
    )
    lines.foreach(l => l.to_xml(out))
    out.write("  </edge>\n")
  }

  def to_plaintext(out: FileWriter) = {
    out.write(
      id + "," + road.id + "," + dir + "," + lane_num + "," + length + ":"
    )
    lines.foreach(l => l.to_plaintext(out))
    out.write("\n")
  }

  //////// Geometry. TODO separate somewhere?

  // recall + means v1->v2, and that road's points are stored in that order
  // what's the first line segment we traverse following this lane?
  def first_road_line = if (dir == Direction.POS)
                          new Line(road.points.head, road.points.tail.head) // 0 -> 1
                        else
                          new Line(road.points.last, road.points.dropRight(1).last) // -1 -> -2
  // what's the last line segment we traverse following this lane?
  def last_road_line = if (dir == Direction.POS)
                         new Line(road.points.dropRight(1).last, road.points.last) // -2 -> -1
                       else
                         new Line(road.points.tail.head, road.points.head) // 1 -> 0

  def debug = {
    Util.log(this + " has length " + length + " m, min entry dist " +
             (queue.worst_entry_dist + cfg.follow_dist))
    Util.log("(lanechange dist is " + (cfg.lanechange_dist +
             cfg.end_threshold) + ")")
    Util.log("Queue contains " + queue.agents)
    Util.log("Speed lim " + speed_limit)
    Util.log("Succs: " + next_turns)
    Util.log("Preds: " + prev_turns)
  }

  // For debug only
  def doomed = next_turns.isEmpty || prev_turns.isEmpty
}

object Direction extends Enumeration {
  type Direction = Value
  val POS = Value("+")  // v1 -> v2
  val NEG = Value("-")  // v2 -> v1
}

// Represent a group of directed edges on one road
class DirectedRoad(val road: Road, val dir: Direction.Direction) {
  override def toString = "%s's %s lanes".format(road, dir)

  def edges = if (dir == Direction.POS)
                road.pos_lanes
              else
                road.neg_lanes

  def start_pt = edges.head.from.location
  def end_pt = edges.head.to.location
  // TODO dont assume some edge being lane-changeable means others are too
  // TODO could even predict/take into account the current distance to see if
  // there's room left
  def naive_leads_to = edges.flatMap(_.succs).map(_.directed_road).toSet
  def leads_to(from: Edge) = if (from.queue.ok_to_lanechange)
                               naive_leads_to
                             else
                               from.succs.map(_.directed_road).toSet

  def length = road.length
}
