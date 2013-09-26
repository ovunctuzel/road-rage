// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.map.analysis.AbstractEdge
import utexas.aorta.ui.Renderable

import utexas.aorta.common.{cfg, RNG, Util, StateWriter, StateReader, RoadID, EdgeID, DirectedRoadID}

// TODO subclass Edge for pos/neg.. seems easier for lots of things

// TODO var id due to tarjan, var lane num due to fixing IDs. maybe not
// necessary...
class Edge(
  var id: EdgeID, road_id: RoadID, val dir: Direction.Value, var lane_num: Int
) extends Traversable with Renderable with Ordered[Edge]
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  var road: Road = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    w.int(id.int)
    w.int(road.id.int)
    w.int(dir.id)
    w.int(lane_num)
    super.serialize(w)
  }

  def setup(g: GraphLike) {
    road = g.get_r(road_id)
    other_lanes += this
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def compare(other: Edge) = id.int.compare(other.id.int)
  override def toString = "Lane %s%d of %s (%s)".format(dir, lane_num, road.name, id)

  // no lane-changing
  //def leads_to = next_turns
  // with lane-changing
  def leads_to = next_turns ++ List(shift_left, shift_right).flatten
  def speed_limit = road.speed_limit

  def directed_road = if (dir == Direction.POS)
                        road.pos_group.get
                      else
                        road.neg_group.get
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
    = adjacent_lanes.minBy(e => math.abs(to_reach.lane_num - e.lane_num))

  def next_turns = to.turns_from(this)
  def prev_turns = from.turns_to(this)
  def succs: List[Edge] = next_turns.map(t => t.to)
  def preds: List[Edge] = prev_turns.map(t => t.from)

  def next_roads = next_turns.map(t => t.to.directed_road)

  def is_rightmost = lane_num == 0
  def is_leftmost  = lane_num == other_lanes.size - 1

  // not for one-ways right now. but TODO it'd be cool to put that here.
  def lane_offset = other_lanes.length - lane_num

  def from: Vertex = if (dir == Direction.POS) road.v1 else road.v2
  def to: Vertex   = if (dir == Direction.POS) road.v2 else road.v1

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
             (worst_entry_dist + cfg.follow_dist))
    Util.log("(lanechange dist is " + (cfg.lanechange_dist +
             cfg.end_threshold) + ")")
    Util.log("Queue contains " + queue.agents)
    Util.log("Speed lim " + speed_limit)
    Util.log("Succs: " + next_turns)
    Util.log("Preds: " + prev_turns)
    Util.log(s"From $from to $to")
  }

  // For debug only
  def doomed = next_turns.isEmpty || prev_turns.isEmpty

  // TODO Starting on highways or in the middle lane seems weird, but allow it for now
  // TODO justify this better, or cite the paper.
  def ok_to_spawn = length >= worst_entry_dist + cfg.end_threshold + (2 * cfg.follow_dist)

  // TODO geometric argument
  // TODO sometimes the max arg is < the min arg. :)
  def safe_spawn_dist(rng: RNG) = rng.double(
    worst_entry_dist + cfg.follow_dist, length - cfg.end_threshold
  )

  def ok_to_lanechange = length >= cfg.lanechange_dist + cfg.end_threshold

  // If true, somebody's turning into this lane and already has a turn secured.
  def dont_block() = from.intersection.policy.approveds_to(this).find(
    t => t.a.wont_block(from.intersection)
  ).isDefined

  // Returns [0, 100] where 0 is light traffic and 100 is heavy.
  def congestion_rating(): Int = {
    // TODO a full road thats flowing is good... how to combine?
    //val avg_speed = queue.agents.map(_.speed).sum / queue.agents.size
    //avg_speed / e.speed_limit
    return queue.percent_full.toInt
  }
}

object Edge {
  def unserialize(r: StateReader): Edge = {
    val e = new Edge(new EdgeID(r.int), new RoadID(r.int), Direction(r.int), r.int)
    e.unserialize(r)
    return e
  }
}

object Direction extends Enumeration {
  type Direction = Value
  val POS = Value("+")  // v1 -> v2
  val NEG = Value("-")  // v2 -> v1
}

// Represent a group of directed edges on one road
// TODO var id because things get chopped up
class DirectedRoad(val road: Road, var id: DirectedRoadID, val dir: Direction.Value)
  extends AbstractEdge with Ordered[DirectedRoad]
{
  override def toString = "%s's %s lanes (DR %s)".format(road, dir, id)
  override def compare(other: DirectedRoad) = id.int.compare(other.id.int)

  def edges = if (dir == Direction.POS)
                road.pos_lanes
              else
                road.neg_lanes

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
  def cost(time: Double) = road.length / road.speed_limit

  def succs = edges.flatMap(e => e.next_turns.map(t => (t.to.directed_road, t.cost)))
  def preds = edges.flatMap(e => e.prev_turns.map(t => (t.from.directed_road, t.cost)))

  def next_roads = edges.flatMap(e => e.next_roads).toSet

  // We're congested if any of our lanes are.
  def is_congested = edges.find(e => e.queue.is_congested).isDefined
}
