// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn}
import utexas.aorta.map.analysis.Waypoint

import utexas.aorta.{Util, RNG, cfg}

// Get a client to their goal by any means possible.
abstract class Route(val goal: DirectedRoad, rng: RNG) {
  def done(at: Edge) = at.directed_road == goal

  // The client tells us they've physically moved
  def transition(from: Traversable, to: Traversable)
  // The client is being forced to pick a turn. If they ask us repeatedly, we
  // have to always return the same answer.
  def pick_turn(e: Edge): Turn
  // The client may try to lane-change somewhere
  def pick_lane(e: Edge): Edge
  // Debug
  def dump_info
}

// Compute the cost of the path from every source to our single goal. 
// TODO does this wind up being too expensive memory-wise? maybe approx as ints
// or something... even a compressed data structure that's a bit slower to read
// from.
class DijkstraRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  // TODO We used to assign a cost to every edge, now we go by directed road.
  val costs = goal.costs_to

  // We don't care.
  def transition(from: Traversable, to: Traversable) = {}

  def pick_turn(e: Edge) = e.next_turns.minBy(t => costs(t.to.directed_road.id))
  def pick_lane(from: Edge): Edge = {
    // Break ties for the best lane overall by picking the lane closest to the
    // current.
    val target_lane = from.other_lanes.minBy(
      e => (costs(pick_turn(e).to.directed_road.id), math.abs(from.lane_num - e.lane_num))
    )
    // Get as close to target_lane as possible.
    return from.adjacent_lanes.minBy(
      e => math.abs(target_lane.lane_num - e.lane_num)
    )
  }

  def dump_info() = {
    Util.log(s"Static route to $goal")
  }
}

class WaypointRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  // We don't know where we are until the first time a client pokes us
  var waypt: Waypoint = null
  // We can navigate to the waypoint by little O(1) lookups at each step, but
  // once we reach the waypoint, we need to grab a path and follow it. Head of
  // path is always the next step.
  // TODO it'd almost be useful to polymorph this object as we go
  var path: List[DirectedRoad] = Nil

  def transition(from: Traversable, to: Traversable) = {
    to match {
      case e: Edge if path.nonEmpty => {
        // TODO if we dont manage, this is actully fragile... would have to return
        // to waypt!
        if (path.head == e.directed_road) {
          path = path.tail
        } else {
          throw new Exception(s"Not following route from $waypt well... wound up at $e instead of ${path.head}")
        }
      }
      case _ =>
    }
  }

  def pick_turn(e: Edge) =
    if (path.isEmpty)
      e.next_turns.minBy(t => waypt.costs_to_waypt(t.to.directed_road.id))
    else
      // TODO assume it exists
      e.next_turns.find(t => path.head == t.to.directed_road).get

  def pick_lane(from: Edge): Edge = {
    if (waypt == null) {
      waypt = Agent.sim.router.best_waypt(from.directed_road, goal)
    }

    // Have we reached the waypoint?
    if (from.directed_road == waypt.base) {
      path = waypt.path_from_waypt(goal).tail
    }

    // Break ties for the best lane overall by picking the lane closest to the
    // current.
    val target_lane =
      if (path.isEmpty)
        from.other_lanes.minBy(
          e => (waypt.costs_to_waypt(pick_turn(e).to.directed_road.id), math.abs(from.lane_num - e.lane_num))
        )
      else
        from.other_lanes.filter(e => e.next_roads.contains(path.head)).minBy(
          e => math.abs(from.lane_num - e.lane_num)
        )
    // Get as close to target_lane as possible.
    return from.adjacent_lanes.minBy(
      e => math.abs(target_lane.lane_num - e.lane_num)
    )
  }

  def dump_info() = {
    Util.log(s"Static route to $goal using $waypt as a waypoint")
  }
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  // Remember answers we've given for the sake of consistency
  var desired_lane: Option[Edge] = None
  val chosen_turns = new MutableMap[Edge, Turn]()

  // Forget what we've remembered
  def transition(from: Traversable, to: Traversable) = {
    (from, to) match {
      case (t: Turn, e: Edge) => {
        desired_lane = None
      }
      case (e: Edge, t: Turn) => {
        chosen_turns.remove(e)
      }
      case _ =>
    }
  }

  // With the right amount of alcohol, a drunk can choose uniformly at random
  def choose_turn(e: Edge) = rng.choose_rand(e.next_turns)

  def pick_turn(e: Edge) =
    chosen_turns.getOrElseUpdate(e, choose_turn(e))

  def pick_lane(e: Edge): Edge = {
    if (!desired_lane.isDefined) {
      if (e.ok_to_lanechange) {
        desired_lane = Some(rng.choose_rand(e.other_lanes))
      } else {
        desired_lane = Some(e)
      }
    }
    return e.best_adj_lane(desired_lane.get)
  }

  def dump_info() = {
    Util.log(s"Drunken route to $goal")
    Util.log(s"  Desired lane: $desired_lane")
    Util.log(s"  Chosen turns: $chosen_turns")
  }
}

// Wanders around slightly less aimlessly by picking directions
class DirectionalDrunkRoute(goal: DirectedRoad, rng: RNG)
  extends DrunkenRoute(goal, rng)
{
  def heuristic(t: Turn) = t.to.to.location.dist_to(goal.start_pt)

  // pick the most direct path 75% of the time
  override def choose_turn(e: Edge) =
    if (rng.percent(.75))
      e.next_turns.minBy(heuristic)
    else
      super.choose_turn(e)
}

// Don't keep making the same choices for roads
class DrunkenExplorerRoute(goal: DirectedRoad, rng: RNG)
  extends DirectionalDrunkRoute(goal, rng)
{
  val past = new MutableMap[DirectedRoad, Int]()

  override def choose_turn(e: Edge): Turn = {
    // break ties by the heuristic of distance to goal
    val choice = e.next_turns.minBy(turn => (
      past.getOrElse(turn.to.directed_road, -1) + rng.rand_int(0, 5),
      heuristic(turn)
    ))
    val road = choice.to.directed_road
    past(road) = past.getOrElse(road, 0) + 1
    return choice
  }
}
