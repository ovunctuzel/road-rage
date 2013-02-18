// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn}

import utexas.aorta.{Util, RNG, Common, cfg}

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
  def route_type(): RouteType.Value
}

// Compute the cost of the path from every source to our single goal, then
// hillclimb each step.
class DijkstraRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  // TODO We used to assign a cost to every edge, now we go by directed road.
  val costs = Common.sim.graph.dijkstra_router.costs_to(goal)

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
  def route_type = RouteType.Dijkstra
}

// Compute and follow a specific path to the goal. When we deviate from the path
// accidentally, recalculate the path.
class PathRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  // Head is the next step. If that step isn't immediately reachable, we have to
  // re-route.
  var path: List[DirectedRoad] = goal :: Nil

  def transition(from: Traversable, to: Traversable) = {
    to match {
      case e: Edge if e.directed_road == path.head => {
        path = path.tail
      }
      // Otherwise, we were just lane-changing, or we were forced to go
      // off-route. In the latter case, we'll recompute later.
      case _ =>
    }
  }

  private def best_turn(e: Edge) =
    e.next_turns.find(t => t.to.directed_road == path.head)

  def pick_turn(e: Edge) =
    // Is the next step reachable?
   best_turn(e) match {
      case Some(t) => t
      case None => {
        // Re-route! Shouldn't fail if graph is connected.
        path = Common.sim.graph.router.path(e.directed_road, goal)
        best_turn(e).get
      }
    }

  private def candidate_lanes(from: Edge) =
    from.other_lanes.filter(
      f => f.succs.find(t => t.directed_road == path.head).isDefined
    ).toList

  def pick_lane(from: Edge): Edge = {
    // Find any lane going to the next step. If none do, re-route and repeat.
    val candidates = candidate_lanes(from) match {
      case Nil => {
        path = Common.sim.graph.router.path(from.directed_road, goal)
        candidate_lanes(from)
      }
      case lanes => lanes
    }
    Util.assert_eq(candidates.isEmpty, false)

    // Pick the lane closest to the current, then get as close as possible.
    val target_lane = candidates.minBy(e => math.abs(from.lane_num - e.lane_num))
    // Get as close to target_lane as possible.
    return from.adjacent_lanes.minBy(
      e => math.abs(target_lane.lane_num - e.lane_num)
    )
  }

  def dump_info() = {
    Util.log(s"Static route to $goal using $path")
  }
  def route_type = RouteType.Path
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
  def choose_turn(e: Edge) = rng.choose(e.next_turns)

  def pick_turn(e: Edge) =
    chosen_turns.getOrElseUpdate(e, choose_turn(e))

  def pick_lane(e: Edge): Edge = {
    if (!desired_lane.isDefined) {
      if (e.ok_to_lanechange) {
        desired_lane = Some(rng.choose(e.other_lanes))
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
  def route_type = RouteType.Drunken
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

  override def route_type = RouteType.DirectionalDrunk
}

// Don't keep making the same choices for roads
class DrunkenExplorerRoute(goal: DirectedRoad, rng: RNG)
  extends DirectionalDrunkRoute(goal, rng)
{
  val past = new MutableMap[DirectedRoad, Int]()

  override def choose_turn(e: Edge): Turn = {
    // break ties by the heuristic of distance to goal
    val choice = e.next_turns.minBy(turn => (
      past.getOrElse(turn.to.directed_road, -1) + rng.int(0, 5),
      heuristic(turn)
    ))
    val road = choice.to.directed_road
    past(road) = past.getOrElse(road, 0) + 1
    return choice
  }

  override def route_type = RouteType.DrunkenExplorer
}
