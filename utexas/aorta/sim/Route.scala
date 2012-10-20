// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import java.util.concurrent.Callable
import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn}
import utexas.aorta.{Util, cfg}

// Get a client to their goal by any means possible.
abstract class Route(val goal: DirectedRoad) {
  def done(at: Edge) = at.directed_road == goal

  // Can return work to delegate to a thread pool that MUST be done before the
  // client asks us questions.
  def compute_route(): Option[Callable[Unit]]
  // The client tells us they've physically moved
  def transition(from: Traversable, to: Traversable)
  // The client is being forced to pick a turn. If they ask us repeatedly, we
  // have to always return the same answer.
  def pick_turn(e: Edge): Turn
  // The client may try to lane-change somewhere
  def pick_lane(e: Edge): Edge
}

// Compute the cost of the path from every source to our single goal. 
// TODO does this wind up being too expensive memory-wise? maybe approx as ints
// or something... even a compressed data structure that's a bit slower to read
// from.
class StaticRoute(goal: DirectedRoad) extends Route(goal) {
  var costs: Array[Double] = null

  def compute_route = Some(new Callable[Unit]() {
    def call = {
      costs = Agent.sim.shortest_paths(goal)
    }
  })

  // We don't care.
  def transition(from: Traversable, to: Traversable) = {}

  def pick_turn(e: Edge) = e.next_turns.sortBy(t => costs(t.to.id)).head
  def pick_lane(from: Edge): Edge = {
    // Break ties for the best lane overall by picking the lane closest to the
    // current.
    val target_lane = from.other_lanes.sortBy(
      e => (costs(e.id), math.abs(from.lane_num - e.lane_num))
    ).head
    // Get as close to target_lane as possible.
    return from.adjacent_lanes.sortBy(
      e => math.abs(target_lane.lane_num - e.lane_num)
    ).head
  }
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute(goal: DirectedRoad) extends Route(goal) {
  // Remember answers we've given for the sake of consistency
  var desired_lane: Option[Edge] = None
  val chosen_turns = new MutableMap[Edge, Turn]()

  def compute_route = None

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

  def pick_turn(e: Edge) =
    chosen_turns.getOrElseUpdate(e, Util.choose_rand(e.next_turns))

  def pick_lane(e: Edge): Edge = {
    if (!desired_lane.isDefined) {
      if (e.queue.ok_to_lanechange) {
        desired_lane = Some(Util.choose_rand(e.other_lanes))
      } else {
        desired_lane = Some(e)
      }
    }
    return e.best_adj_lane(desired_lane.get)
  }
}

// TODO do these too
/*
// Wanders around slightly less aimlessly by picking directions
class DirectionalDrunkRoute extends DrunkenRoute() { 
  def heuristic(r: DirectedRoad) = r.end_pt.dist_to(goal.start_pt)

  // pick the most direct path 75% of the time
  override def pick_next_road(r: DirectedRoad): DirectedRoad = {
    return if (Util.percent(.75))
             r.leads_to.toList.sortBy(heuristic).head
           else
             super.pick_next_road(r)
  }
}

// Don't keep picking the same turn
class DrunkenExplorerRoute extends DirectionalDrunkRoute() {
  var past = new MutableMap[DirectedRoad, Int]()
  override def pick_next_road(r: DirectedRoad): DirectedRoad = {
    // break ties by the heuristic of distance to goal
    val choice = r.leads_to.toList.sortBy(
      next => (past.getOrElse(next, -1) + Util.rand_int(0, 5), heuristic(next))
    ).head
    past(choice) = past.getOrElse(choice, 0) + 1
    return choice
  }
}
*/
