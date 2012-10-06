// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import java.util.concurrent.Callable
import utexas.aorta.map.{Edge, DirectedRoad}
import utexas.aorta.{Util, cfg}

abstract class Route() {
  // Define these
  def request_route(from: DirectedRoad, to: DirectedRoad): Option[Callable[List[DirectedRoad]]]
  def request_route(from: Edge, to: Edge): Option[Callable[List[DirectedRoad]]]
    = request_route(from.directed_road, to.directed_road)

  // Common to most, but can change. Should never become empty after got_route.
  // TODO make sure perf characteristics are good
  var general_steps: Stream[DirectedRoad] = Stream.empty
  var specific_steps: Stream[Turn] = Stream.empty // or edge, turn pairs..
  // TODO idea: dont disconnectedly ask choose_turn to us, track where lookahead
  // is here, and someow interlave with general_steps to easily detect
  // disagreements.

  // We're only updated when the agent enters a new road
  def transition(from: DirectedRoad, to: DirectedRoad) = {
    if (steps.head == to) {
      steps = steps.tail      // moving right along
    } else {
      // TODO or the behavior carrying out this route couldnt lane-change and
      // had to detour...
      throw new Exception("We missed a move!")
    }
  }

  def got_route(response: List[DirectedRoad]) = {
    steps = response.toStream
  }

  // TODO re-evaluate the uses of this and if this'd ever break
  def next_step = steps.headOption
}

// It's all there, all at once... just look at it
class StaticRoute() extends Route() {
  override def request_route(from: DirectedRoad, to: DirectedRoad) = Some(
    new Callable[List[DirectedRoad]]() {
      def call = Agent.sim.pathfind_astar(from, to)
    }
  )
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute() extends Route() {
  var goal: DirectedRoad = null

  private def plan_step(last_step: DirectedRoad): Stream[DirectedRoad] =
    if (last_step == goal)
      Stream.empty
    else {
      val next = pick_next_road(last_step)
      next #:: plan_step(next)
    }

  // No actual work to do
  override def request_route(from: DirectedRoad, to: DirectedRoad): Option[Callable[List[DirectedRoad]]] = {
    goal = to
    steps = plan_step(from)
    return None
  }

  def pick_next_road(r: DirectedRoad): DirectedRoad = Util.choose_rand[DirectedRoad](r.leads_to.toList)
}

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
