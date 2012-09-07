// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import java.util.concurrent.Callable
import utexas.aorta.map.{Edge, Turn, Traversable}
import utexas.aorta.{Util, cfg}

abstract class Route() {
  // Define these
  def request_route(from: Edge, to: Edge): Option[Callable[List[Traversable]]]

  // Common to most, but can change. Should never become empty after got_route.
  // TODO make sure perf characteristics are good
  var steps: Stream[Traversable] = Stream.empty

  def transition(from: Traversable, to: Traversable) = {
    if (steps.head == to) {
      steps = steps.tail      // moving right along
    } else {
      throw new Exception("We missed a move!")
    }
  }

  def got_route(response: List[Traversable]) = {
    steps = response.toStream
  }

  // TODO re-evaluate the uses of this and if this'd ever break
  def next_step = steps.headOption
}

// It's all there, all at once... just look at it
class StaticRoute() extends Route() {
  override def request_route(from: Edge, to: Edge) = Some(new Callable[List[Traversable]]() {
    def call(): List[Traversable] = {
      return Agent.sim.pathfind_astar(from, to)
    }
  })
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute() extends Route() {
  var goal: Edge = null

  private def plan_step(last_step: Traversable): Stream[Traversable] = last_step match {
    case _ if goal == last_step => Stream.empty
    case e: Edge => {
      val turn = pick_turn(e)
      turn #:: plan_step(turn)
    }
    case t: Turn => t.to #:: plan_step(t.to)
  }

  // No actual work to do
  override def request_route(from: Edge, to: Edge): Option[Callable[List[Traversable]]] = {
    goal = to
    steps = plan_step(from)
    return None
  }

  // TODO choose a random point, or one leading in general direction of goal?
  def pick_turn(e: Edge) = Util.choose_rand[Turn](e.next_turns)
}

// Wanders around slightly less aimlessly by picking directions
class DirectionalDrunkRoute extends DrunkenRoute() { 
  def heuristic(t: Turn) = t.to.to.location.dist_to(goal.to.location)

  // pick the most direct path 75% of the time
  override def pick_turn(e: Edge): Turn = {
    return if (Util.percent(.75))
             e.next_turns.sortBy(heuristic).head
           else
             super.pick_turn(e)
  }
}

// Don't keep picking the same turn
class DrunkenExplorerRoute extends DirectionalDrunkRoute() {
  var past = new MutableMap[Edge, Int]()
  override def pick_turn(e: Edge): Turn = {
    // break ties by the heuristic of distance to goal
    val choice = e.next_turns.sortBy(
      t => (past.getOrElse(t.to, -1) + Util.rand_int(0, 5), heuristic(t))
    ).head
    past(choice.to) = past.getOrElse(choice.to, 0) + 1
    return choice
  }
}
