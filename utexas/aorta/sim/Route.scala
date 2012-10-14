// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import java.util.concurrent.Callable
import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn}
import utexas.aorta.{Util, cfg}

abstract class Route() {
  protected var goal: DirectedRoad = null

  // This shouldn't modify any state of the route (eg, don't change goal here)
  // It can give a response immediately, or delegate to a thread pool
  def reroute(from: DirectedRoad, to: DirectedRoad): Either[Stream[DirectedRoad], Callable[Stream[DirectedRoad]]]

  // This sets the goal, calls reroute, and even handles the case when reroute
  // immediately answers.
  def request_route(from: DirectedRoad, to: DirectedRoad): Option[Callable[Stream[DirectedRoad]]] = {
    goal = to
    return reroute(from, to) match {
      case Left(route) => {
        got_route(route)
        None
      }
      case Right(thunk) => Some(thunk)
    }
  }

  // TODO make sure perf characteristics of list/stream are correct
  // Includes the agent's current step as the head.
  // When we're in a turn, general_path has the next road as head.
  var general_path: Stream[DirectedRoad] = Stream.empty

  def transition(from: Traversable, to: Traversable) = {
    (from, to) match {
      // Make sure we're following general path
      case (e: Edge, t: Turn) => {
        //Util.assert_eq(general_path.head, e.directed_road)
        assert(general_path.head == e.directed_road,
          { general_path.toList + " doesnt start with " + e.directed_road })
        general_path = general_path.tail
      }
      case _ =>
    }
  }

  def got_route(response: Stream[DirectedRoad]) = {
    general_path = response
  }

  // None indicates the route's done
  def pick_turn(e: Edge): Option[Turn] = {
    // Split our path into roads before and after e's road
    // TODO this is quite inefficient, specific_path should track us or so.
    return general_path.span(_ != e.directed_road) match {
      case (before, this_road #:: next_road #:: rest) => {
        // Find a turn that leads to the desired road
        e.turns_leading_to(next_road) match {
          // Pick the first arbitrarily if there are multiple
          case turn :: _ => Some(turn)
          // If it doesn't exist, that means we couldn't lane-change in
          // time, so blockingly re-route
          case Nil => {
            // TODO let the route choose this
            val new_src = e.next_turns.head.to.directed_road

            // TODO it's useful to know how often this is happening for debug
            Util.log("Blockingly re-routing from " + new_src)

            val new_route = reroute(new_src, goal) match {
              case Left(route) => route
              // Blockingly do the work right now
              case Right(thunk) => thunk.call
            }

            got_route(
              (before.toList ++ List(this_road)).toStream #::: new_route
            )

            // Call ourselves again. desired_road will not be blank again.
            pick_turn(e)
          }
        }
      }
      // Done with route
      case (before, this_road #:: Stream.Empty) => None
    }
  }
}

// A* and don't adjust the route unless we miss part of the sequence due to
// inability to lanechange.
class StaticRoute() extends Route() {
  override def reroute(from: DirectedRoad, to: DirectedRoad) = Right(
    new Callable[Stream[DirectedRoad]]() {
      def call = Agent.sim.pathfind_astar(from, to).toStream
    }
  )
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute() extends Route() {
  // No actual work to do
  override def reroute(from: DirectedRoad, to: DirectedRoad) = Left(plan_step(from))

  private def plan_step(cur_step: DirectedRoad): Stream[DirectedRoad] =
    if (cur_step == goal)
      cur_step #:: Stream.empty
    else {
      val next = pick_next_road(cur_step)
      cur_step #:: plan_step(next)
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
