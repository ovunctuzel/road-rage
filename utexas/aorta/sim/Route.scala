// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import java.util.concurrent.Callable
import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn}
import utexas.aorta.{Util, cfg}

abstract class Route() {
  private var goal: DirectedRoad = null

  // This sets a new long-term plan, possibly delegating work to a thread pool.
  def reroute(from: DirectedRoad, to: DirectedRoad): Option[Callable[List[DirectedRoad]]]
  def request_route(from: DirectedRoad, to: DirectedRoad): Option[Callable[List[DirectedRoad]]] = {
    goal = to
    return reroute(from, to)
  }
  def request_route(from: Edge, to: Edge): Option[Callable[List[DirectedRoad]]]
    = request_route(from.directed_road, to.directed_road)

  // TODO make sure perf characteristics of list/stream are correct
  // Both of these include the agent's current step as the head.
  // When we're in a turn, general_path has the next road as head.
  var general_path: Stream[DirectedRoad] = Stream.empty
  var specific_path: Stream[Traversable] = Stream.empty

  def transition(from: Traversable, to: Traversable) = {
    (from, to) match {
      // When we lane-change, have to consider a new specific path
      case (_: Edge, _: Edge) => {
        // TODO pick_next_step will call high level replan() if needed.
        specific_path = to #:: pick_next_step(to)
      }
      // Make sure we're following general path
      case (e: Edge, t: Turn) => {
        assert(general_path.head == e.directed_road)
        general_path = general_path.tail
      }
      case _ =>
    }

    (from, to) match {
      case (_: Edge, _: Edge) =>
      case _ => {
        // If we didn't just reset the specific path, make sure we're following
        // it
        assert(specific_path.head == from)
        specific_path = specific_path.tail
      }
    }
  }

  def got_route(response: List[DirectedRoad]) = {
    general_path = response.toStream
  }

  def initialize(start: Traversable) = {
    if (specific_path.isEmpty) {
      specific_path = start #:: pick_next_step(start)
    }
  }

  // TODO problem: if a user strictly evaluates all of our steps, itll force us
  // to pick poorly
  def pick_next_step(current: Traversable): Stream[Traversable] = {
    current match {
      // We don't have a choice
      case t: Turn => { return t.to #:: pick_next_step(t.to) }
      // TODO this is kinda a*-specific
      case e: Edge => {
        // TODO this is quite inefficient
        // Get the next road after the current.
        val before_and_after = general_path.span(_ != e.directed_road)
        val desired_road = before_and_after._2.tail.headOption

        desired_road match {
          case Some(target_road) => {
            // Find a turn that leads to the desired road
            e.turns_leading_to(target_road) match {
              case turn :: _ => turn #:: pick_next_step(turn)
              case Nil => {
                // If it doesn't exist, that means we couldn't lane-change in
                // time, so blockingly re-route
                val new_src = pick_road_for_reroute(e)
                // TODO this is very A* specific. will generalize again soon.
                Util.log("Blockingly re-routing from " + new_src)
                general_path = (
                  before_and_after._1.toList ++ List(e.directed_road) ++
                  Agent.sim.pathfind_astar(new_src, goal)
                ).toStream
                // Call ourselves again. desired_road will not be blank again.
                return pick_next_step(current)
              }
            }
          }
          case None => {
            // Done with the route
            return Stream.empty
          }
        }
      }
    }
  }

  // TODO leave it up to the implementation
  def pick_road_for_reroute(e: Edge) = e.next_turns.head.to.directed_road
}

// A* and don't adjust the route unless we miss part of the sequence due to
// inability to lanechange.
class StaticRoute() extends Route() {
  override def reroute(from: DirectedRoad, to: DirectedRoad) = Some(
    new Callable[List[DirectedRoad]]() {
      def call = Agent.sim.pathfind_astar(from, to)
    }
  )
}

// TODO for the moment just redesign things for StaticRoute. maybe drunken
// doesnt even care about an overall plan, so itd never recommend lane changing.
// desired lane should maybe be a route thing?

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
  override def reroute(from: DirectedRoad, to: DirectedRoad): Option[Callable[List[DirectedRoad]]] = {
    goal = to
    general_path = plan_step(from)
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
