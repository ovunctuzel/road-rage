// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.{Util, Price}
import utexas.aorta.common.algorithms.{AStar, Pathfind}

case class PathResult(path: List[Road], costs: Map[Road, (Double, Double)])

abstract class Router(graph: Graph) {
  protected var debug_me = false

  def router_type: RouterType.Value
  // Includes 'from' as the first step
  def path(spec: Pathfind[Road]): PathResult
  def path(from: Road, to: Road): PathResult = path(Pathfind(start = from, goals = Set(to)))

  // TODO messy to include this jump, but hard to pipe in specific params...
  def setup(a: Agent) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }
}

class FixedRouter(graph: Graph, initial_path: List[Road]) extends Router(graph) {
  override def router_type = RouterType.Fixed
  override def path(spec: Pathfind[Road]): PathResult = {
    Util.assert_eq(spec.start, initial_path.head)
    Util.assert_eq(spec.goals.contains(initial_path.last), true)
    return PathResult(initial_path, Map())
  }
}

// Score is a pair of doubles
abstract class AbstractPairAstarRouter(graph: Graph) extends Router(graph) {
  override def path(spec: Pathfind[Road]) = AStar.path(transform(spec)) match {
    case (route, costs) => PathResult(route, costs)
  }

  protected def transform(spec: Pathfind[Road]) = spec.copy(
    successors = (step: Road) => step.succs
  )
}

// No guess for cost, straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractPairAstarRouter {
  private val max_speed = 80.0  // TODO put in cfg
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_heuristic = (state: Road) => (0.0, state.end_pt.dist_to(spec.goals.head.end_pt) / max_speed)
  )
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy. (CH, Dijkstra table of distances)
}

class FreeflowRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Unusable
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) => (0, next.freeflow_time)
  )
}

// Cost for each step is (dollars, time)
trait TollAndTimeCost extends AbstractPairAstarRouter {
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (next.road_agent.toll.dollars, next.freeflow_time)
  )
}

// Score is (number of congested roads, total freeflow time)
class CongestionRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Congestion
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (Util.bool2binary(next.road_agent.congested), next.freeflow_time)
  )
}

// Score is (max congestion toll, total freeflow time)
class DumbTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.DumbToll
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    add_cost = (a: (Double, Double), b: (Double, Double)) => (math.max(a._1, b._1), a._2 + b._2)
  )
}

// Score is (number of toll violations, total freeflow time)
// We have a max_toll we're willing to pay, so we try to never pass through a road with that toll
class TollThresholdRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic
{
  private var max_toll: Price = new Price(-1)

  override def setup(a: Agent) {
    max_toll = new Price(a.wallet.priority)
  }

  override def router_type = RouterType.TollThreshold
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (Util.bool2binary(next.road_agent.toll.dollars > max_toll.dollars), next.freeflow_time)
  )
}

// Score is (sum of tolls, total freeflow time). The answer is used as the "free" baseline with the
// least cost to others.
class SumTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.SumToll
  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) =>
      (next.road_agent.toll.dollars, next.freeflow_time)
  )
}

// Score is (price + time * impatience_penalty, time). The time component lets us request quotes
// properly from tollbooths.
class TollboothRouter(graph: Graph) extends AbstractPairAstarRouter(graph) {
  private var owner: Agent = null
  override def router_type = RouterType.Tollbooth
  override def setup(a: Agent) {
    owner = a
  }

  override def transform(spec: Pathfind[Road]) = super.transform(spec).copy(
    // TODO usually ignore prev, but here, have to know pairs to moves, so grab toll for prev->next
    // TODO does this make eta offset strangely?
    calc_cost = (prev: Road, next: Road, cost_sofar: (Double, Double)) => (
      (prev.road_agent.tollbooth.toll_with_discount(cost_sofar._2, owner).dollars +
       prev.to.intersection.tollbooth.toll_with_discount(
         cost_sofar._2 + prev.freeflow_time, owner, prev, next
       ).dollars) + (prev.freeflow_time * owner.wallet.priority),
      prev.freeflow_time
    ),
    // Seed with the actual time we're starting
    cost_start = (0, owner.sim.tick)
  )
}
