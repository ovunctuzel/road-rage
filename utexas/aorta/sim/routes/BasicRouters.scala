// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.routes

import utexas.aorta.map.{AbstractPairAstarRouter, Road, Graph}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.{Util, Price}

// No guess for cost, straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractPairAstarRouter {
  override def heuristic_factory(goal: Road) =
    (state: Road) => (0.0, state.end_pt.dist_to(goal.end_pt))  // TODO divided by some speed limit?
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy. (CH, Dijkstra table of distances)
}

class FreeflowRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Unusable

  override def cost_step(prev: Road, next: Road, cost_sofar: (Double, Double)) =
    (next.freeflow_time, 0)
}

// Cost for each step is (dollars, time)
trait TollAndTimeCost extends AbstractPairAstarRouter {
  override def cost_step(prev: Road, next: Road, cost_sofar: (Double, Double)) =
    (next.auditor.toll.dollars, next.freeflow_time)
}

// Score is (number of congested roads, total freeflow time)
class CongestionRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Congestion

  override def cost_step(prev: Road, next: Road, cost_sofar: (Double, Double)) =
    (Util.bool2binary(next.auditor.congested), next.freeflow_time)
}

// Score is (max congestion toll, total freeflow time)
class DumbTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.DumbToll

  override def add_cost(a: (Double, Double), b: (Double, Double)) =
    (math.max(a._1, b._1), a._2 + b._2)
}

// Score is (number of toll violations, total freeflow time)
// We have a max_toll we're willing to pay, so we try to never pass through a road with that toll
class TollThresholdRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic
{
  private var max_toll: Price = new Price(-1)

  override def setup(a: Agent) {
    max_toll = new Price(a.wallet.priority)
  }

  override def router_type = RouterType.TollThreshold

  override def cost_step(prev: Road, next: Road, cost_sofar: (Double, Double)) =
    (Util.bool2binary(next.auditor.toll.dollars > max_toll.dollars), next.freeflow_time)
}

// Score is (sum of tolls, total freeflow time). The answer is used as the "free" baseline with the
// least cost to others.
class SumTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.SumToll

  override def cost_step(prev: Road, next: Road, cost_sofar: (Double, Double)) =
    (next.auditor.toll.dollars, next.freeflow_time)
}
