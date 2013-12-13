// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.Util
import utexas.aorta.common.algorithms.AStar

abstract class Router(graph: Graph) {
  protected var debug_me = false

  def router_type: RouterType.Value
  // Doesn't include 'from' as the first step
  def path(from: Road, to: Road, time: Double): List[Road]

  // TODO messy to include this jump, but hard to pipe in specific params...
  def setup(a: Agent) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }
}

class FixedRouter(graph: Graph, path: List[Road]) extends Router(graph) {
  override def router_type = RouterType.Fixed
  override def path(from: Road, to: Road, time: Double): List[Road] = {
    if (path.nonEmpty) {
      // remember, paths don't include from as the first step.
      Util.assert_eq(from.succs.contains(path.head), true)
      Util.assert_eq(to, path.last)
    } else {
      // This is the only time when empty paths should be acceptable
      Util.assert_eq(from, to)
    }
    return path
  }
}

// Score is a pair of doubles
abstract class AbstractPairAstarRouter(graph: Graph) extends Router(graph) {
  def heuristic_factory(goal: Road): (Road) => (Double, Double)
  def cost_step(
    prev: Road, next: Road, cost_sofar: (Double, Double)
  ): (Double, Double)

  protected def add_cost(a: (Double, Double), b: (Double, Double)) = (a._1 + b._1, a._2 + b._2)

  override def path(from: Road, to: Road, time: Double) = AStar.path(
    from, Set(to), (step: Road) => step.succs, cost_step, heuristic_factory(to), add_cost
  )
}
