// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable

import utexas.aorta.map.{Graph, DirectedRoad, Coordinate}
import utexas.aorta.sim.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.{Util, Common, Physics, RNG, DirectedRoadID, Price}
import utexas.aorta.common.algorithms.AStar

abstract class Router(graph: Graph) {
  def router_type: RouterType.Value
  // Doesn't include 'from' as the first step
  def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad]

  // TODO messy to include this jump, but hard to pipe in specific params...
  def setup(a: Agent) {}
}

class FixedRouter(graph: Graph, path: List[DirectedRoad]) extends Router(graph) {
  override def router_type = RouterType.Fixed
  override def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad] = {
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

class DijkstraRouter(graph: Graph) extends Router(graph) {
  override def router_type = RouterType.Unusable

  def costs_to(r: DirectedRoad) = dijkstras(graph.directed_roads.size, r)

  override def path(from: DirectedRoad, to: DirectedRoad, time: Double) =
    hillclimb(costs_to(to), from).tail

  // Precomputes a table of the cost from source to everything.
  private def dijkstras(size: Int, source: DirectedRoad): Array[Double] = {
    val costs = Array.fill[Double](size)(Double.PositiveInfinity)

    // TODO needs tests!
    // TODO pass in a comparator to the queue instead of having a wrapper class
    class Step(val dr: DirectedRoad) extends Ordered[Step] {
      def cost = costs(dr.id.int)
      def compare(other: Step) = other.cost.compare(cost)
    }

    // Roads in the open set don't have their final cost yet
    val open = new mutable.PriorityQueue[Step]()
    val done = new mutable.HashSet[DirectedRoad]()

    costs(source.id.int) = 0
    open.enqueue(new Step(source))

    while (open.nonEmpty) {
      val step = open.dequeue
      
      // Skip duplicate steps, since we chose option 3 for the problem below.
      if (!done.contains(step.dr)) {
        done += step.dr

        for (next <- step.dr.preds if !done.contains(next)) {
          val cost = step.cost + next.freeflow_time
          if (cost < costs(next.id.int)) {
            // Relax!
            costs(next.id.int) = cost
            // TODO ideally, decrease-key
            // 1) get a PQ that uses a fibonacci heap
            // 2) remove, re-insert again
            // 3) just insert a dupe, then skip the duplicates when we get to them
            // Opting for 3, for now.
            open.enqueue(new Step(next))
          }
        }
      }
    }
    return costs
  }

  // Starts at source, hillclimbs to lower costs, and returns the path to 0.
  private def hillclimb(costs: Array[Double], start: DirectedRoad): List[DirectedRoad] =
    costs(start.id.int) match {
      case 0 => start :: Nil
      case c => start :: hillclimb(
        costs, start.succs.minBy(step => costs(step.id.int))
      )
    }
}

// Score is a pair of doubles
// TODO dont operate on graph particularly, do anything with successor fxn and cost fxn...
abstract class AbstractPairAstarRouter(graph: Graph) extends Router(graph) {
  def calc_heuristic(state: DirectedRoad, goal: DirectedRoad): (Double, Double)
  def cost_step(
    prev: DirectedRoad, next: DirectedRoad, cost_sofar: (Double, Double)
  ): (Double, Double)

  protected def add_cost(a: (Double, Double), b: (Double, Double)) = (a._1 + b._1, a._2 + b._2)

  override def path(from: DirectedRoad, to: DirectedRoad, time: Double) = AStar.path(
    from, to, (step: DirectedRoad) => step.succs, cost_step, calc_heuristic, add_cost
  )
}

// No guess for cost, straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractPairAstarRouter {
  override def calc_heuristic(state: DirectedRoad, goal: DirectedRoad) =
    (0.0, state.end_pt.dist_to(goal.end_pt))  // TODO divided by some speed limit?
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy. (CH, Dijkstra table of distances)
}

// Cost for each step is (dollars, time)
trait TollAndTimeCost extends AbstractPairAstarRouter {
  override def cost_step(prev: DirectedRoad, next: DirectedRoad, cost_sofar: (Double, Double)) =
    (next.auditor.toll.dollars, next.freeflow_time)
}

// Score is (number of congested roads, total freeflow time)
class CongestionRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Congestion

  override def cost_step(prev: DirectedRoad, next: DirectedRoad, cost_sofar: (Double, Double)) =
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

  override def cost_step(prev: DirectedRoad, next: DirectedRoad, cost_sofar: (Double, Double)) =
    (Util.bool2binary(next.auditor.toll.dollars > max_toll.dollars), next.freeflow_time)
}

// Score is (sum of tolls, total freeflow time). The answer is used as the "free" baseline with the
// least cost to others.
class SumTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.SumToll

  override def cost_step(prev: DirectedRoad, next: DirectedRoad, cost_sofar: (Double, Double)) =
    (next.auditor.toll.dollars, next.freeflow_time)
}
