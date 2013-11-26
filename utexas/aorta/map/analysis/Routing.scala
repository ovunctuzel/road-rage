// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable
import com.graphhopper.storage.{LevelGraphStorage, RAMDirectory}
import com.graphhopper.routing.ch.PrepareContractionHierarchies
import com.graphhopper.routing.DijkstraBidirectionRef

import utexas.aorta.map.{Graph, DirectedRoad, Coordinate}
import utexas.aorta.sim.Agent
import utexas.aorta.sim.make.RouterType

import utexas.aorta.common.{Util, Common, Physics, RNG, DirectedRoadID, Price}

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
    Util.assert_eq(from, path.head)
    Util.assert_eq(to, path.last)
    return path
  }
}

// A convenient abstraction if we ever switch to pathfinding on
// edges/roads/others.
abstract class AbstractEdge {
  // TODO dont tie it to this ID space...
  var id: DirectedRoadID // TODO var because fix_ids
  def cost(time: Double): Double
  // List and not Set means there could be multiple transitions, with possibly
  // different weights.
  def succs: Seq[(AbstractEdge, Double)]
  def preds: Seq[(AbstractEdge, Double)]
}

class DijkstraRouter(graph: Graph) extends Router(graph) {
  override def router_type = RouterType.TMP

  def costs_to(r: DirectedRoad, time: Double) = dijkstras(
    graph.directed_roads.size, r, (e: AbstractEdge) => e.preds, time
  )

  def path(from: DirectedRoad, to: DirectedRoad, time: Double) =
    hillclimb(costs_to(to, time), from).tail.asInstanceOf[List[DirectedRoad]]

  // Precomputes a table of the cost from source to everything.
  def dijkstras(size: Int, source: AbstractEdge,
                next: AbstractEdge => Seq[(AbstractEdge, Double)],
                time: Double): Array[Double] =
  {
    val costs = Array.fill[Double](size)(Double.PositiveInfinity)

    // TODO needs tests!
    // TODO pass in a comparator to the queue instead of having a wrapper class
    class Step(val edge: AbstractEdge) extends Ordered[Step] {
      def cost = costs(edge.id.int)
      def compare(other: Step) = other.cost.compare(cost)
    }

    // Edges in the open set don't have their final cost yet
    val open = new mutable.PriorityQueue[Step]()
    val done = new mutable.HashSet[AbstractEdge]()

    costs(source.id.int) = 0
    open.enqueue(new Step(source))

    while (open.nonEmpty) {
      val step = open.dequeue
      
      // Skip duplicate steps, since we chose option 3 for the problem below.
      if (!done.contains(step.edge)) {
        done += step.edge

        for ((next, transition_cost) <- next(step.edge) if !done.contains(next)) {
          val cost = step.cost + transition_cost + next.cost(time)
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
  def hillclimb(costs: Array[Double], start: AbstractEdge): List[AbstractEdge] =
    costs(start.id.int) match {
      case 0 => start :: Nil
      case c => start :: hillclimb(
        costs, start.succs.minBy(t => costs(t._1.id.int))._1
      )
    }
}

// One of these in memory per graph, please
object CHRouter {
  var gh: LevelGraphStorage = null
  var usable = false
  var algo: DijkstraBidirectionRef = null
}

class CHRouter(graph: Graph) extends Router(graph) {
  if (CHRouter.gh == null) {
    CHRouter.gh = new LevelGraphStorage(new RAMDirectory(s"maps/route_${graph.name}", true))
    CHRouter.usable = CHRouter.gh.loadExisting
    CHRouter.algo = new PrepareContractionHierarchies().graph(CHRouter.gh).createAlgo
  }

  override def router_type = RouterType.ContractionHierarchy

  def path(from: DirectedRoad, to: DirectedRoad, path: Double): List[DirectedRoad] = {
    // GraphHopper can't handle loops. For now, empty path; freebie.
    // TODO force a loop by starting on a road directly after 'from'
    if (from == to) {
      return Nil
    }

    if (Common.sim != null) {
      Common.sim.ch_since_last_time += 1
    }

    Util.assert_eq(CHRouter.usable, true)
    val path = CHRouter.algo.calcPath(from.id.int, to.id.int)
    CHRouter.algo.clear()
    Util.assert_eq(path.found, true)

    val result = new mutable.ListBuffer[DirectedRoad]()
    val iter = path.calcNodes.iterator
    while (iter.hasNext) {
      result += graph.directed_roads(iter.next)
    }
    return result.tail.toList
  }

  // Other analyses can use this as a pretty quick oracle to answer distance
  // queries. Useful for heuristics.
  def dist(from: DirectedRoad, to: DirectedRoad): Double = {
    if (from == to) {
      return 0
    }

    Common.sim.ch_since_last_time += 1

    Util.assert_eq(CHRouter.usable, true)
    val path = CHRouter.algo.calcPath(from.id.int, to.id.int)
    CHRouter.algo.clear()
    Util.assert_eq(path.found, true)
    return path.distance
  }
}

// Score is a pair of doubles
abstract class AbstractPairAstarRouter(graph: Graph) extends Router(graph) {
  def calc_heuristic(state: DirectedRoad, goal: DirectedRoad): (Double, Double)
  def cost_step(turn_cost: Double, state: DirectedRoad, time: Double): (Double, Double)

  protected def add_cost(a: (Double, Double), b: (Double, Double)) =
    (a._1 + b._1, a._2 + b._2)

  override def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad] = {
    if (from == to) {
      return Nil
    }

    // Stitch together our path
    val backrefs = new mutable.HashMap[DirectedRoad, DirectedRoad]()
    // We're finished with these
    val visited = new mutable.HashSet[DirectedRoad]()
    // Best cost so far
    val costs = new mutable.HashMap[DirectedRoad, (Double, Double)]()

    case class Step(state: DirectedRoad) {
      lazy val heuristic = calc_heuristic(state, to)
      def cost = add_cost(costs(state), heuristic)
    }
    val ordering = Ordering[(Double, Double)].on((step: Step) => step.cost).reverse
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    // Priority queue grabs highest priority first, so reverse to get lowest
    // cost first.
    val open = new mutable.PriorityQueue[Step]()(ordering)
    // Used to see if we've already added a road to the queue
    val open_members = new mutable.HashSet[DirectedRoad]()

    costs(from) = (0, 0)
    open.enqueue(Step(from))
    open_members += from
    backrefs(from) = null

    while (open.nonEmpty) {
      val current = open.dequeue()
      visited += current.state
      open_members -= current.state

      if (current.state == to) {
        // Reconstruct the path
        var path: List[DirectedRoad] = Nil
        var pointer: Option[DirectedRoad] = Some(current.state)
        while (pointer.isDefined && pointer.get != null) {
          path = pointer.get :: path
          // Clean as we go to break loops
          pointer = backrefs.remove(pointer.get)
        }
        // Exclude 'from'
        return path.tail
      } else {
        for ((next_state_raw, transition_cost) <- current.state.succs) {
          val next_state = next_state_raw.asInstanceOf[DirectedRoad]
          val tentative_cost = add_cost(
            costs(current.state), cost_step(transition_cost, next_state, time)
          )
          if (!visited.contains(next_state) && (!open_members.contains(next_state) || ordering_tuple.lt(tentative_cost, costs(next_state)))) {
            backrefs(next_state) = current.state
            costs(next_state) = tentative_cost
            // TODO if they're in open_members, modify weight in the queue? or
            // new step will clobber it. fine.
            open.enqueue(Step(next_state))
            open_members += next_state
          }
        }
      }
    }

    // We didn't find the way?! The graph is connected!
    throw new Exception("Couldn't A* from " + from + " to " + to)
  }
}

// No guess for cost, straight-line distance at 1m/s for freeflow time
trait SimpleHeuristic extends AbstractPairAstarRouter {
  override def calc_heuristic(state: DirectedRoad, goal: DirectedRoad) =
    (0.0, state.end_pt.dist_to(goal.end_pt))  // TODO divided by some speed limit?
  // Alternate heuristics explore MUCH less states, but the oracles are too
  // pricy.
  /*def calc_heuristic(state: DirectedRoad) =
    (0.0, graph.ch_router.dist(state, to))*/
  /*val table = graph.dijkstra_router.costs_to(to)
  def calc_heuristic(state: DirectedRoad) =
    (0.0, table(state.id))*/
}

// Cost for each step is (dollars, time)
trait TollAndTimeCost extends AbstractPairAstarRouter {
  override def cost_step(turn_cost: Double, state: DirectedRoad, time: Double) =
    (state.toll.dollars, turn_cost + state.cost(time))
}

// Score is (number of congested roads, total freeflow time)
class CongestionRouter(graph: Graph) extends AbstractPairAstarRouter(graph) with SimpleHeuristic {
  override def router_type = RouterType.Congestion

  override def cost_step(turn_cost: Double, state: DirectedRoad, time: Double) =
    (Util.bool2binary(state.is_congested), turn_cost + state.cost(time))
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

  override def cost_step(turn_cost: Double, state: DirectedRoad, time: Double) =
    (Util.bool2binary(state.toll.dollars > max_toll.dollars), turn_cost + state.cost(time))
}

// Score is (sum of tolls, total freeflow time). The answer is used as the "free" baseline with the
// least cost to others.
class SumTollRouter(graph: Graph) extends AbstractPairAstarRouter(graph)
  with SimpleHeuristic with TollAndTimeCost
{
  override def router_type = RouterType.SumToll

  override def cost_step(turn_cost: Double, state: DirectedRoad, time: Double) =
    (state.toll.dollars, turn_cost + state.cost(time))
}
