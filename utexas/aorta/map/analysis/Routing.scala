// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable.{PriorityQueue, HashSet, ListBuffer, HashMap}
import com.graphhopper.storage.{LevelGraphStorage, RAMDirectory}
import com.graphhopper.routing.ch.PrepareContractionHierarchies

import utexas.aorta.map.{Graph, DirectedRoad}

import utexas.aorta.common.{Util, Common, Physics, RNG}

abstract class Router(graph: Graph) {
  // Doesn't include 'from' as the first step
  def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad]
}

// A convenient abstraction if we ever switch to pathfinding on
// edges/roads/others.
abstract class AbstractEdge {
  var id: Int // TODO var because fix_ids
  def cost(time: Double): Double
  // List and not Set means there could be multiple transitions, with possibly
  // different weights.
  def succs: Seq[(AbstractEdge, Double)]
  def preds: Seq[(AbstractEdge, Double)]
}

class DijkstraRouter(graph: Graph) extends Router(graph) {
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
      def cost = costs(edge.id)
      def compare(other: Step) = other.cost.compare(cost)
    }

    // Edges in the open set don't have their final cost yet
    val open = new PriorityQueue[Step]()
    val done = new HashSet[AbstractEdge]()

    costs(source.id) = 0
    open.enqueue(new Step(source))

    while (open.nonEmpty) {
      val step = open.dequeue
      
      // Skip duplicate steps, since we chose option 3 for the problem below.
      if (!done.contains(step.edge)) {
        done += step.edge

        for ((next, transition_cost) <- next(step.edge) if !done.contains(next)) {
          val cost = step.cost + transition_cost + next.cost(time)
          if (cost < costs(next.id)) {
            // Relax!
            costs(next.id) = cost
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
    costs(start.id) match {
      case 0 => start :: Nil
      case c => start :: hillclimb(
        costs, start.succs.minBy(t => costs(t._1.id))._1
      )
    }
}

class CHRouter(graph: Graph) extends Router(graph) {
  private val gh = new LevelGraphStorage(
    new RAMDirectory(s"maps/route_${graph.name}", true)
  )
  var usable = gh.loadExisting
  private val algo = new PrepareContractionHierarchies().graph(gh).createAlgo

  def path(from: DirectedRoad, to: DirectedRoad, path: Double): List[DirectedRoad] = {
    // GraphHopper can't handle loops. For now, empty path; freebie.
    // TODO force a loop by starting on a road directly after 'from'
    if (from == to) {
      return Nil
    }

    Common.sim.ch_since_last_time += 1

    Util.assert_eq(usable, true)
    val path = algo.calcPath(from.id, to.id)
    algo.clear
    Util.assert_eq(path.found, true)

    val result = new ListBuffer[DirectedRoad]()
    var iter = path.calcNodes.iterator
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

    Util.assert_eq(usable, true)
    val path = algo.calcPath(from.id, to.id)
    algo.clear
    Util.assert_eq(path.found, true)
    return path.distance
  }
}

// Run sparingly -- it's A* that penalizes going through roads that're congested
// right now
// TODO remove, it's a degenerate case of the future impl...
class CongestionRouter(graph: Graph) extends Router(graph) {
  // TODO clean this up >_<
  def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad] = {
    if (from == to) {
      return Nil
    }

    Common.sim.astar_since_last_time += 1

    val goal_pt = to.end_pt
    def calc_heuristic(state: DirectedRoad) =
      (0.0, state.end_pt.dist_to(goal_pt))
    // Alternate heuristics explore MUCH less states, but the oracles are too
    // pricy.
    /*def calc_heuristic(state: DirectedRoad) =
      (0.0, graph.ch_router.dist(state, to))*/
    /*val table = graph.dijkstra_router.costs_to(to)
    def calc_heuristic(state: DirectedRoad) =
      (0.0, table(state.id))*/

    def add_cost(a: (Double, Double), b: (Double, Double)) =
      (a._1 + b._1, a._2 + b._2)

    // Stitch together our path
    val backrefs = new HashMap[DirectedRoad, DirectedRoad]()
    // We're finished with these
    val visited = new HashSet[DirectedRoad]()
    // Best cost so far
    val costs = new HashMap[DirectedRoad, (Double, Double)]()

    case class Step(state: DirectedRoad) {
      lazy val heuristic = calc_heuristic(state)
      def cost = add_cost(costs(state), heuristic)
    }
    val ordering = Ordering[(Double, Double)].on((step: Step) => step.cost).reverse
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    // Priority queue grabs highest priority first, so reverse to get lowest
    // cost first.
    val open = new PriorityQueue[Step]()(ordering)
    // Used to see if we've already added a road to the queue
    val open_members = new HashSet[DirectedRoad]()

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
          val next_time_cost = transition_cost + next_state.cost(time)
          val next_congestion_cost =
            if (next_state.is_congested)
              1.0
            else
              0.0
          val tentative_cost = add_cost(
            costs(current.state), (next_congestion_cost, next_time_cost)
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

// Encodes all factors describing the quality of a path
case class RouteFeatures(
  total_length: Double,           // sum of path length
  total_freeflow_time: Double,    // sum of time to cross each road at the speed limit
  congested_road_count: Double,   // number of congested roads
  // TODO historical avg/max of road congestion, and more than a binary is/is not
  intersection_count: Double,     // number of intersections crossed
  // TODO account for intersection type?
  queued_turn_count: Double,      // sum of queued turns at each intersection
  // TODO historical avg/max. and count this carefully!
  total_avg_waiting_time: Double  // sum of average waiting time of turns at each intersection
) {
  // TODO impl as taking a list, so we can do dot product and + easily

  def +(other: RouteFeatures) = RouteFeatures(
    total_length + other.total_length,
    total_freeflow_time + other.total_freeflow_time,
    congested_road_count + other.congested_road_count,
    intersection_count + other.intersection_count,
    queued_turn_count + other.queued_turn_count,
    total_avg_waiting_time + other.total_avg_waiting_time)

  def *(other: RouteFeatures) = RouteFeatures(
    total_length * other.total_length,
    total_freeflow_time * other.total_freeflow_time,
    congested_road_count * other.congested_road_count,
    intersection_count * other.intersection_count,
    queued_turn_count * other.queued_turn_count,
    total_avg_waiting_time * other.total_avg_waiting_time)

  // weights should also account for normalization by dividing by the "soft max" for each feature
  def score(weights: RouteFeatures): Double
    = ((total_length * weights.total_length)
    + (total_freeflow_time * weights.total_freeflow_time)
    + (congested_road_count * weights.congested_road_count)
    + (intersection_count * weights.intersection_count)
    + (queued_turn_count * weights.queued_turn_count)
    + (total_avg_waiting_time * weights.total_avg_waiting_time))
}

object RouteFeatures {
  // Some presets
  val BLANK = RouteFeatures(0, 0, 0, 0, 0, 0)
  val JUST_FREEFLOW_TIME = BLANK.copy(total_freeflow_time = 1)

  def for_step(step: DirectedRoad) = RouteFeatures(
    total_length = step.length,
    total_freeflow_time = step.cost(0.0),
    congested_road_count = if (step.is_congested) 1 else 0,
    intersection_count = 1,
    queued_turn_count = step.to.intersection.policy.queued_count,
    total_avg_waiting_time = step.to.intersection.average_waiting_time)

  private val rng = new RNG()
  def random_weight = RouteFeatures(
    rng.double(0, 1), rng.double(0, 1), rng.double(0, 1), rng.double(0, 1), rng.double(0, 1),
    rng.double(0, 1)
  )
}

// A* is a misnomer; there's no heuristic right now.
class AstarRouter(graph: Graph, raw_weights: RouteFeatures) extends Router(graph) {
  private val weights = raw_weights * normalize_weights()

  override def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad] = {
    if (from == to) {
      return Nil
    }

    Common.sim.astar_since_last_time += 1

    // Stitch together our path
    val backrefs = new HashMap[DirectedRoad, DirectedRoad]()
    // We're finished with these
    val visited = new HashSet[DirectedRoad]()
    // Best cost so far
    val costs = new HashMap[DirectedRoad, RouteFeatures]()
    // Used to see if we've already added a road to the queue
    val open_members = new HashSet[DirectedRoad]()

    case class Step(state: DirectedRoad) {
      // No heuristics for now
      def cost = costs(state).score(weights)
    }
    val ordering = Ordering[Double].on((step: Step) => step.cost).reverse
    // Priority queue grabs highest priority first, so reverse to get lowest
    // cost first.
    val open = new PriorityQueue[Step]()(ordering)

    costs(from) = RouteFeatures.BLANK
    open.enqueue(Step(from))
    open_members += from
    backrefs(from) = null

    while (open.nonEmpty) {
      val current = open.dequeue()
      //println(s"- examining ${current.state} with cost ${current.cost} (${costs(current.state)})")
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
        for ((next_state_raw, _) <- current.state.succs) {
          val next_state = next_state_raw.asInstanceOf[DirectedRoad]
          val tentative_cost = costs(current.state) + RouteFeatures.for_step(next_state)
          if (!visited.contains(next_state) && (!open_members.contains(next_state) || tentative_cost.score(weights) < costs(next_state).score(weights))) {
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

  private def normalize_weights(): RouteFeatures = {
    // These are "soft max" values, meaning the actual value could exceed these, but generally they
    // wont.
    val max_length = graph.width + graph.height // TODO is this the right dist unit?
    val max_time = max_length / Physics.mph_to_si(30)
    val max_congestion = graph.directed_roads.size
    val max_intersections = graph.vertices.size
    val max_queued_turns = graph.edges.map(e => e.queue.capacity).sum
    val max_avg_waiting = graph.vertices.size * 60  // particularly arbitrary, this one!
    return RouteFeatures(max_length, max_time, max_congestion, max_intersections, max_queued_turns,
      max_avg_waiting)
  }
}
