// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable.{PriorityQueue, HashSet, ListBuffer, HashMap}
import com.graphhopper.storage.{LevelGraphStorage, RAMDirectory}
import com.graphhopper.routing.ch.PrepareContractionHierarchies

import utexas.aorta.map.{Graph, DirectedRoad, Coordinate}
import utexas.aorta.sim.{IntersectionType, Scenario}

import utexas.aorta.common.{Util, Common, Physics, RNG, DirectedRoadID}

abstract class Router(graph: Graph) {
  // Doesn't include 'from' as the first step
  def path(from: DirectedRoad, to: DirectedRoad, time: Double): List[DirectedRoad]
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
    val open = new PriorityQueue[Step]()
    val done = new HashSet[AbstractEdge]()

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

    if (Common.sim != null) {
      Common.sim.ch_since_last_time += 1
    }

    Util.assert_eq(usable, true)
    val path = algo.calcPath(from.id.int, to.id.int)
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
    val path = algo.calcPath(from.id.int, to.id.int)
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
  stop_sign_count: Double,        // number of intersections crossed
  signal_count: Double,
  reservation_count: Double,
  // TODO account for intersection type?
  queued_turn_count: Double,      // sum of queued turns at each intersection
  // TODO historical avg/max. and count this carefully!
  total_avg_waiting_time: Double, // sum of average waiting time of turns at each intersection
  road_demand: Double,            // number of agents who want to use this road sometime
  intersection_demand: Double,    // likewise for intersections
  agents_enroute: Double          // number of agents on the path right now
) {
  // TODO impl as taking a list, so we can do dot product and + easily

  def +(other: RouteFeatures) = RouteFeatures(
    total_length + other.total_length,
    total_freeflow_time + other.total_freeflow_time,
    congested_road_count + other.congested_road_count,
    stop_sign_count + other.stop_sign_count,
    signal_count + other.signal_count,
    reservation_count + other.reservation_count,
    queued_turn_count + other.queued_turn_count,
    total_avg_waiting_time + other.total_avg_waiting_time,
    road_demand + other.road_demand,
    intersection_demand + other.intersection_demand,
    agents_enroute + other.agents_enroute)

  def /(other: RouteFeatures) = RouteFeatures(
    total_length / other.total_length,
    total_freeflow_time / other.total_freeflow_time,
    congested_road_count / other.congested_road_count,
    stop_sign_count / other.stop_sign_count,
    signal_count / other.signal_count,
    reservation_count / other.reservation_count,
    queued_turn_count / other.queued_turn_count,
    total_avg_waiting_time / other.total_avg_waiting_time,
    road_demand / other.road_demand,
    intersection_demand / other.intersection_demand,
    agents_enroute / other.agents_enroute)

  // weights should also account for normalization by dividing by the "soft max" for each feature
  def score(weights: RouteFeatures): Double
    = ((total_length * weights.total_length)
    + (total_freeflow_time * weights.total_freeflow_time)
    + (congested_road_count * weights.congested_road_count)
    + (stop_sign_count * weights.stop_sign_count)
    + (signal_count * weights.signal_count)
    + (reservation_count * weights.reservation_count)
    + (queued_turn_count * weights.queued_turn_count)
    + (total_avg_waiting_time * weights.total_avg_waiting_time)
    + (road_demand * weights.road_demand)
    + (intersection_demand * weights.intersection_demand)
    + (agents_enroute * weights.agents_enroute))

  def toList = List(
    total_length, total_freeflow_time, congested_road_count, stop_sign_count, signal_count,
    reservation_count, queued_turn_count, total_avg_waiting_time, road_demand, intersection_demand,
    agents_enroute
  )
}

object RouteFeatures {
  // Some presets
  val BLANK = RouteFeatures(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val JUST_FREEFLOW_TIME = BLANK.copy(total_freeflow_time = 1)

  def for_step(step: DirectedRoad, demand: Demand): RouteFeatures = {
    def one_if(matches: IntersectionType.Value) =
      if (step.to.intersection.policy.policy_type == matches)
        1.0
      else
        0.0
    return RouteFeatures(
      total_length = step.length,
      total_freeflow_time = step.cost(0.0),
      congested_road_count = if (step.is_congested) 1 else 0,
      stop_sign_count = one_if(IntersectionType.StopSign),
      signal_count = one_if(IntersectionType.Signal),
      reservation_count = one_if(IntersectionType.Reservation),
      queued_turn_count = step.to.intersection.policy.queued_count,
      total_avg_waiting_time = step.to.intersection.average_waiting_time,
      road_demand = demand.directed_roads(step.id.int).toDouble,
      intersection_demand = demand.intersections(step.to.id.int).toDouble,
      agents_enroute = step.edges.map(_.queue.agents.size).sum
    )
  }

  private val rng = new RNG()
  // Produce more diverse paths by rewarding things usually considered bad
  private def rnd = rng.double(-1, 1)
  def random_weight = RouteFeatures(rnd, rnd, rnd, rnd, rnd, rnd, rnd, rnd, rnd, rnd, rnd)
}

// Magically knows where everybody wants to go ahead of time. Think of this as representing a
// historical average, though.
case class Demand(directed_roads: Array[Integer], intersections: Array[Integer])

object Demand {
  def blank_for(scenario: Scenario, graph: Graph) =
    Demand(Array.fill(graph.directed_roads.size)(0), Array.fill(graph.vertices.size)(0))

  def demand_for(scenario: Scenario, graph: Graph): Demand = {
    val demand = blank_for(scenario, graph)
    for (a <- scenario.agents) {
      val from = graph.edges(a.start_edge.int).directed_road
      val to = graph.edges(a.route.goal.int).directed_road
      for (step <- graph.router.path(from, to, 0)) {
        demand.directed_roads(step.id.int) += 1
        demand.intersections(step.to.id.int) += 1
      }
    }
    return demand
  }
}

// A* is a misnomer; there's no heuristic right now.
class AstarRouter(graph: Graph, raw_weights: RouteFeatures, demand: Demand)
  extends Router(graph)
{
  val weights = raw_weights / max_weights()

  override def path(from: DirectedRoad, to: DirectedRoad, time: Double) = scored_path(from, to)._1

  // Return the weight of the final path too
  def scored_path(from: DirectedRoad, to: DirectedRoad): (List[DirectedRoad], RouteFeatures) = {
    if (from == to) {
      return (Nil, RouteFeatures.BLANK)
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
        return (path.tail, costs(current.state))
      } else {
        for ((next_state_raw, _) <- current.state.succs) {
          val next_state = next_state_raw.asInstanceOf[DirectedRoad]
          val tentative_cost = costs(current.state) + RouteFeatures.for_step(next_state, demand)
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

  def max_weights(): RouteFeatures = {
    // These are "soft max" values, meaning the actual value could exceed these, but generally they
    // wont.
    val min_x = graph.vertices.map(_.location.x).min
    val max_x = graph.vertices.map(_.location.x).max
    val min_y = graph.vertices.map(_.location.y).min
    val max_y = graph.vertices.map(_.location.y).max
    val max_length = new Coordinate(min_x, min_y).dist_to(new Coordinate(max_x, max_y))
    val max_time = max_length / Physics.mph_to_si(30)
    val max_congestion = graph.directed_roads.size
    val intersections = graph.vertices.groupBy(_.intersection.policy.policy_type)
    val max_queued_turns = graph.edges.map(e => e.queue.capacity).sum
    val max_avg_waiting = graph.vertices.size * 60  // particularly arbitrary, this one!
    // These depend on the scenario size! Really no way to pick.
    val max_road_demand = 15000
    val max_intersection_demand = 15000
    val max_enroute = 15000
    return RouteFeatures(
      max_length, max_time, max_congestion,
      intersections(IntersectionType.StopSign).size,
      intersections(IntersectionType.Signal).size,
      intersections(IntersectionType.Reservation).size,
      max_queued_turns, max_avg_waiting, max_road_demand, max_intersection_demand, max_enroute
    )
  }
}
