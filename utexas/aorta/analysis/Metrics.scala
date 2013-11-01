// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, Sim_Event, Route_Event, EV_AgentSpawned, EV_AgentQuit,
                         EV_Reroute, IntersectionType, EV_Stat, EV_IntersectionOutcome,
                         EV_Transition}
import utexas.aorta.map.{Edge, DirectedRoad, Turn}
import utexas.aorta.common.{Common, AgentID}

import scala.collection.mutable

// TODO one metric base class with name, output method

// Record one double per agent
abstract class SinglePerAgentMetric {
  protected val per_agent = new mutable.HashMap[AgentID, Double]()
  def apply(a: AgentID) = per_agent(a)
  def name: String
}

// Record many doubles per agent, further grouped by some string category
abstract class MultiplePerAgentMetric {
  type Key = (AgentID, String)
  protected val per_agent_category = new mutable.HashMap[Key, mutable.Set[Double]]
    with mutable.MultiMap[Key, Double]
  def name: String
}

// Measure how long each agent's trip takes
class TripTimeMetric(sim: Simulation) extends SinglePerAgentMetric {
  override def name = "trip_time"

  sim.listen(name, _ match {
    case EV_Stat(s: Agent_Lifetime_Stat) => per_agent(s.id) = s.trip_time
    case _ =>
  })
}

// Measure how long a driver follows their original route.
class OriginalRouteMetric(sim: Simulation) extends SinglePerAgentMetric {
  override def name = "orig_routes"

  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  sim.listen(name, _ match {
    case EV_Reroute(_, false) if !first_reroute_time.contains(a.id) =>
      first_reroute_time(a.id) = Common.tick
    // [0, 100]
    case EV_Stat(s: Agent_Lifetime_Stat) => per_agent(s.id) =
      100.0 * ((first_reroute_time.getOrElse(a, s.end_tick) - s.start_tick) / s.trip_time)
    case _ =>
  })
}

// Measure how long drivers wait at intersections, grouped by intersection type
class TurnDelayMetric(sim: Simulation) extends MultiplePerAgentMetric {
  override def name = "turn_delays"

  sim.listen(name, _ match {
    case EV_Stat(s: Turn_Stat) => {
      val policy = Common.sim.graph.vertices(s.vert.int).intersection.policy.policy_type.toString
      per_agent_category((s.agent, policy)) += s.total_delay  // could be accept_delay
    }
    case _ =>
  })
}

// Measure how congested roads are when agents enter them, grouped by nothing (category "all")
class RoadCongestionMetric(sim: Simulation) extends MultiplePerAgentMetric {
  override def name = "road_congestion"

  sim.listen(name, _ match {
    case EV_AgentSpawned(a) => a.route.listen(name, _ match {
      case EV_Transition(_, to: Edge) =>
        per_agent_category((a.id, "all")) += to.directed_road.freeflow_percent_full
      case _ =>
    })
    case _ =>
  })
}

// Measure how much competition is present at intersections
class TurnCompetitionMetric(sim: Simulation) {
  private val losers_per_policy = IntersectionType.values.toList.map(
    t => t -> new mutable.ListBuffer[Double]()
  ).toMap

  sim.listen("turn-competition", _ match {
    case EV_IntersectionOutcome(policy, losers) => {
      losers_per_policy(policy) += losers.size
    }
    case _ =>
  })

  def competition = losers_per_policy.keys.map(p => p.toString -> losers_per_policy(p).toList).toMap
}

class RouteRecordingMetric(sim: Simulation) {
  def name = "route_recording"

  private val routes = new mutable.HashMap[AgentID, mutable.ListBuffer[DirectedRoad]]()

  sim.listen(name, _ match {
    case EV_AgentSpawned(a) => {
      routes(a.id) = new mutable.ListBuffer[DirectedRoad]()
      a.route.listen(name, _ match {
        case EV_Transition(from, to: Turn) => {
          val path = routes(a.id)
          if (path.isEmpty) {
            path += t.from.directed_road
          }
          path += t.to.directed_road
        }
        case _ =>
      })
    }
    case _ =>
  })

  def apply(a: AgentID) = routes(a).toList
}

// TODO delay on roads vs at intersections?
