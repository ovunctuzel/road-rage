// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, Sim_Event, Route_Event, EV_AgentSpawned, EV_AgentQuit,
                         EV_Reroute, IntersectionType, EV_Stat, EV_IntersectionOutcome}
import utexas.aorta.common.{Common, AgentID}

import scala.collection.mutable

// Measure how long a driver follows their original route.
class OriginalRouteMetric(sim: Simulation) {
  // TODO make this one create a TripTimeMetric or so, reusing code to get start/stop time

  // TODO map all to times
  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  private val start_time = new mutable.HashMap[AgentID, Double]()
  private val stop_time = new mutable.HashMap[AgentID, Double]()

  sim.listen("orig-route", (sim_ev: Sim_Event) => sim_ev match {
    case EV_AgentSpawned(a) => a.route.listen("orig-route", (ev: Route_Event) => ev match {
      case EV_Reroute(_, false) if !first_reroute_time.contains(a.id) =>
        first_reroute_time(a.id) = Common.tick
      case _ =>
    })
    case EV_AgentQuit(a) => {
      start_time(a.id) = a.time_spawned
      stop_time(a.id) = Common.tick
    }
    case _ =>
  })

  // [0, 100]
  def apply(a: AgentID) = first_reroute_time.get(a) match {
    case Some(tick) => 100.0 * (tick - start_time(a)) / (stop_time(a) - start_time(a))
    case None => 100.0
  }
}

// Measure how long drivers wait at intersections
class TurnDelayMetric(sim: Simulation) {
  // TODO correlate with the agent that experiences it. problem is how to do the same for
  // TurnCompetitionMetric, where there could be multiple winners.
  private val delay_per_policy = IntersectionType.values.toList.map(
    t => t -> new mutable.ListBuffer[Double]()
  ).toMap

  sim.listen("turn-delay", (sim_ev: Sim_Event) => sim_ev match {
    case EV_Stat(s: Turn_Stat) => {
      val t = Common.sim.graph.vertices(s.vert.int).intersection.policy.policy_type
      delay_per_policy(t) += s.total_delay  // could be accept_delay
    }
    case _ =>
  })

  def delays = delay_per_policy.keys.map(p => p.toString -> delay_per_policy(p).toList).toMap
}

// Measure how much competition is present at intersections
class TurnCompetitionMetric(sim: Simulation) {
  private val losers_per_policy = IntersectionType.values.toList.map(
    t => t -> new mutable.ListBuffer[Double]()
  ).toMap

  sim.listen("turn-competition", (sim_ev: Sim_Event) => sim_ev match {
    case EV_IntersectionOutcome(policy, losers) => {
      losers_per_policy(policy) += losers.size
    }
    case _ =>
  })

  def competition = losers_per_policy.keys.map(p => p.toString -> losers_per_policy(p).toList).toMap
}

// TODO make a class of per-agent metrics, and have a way to merge them..

// TODO delay on roads vs at intersections?
