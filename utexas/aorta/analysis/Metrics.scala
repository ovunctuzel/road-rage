// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, Sim_Event, Route_Event, EV_AgentSpawned, EV_AgentQuit,
                         EV_Reroute}
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
  def percent_time_orig(a: AgentID) = first_reroute_time.get(a) match {
    case Some(tick) => 100.0 * (tick - start_time(a)) / (stop_time(a) - start_time(a))
    case None => 100.0
  }
}
