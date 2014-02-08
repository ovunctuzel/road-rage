// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.routes

import scala.collection.mutable

import utexas.aorta.sim.{Simulation, EV_LinkChanged, EV_Transition, EV_Reroute}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.map.Turn

// When any link in an agent's path changes state, inform them.
// TODO consider updating at the zone granularity
class RouteChangeWatcher(sim: Simulation) {
  // Subscribed when the road was congested
  private val subscribers_congested
    = sim.graph.roads.map(r => r -> new mutable.HashSet[Agent]()).toMap
  // Subscribed when the road was clear
  private val subscribers_clear = sim.graph.roads.map(r => r -> new mutable.HashSet[Agent]()).toMap

  sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(a, path, _, _, _, old_path) => {
      for (r <- old_path) {
        subscribers_congested(r) -= a
        subscribers_clear(r) -= a
      }
      for (r <- path) {
        if (r.auditor.congested) {
          subscribers_congested(r) += a
        } else {
          subscribers_clear(r) += a
        }
      }
    }
  })
  sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, _, to: Turn) => {
      subscribers_congested(to.from.road) -= a
      subscribers_clear(to.from.road) -= a
    }
    case _ =>
  })
  sim.listen(classOf[EV_LinkChanged], _ match { case EV_LinkChanged(r, congested) => {
    if (congested) {
      //subscribers_clear(r).foreach(a => ...)
    } else {
      // Maybe don't tell these guys?
      //subscribers_congested(r).foreach(a => ...)
    }
  }})
}
