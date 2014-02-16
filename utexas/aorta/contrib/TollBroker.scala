// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.sim.EV_Reroute
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.map.{Road, Turn}

class TollBroker(a: Agent) {
  private val k = 5
  private val registrations = new mutable.HashSet[Road]()

  // Setup
  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => {
      for (r <- registrations -- a.route.next_roads(k)) {
        r.road_agent.tollbooth.cancel(a)
      }
      // We'll register for the new roads during react()
    }
  })

  // Invoked before lookahead behavior runs
  def react() {
    // Only register while on roads
    a.at.on match {
      case t: Turn => return
      case _ =>
    }

    // Register for the next k=5 roads
    var eta = a.at.dist_left / a.at.on.speed_limit
    for (r <- a.route.next_roads(k) if !registrations.contains(r)) {
      r.road_agent.tollbooth.register(a, eta)
      registrations += r
      eta += r.freeflow_time
    }
  }
}
