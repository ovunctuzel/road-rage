// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.sim.EV_Reroute
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.map.{Road, Turn, Edge}
import utexas.aorta.common.Util

class TollBroker(a: Agent) {
  //private val k = 5
  private val k =
    if (a.wallet.priority > 250)
      20
    else
      5
  private val registrations = new mutable.HashSet[Road]()
  private var total_cost = 0.0

  // Setup
  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => {
      val obselete = registrations -- a.route.next_roads(k)
      // TODO cancel all and re-calculate eta for planned ones, IF we're changing path to get there?
      for (r <- obselete) {
        r.road_agent.tollbooth.cancel(a)
      }
      registrations --= obselete
      // We'll register for the new roads during react()
    }
  })

  def exited(r: Road) {
    Util.assert_eq(registrations.contains(r), true)
    registrations -= r
  }

  // Invoked before lookahead behavior runs
  def react() {
    // Only register while on roads
    a.at.on match {
      case t: Turn => return
      case _ =>
    }

    // TODO bad workaround. agents that start on same road they end break otherwise.
    val next_roads = a.route.next_roads(k) match {
      case Nil => List(a.at.on.asInstanceOf[Edge].road)
      case ls => ls
    }

    // Register for the next k=5 roads
    var eta = a.sim.tick + a.at.dist_left / a.at.on.speed_limit
    for (r <- next_roads) {
      if (!registrations.contains(r)) {
        // For now, offer to raise the toll by the value of our priority
        // TODO * freeflow time?
        val offer = a.wallet.priority
        r.road_agent.tollbooth.register(a, eta, offer)
        registrations += r
      }
      // We've already counted the current road, so start with the next
      if (r != next_roads.head) {
        eta += r.freeflow_time
      }
    }
  }

  // only Tollbooths should call this
  def spend(amount: Double) {
    Util.assert_ge(amount, 0)
    total_cost += amount
  }
}
