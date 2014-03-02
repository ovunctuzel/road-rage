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
  // We've also registered for intersections at the end of each road
  private val registrations = new mutable.HashSet[Road]()
  private var total_cost = 0.0

  // Setup
  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => {
      val obselete = registrations -- a.route.current_path.take(k)
      // TODO cancel all and re-calculate eta for planned ones, IF we're changing path to get there?
      // TODO cancel intersections if we're going to take a different turn
      for (r <- obselete) {
        r.road_agent.tollbooth.cancel(a)
        if (r != a.route.goal) {
          r.to.intersection.tollbooth.cancel(a, r)
        }
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
    val next_roads = a.route.current_path match {
      case Nil => Array(a.at.on.asInstanceOf[Edge].road)
      case ls => ls.toArray
    }

    // Register for the next k=5 roads
    var eta = a.sim.tick + a.at.dist_left / a.at.on.speed_limit
    for (idx <- Range(0, math.min(k, next_roads.size))) {
      val r = next_roads(idx)
      val next_r = next_roads.lift(idx + 1)
      var registered = false
      // For now, offer to raise the toll by the value of our priority
      // TODO * freeflow time?
      val offer = a.wallet.priority

      if (!registrations.contains(r)) {
        registered = true
        r.road_agent.tollbooth.register(a, eta, offer)
        registrations += r
      }
      // We've already counted the current road, so start with the next
      if (r != next_roads.head) {
        eta += r.freeflow_time
      }
      // Update the eta before registering for the intersection
      if (registered && next_r.isDefined) {
        r.to.intersection.tollbooth.register(a, r, eta, next_r.get, offer)
      }
    }
  }

  // only Tollbooths should call this
  def spend(amount: Double) {
    Util.assert_ge(amount, 0)
    total_cost += amount
  }
}
