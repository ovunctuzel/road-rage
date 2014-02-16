// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.sim.RoadAgent
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.common.Util

// Manage reservations to use roads
class Tollbooth(road: RoadAgent) {
  // TODO serialization and such
  // map to ETA
  private val registrations = new mutable.HashMap[Agent, Double]()

  def register(a: Agent, eta: Double) {
    Util.assert_eq(registrations.contains(a), false)
    registrations(a) = eta
  }

  def cancel(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    registrations -= a
  }

  def enter(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val lag = a.sim.tick - registrations(a)
    // Drivers are arriving both early and late right now
    //println(s"$a arrived $lag late")
  }

  def exit(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    registrations -= a
  }

  def verify_done() {
    Util.assert_eq(registrations.isEmpty, true)
  }

  // TODO tmp. this'll get fancier soon.
  def toll(eta: Double) = road.toll
}
