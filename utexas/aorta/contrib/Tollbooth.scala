// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.sim.RoadAgent
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.common.{Util, Price, BatchDuringStep}

// Manage reservations to use roads
class Tollbooth(road: RoadAgent) extends BatchDuringStep[Request] {
  private val half_duration = 15 * 60.0
  // TODO serialization and such
  // map to ETA
  private val registrations = new mutable.HashMap[Agent, Double]()
  // The int idx is 0 for 0-30 mins, 1 for 15-45 mins, etc
  private val slots = new mutable.HashMap[Int, mutable.Set[Agent]] with mutable.MultiMap[Int, Agent]
  // TODO refactor the maintenance of registrations/slots and all the asserts

  // TODO this assigns 16 to 15-45, so eta better be a lower bound... desirable?
  private def idx(eta: Double) = math.floor(eta / half_duration).toInt

  def register(a: Agent, eta: Double) {
    Util.assert_eq(registrations.contains(a), false)
    Util.assert_eq(slots.entryExists(idx(eta), _ == a), false)
    add_request(Request(a, eta))
  }

  def cancel(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val eta = registrations(a)
    Util.assert_eq(slots.entryExists(idx(eta), _ == a), true)
    // TODO is it possible to cancel a request they made earlier during the tick? I think not...
    registrations -= a
    slots.removeBinding(idx(eta), a)
  }

  def enter(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val eta = registrations(a)
    Util.assert_eq(slots.entryExists(idx(eta), _ == a), true)
    val lag = a.sim.tick - eta
    // Drivers are arriving both early and late right now
    //println(s"$a arrived $lag late")
  }

  def exit(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val eta = registrations(a)
    Util.assert_eq(slots.entryExists(idx(eta), _ == a), true)
    registrations -= a
    slots.removeBinding(idx(eta), a)
  }

  def verify_done() {
    Util.assert_eq(registrations.isEmpty, true)
    Util.assert_eq(slots.isEmpty, true)
  }

  def react() {
    end_batch_step()
    for (r <- request_queue) {
      registrations(r.a) = r.eta
      slots.addBinding(idx(r.eta), r.a)
    }
    request_queue = Nil
  }

  // Simple policy for now.
  def toll(eta: Double) = new Price(slots.getOrElse(idx(eta), Set()).size / road.freeflow_capacity)
}

case class Request(a: Agent, eta: Double) extends Ordered[Request] {
  override def compare(other: Request) = Ordering[Tuple2[Int, Double]].compare(
    (a.id.int, eta), (other.a.id.int, other.eta)
  )
}
