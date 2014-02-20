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
  private val cancellation_fee = 5.0
  private val early_fee = 10.0
  private val late_fee = 10.0

  private val half_duration = 15 * 60.0
  // TODO serialization and such
  // map to (ETA, offer)
  private val registrations = new mutable.HashMap[Agent, (Double, Double)]()
  // The int idx is 0 for 0-30 mins, 1 for 15-45 mins, etc
  private val current_prices = new mutable.HashMap[Int, Double]()
  // TODO refactor the maintenance of registrations/slots and all the asserts

  // TODO this assigns 16 to 15-45, so eta better be a lower bound... desirable?
  private def idx(eta: Double) = math.floor(eta / half_duration).toInt

  // The offer doesn't have to be past any threshold; it's up to the drivers to reroute and see high
  // new toll
  def register(a: Agent, eta: Double, offer: Double) {
    Util.assert_eq(registrations.contains(a), false)
    Util.assert_ge(offer, 0)
    add_request(Request(a, eta, toll(eta).dollars, offer))
  }

  def cancel(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val (eta, offer) = registrations(a)
    // TODO is it possible to cancel a request they made earlier during the tick? I think not...
    registrations -= a
    current_prices(idx(eta)) -= offer
    a.toll_broker.spend(cancellation_fee)
  }

  def enter(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val (eta, _) = registrations(a)

    // Arriving early/late could be a way to game the system. For the driver behaviors in TollBroker
    // now that don't try to cheat, arriving early/late is actually a bug.
    val earliest_time = idx(eta) * half_duration
    val latest_time = (idx(eta) + 2) * half_duration
    if (a.sim.tick < earliest_time) {
      a.toll_broker.spend(early_fee)
      //Util.log(s"$a (rank ${a.wallet.priority}) early to ${road.r}: ${a.sim.tick} < $earliest_time. eta was $eta")
    }
    if (a.sim.tick > latest_time) {
      a.toll_broker.spend(late_fee)
      //Util.log(s"$a (rank ${a.wallet.priority}) late to ${road.r}: ${a.sim.tick} > $latest_time. eta was $eta")
    }
  }

  def exit(a: Agent) {
    Util.assert_eq(registrations.contains(a), true)
    val (eta, offer) = registrations(a)
    registrations -= a
    current_prices(idx(eta)) -= offer
  }

  def verify_done() {
    Util.assert_eq(registrations.isEmpty, true)
    Util.assert_eq(current_prices.values.toSet, Set(0))
  }

  def react() {
    end_batch_step()
    for (r <- request_queue) {
      registrations(r.a) = (r.eta, r.offer)
      current_prices(idx(r.eta)) = current_prices.getOrElse(idx(r.eta), 0.0) + r.offer
      // What should people spend?
      r.a.toll_broker.spend(r.offer)
    }
    request_queue = Nil
  }

  // Simple policy for now.
  def toll(eta: Double) = new Price(current_prices.getOrElse(idx(eta), 0.0) / road.freeflow_capacity)
}

// We, the tollbooth, set the toll at the time the request is made
case class Request(a: Agent, eta: Double, old_toll: Double, offer: Double) extends Ordered[Request] {
  override def compare(other: Request) = Ordering[Tuple2[Int, Double]].compare(
    (a.id.int, eta), (other.a.id.int, other.eta)
  )
}
