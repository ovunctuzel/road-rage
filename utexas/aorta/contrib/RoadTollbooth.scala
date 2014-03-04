// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.map.Road
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.RoadAgent
import utexas.aorta.common.{Util, Price, BatchDuringStep}

// Manage reservations to use a road resource during a window of time
class RoadTollbooth(road: RoadAgent) extends BatchDuringStep[RoadRequest] {
  def toll(eta: Double) = new Price(current_prices.getOrElse(idx(eta), 0.0) / road.freeflow_capacity)

  protected val cancellation_fee = 5.0
  protected val early_fee = 10.0
  protected val late_fee = 10.0

  protected val half_duration = 15 * 60.0
  // TODO serialization and such
  // value is (ETA, offer)
  protected val registrations = new mutable.HashMap[Agent, (Double, Double)]()
  // The int idx is 0 for 0-30 mins, 1 for 15-45 mins, etc
  protected val current_prices = new mutable.HashMap[Int, Double]()
  // TODO refactor the maintenance of registrations/slots and all the asserts

  // TODO this assigns 16 to 15-45, so eta better be a lower bound... desirable?
  protected def idx(eta: Double) = math.floor(eta / half_duration).toInt

  // The offer doesn't have to be past any threshold; it's up to the drivers to reroute and see high
  // new toll
  def register(a: Agent, eta: Double, offer: Double) {
    Util.assert_eq(registrations.contains(a), false)
    Util.assert_ge(offer, 0)
    add_request(RoadRequest(a, eta, offer))
  }

  def cancel(a: Agent) {
    val key = a
    Util.assert_eq(registrations.contains(key), true)
    val (eta, offer) = registrations(key)
    // TODO is it possible to cancel a request they made earlier during the tick? I think not...
    registrations -= key
    current_prices(idx(eta)) -= offer
    a.toll_broker.spend(cancellation_fee)
  }

  def enter(a: Agent) {
    val key = a
    Util.assert_eq(registrations.contains(key), true)
    val (eta, _) = registrations(key)

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
    val key = a
    Util.assert_eq(registrations.contains(key), true)
    val (eta, offer) = registrations(key)
    registrations -= key
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
    request_queue.clear()
  }

  // If the agent is rerouting and may have already registered here, don't count their own
  // contribution to the toll
  def toll_with_discount(eta: Double, a: Agent): Price = {
    val base_price = toll(eta).dollars
    val discounted_price = registrations.get(a) match {
      case Some((prev_eta, offer)) if idx(eta) == idx(prev_eta) => base_price - offer
      case _ => base_price
    }
    return new Price(discounted_price)
  }
}

case class RoadRequest(a: Agent, eta: Double, offer: Double) extends Ordered[RoadRequest] {
  override def compare(other: RoadRequest) = Ordering[Tuple2[Int, Double]].compare(
    (a.id.int, eta), (other.a.id.int, other.eta)
  )
}
