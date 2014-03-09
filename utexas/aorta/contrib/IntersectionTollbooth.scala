// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.map.Road
import utexas.aorta.sim.intersections.Intersection
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.common.{Util, Price, BatchDuringStep}

// Manage reservations to use some an intersection resource during a window of time
// TODO refactor with RoadTollbooth.
class IntersectionTollbooth(intersection: Intersection) extends BatchDuringStep[IntersectionRequest]
{
  private def toll(eta: Double, source: Road, dest: Road) = new Price(
    registrations.values
      .filter(r => idx(r.eta) == idx(eta) && r.conflicts.contains((source, dest)))
      .map(_.offer)
      .reduceOption(_ max _).getOrElse(0)
  )

  protected val cancellation_fee = 5.0
  protected val early_fee = 10.0
  protected val late_fee = 10.0

  protected val half_duration = 15 * 60.0
  // TODO serialization and such
  case class Registration(eta: Double, offer: Double, conflicts: Set[(Road, Road)])
  // key is (agent, source road)
  protected val registrations = new mutable.HashMap[(Agent, Road), Registration]()

  // TODO this assigns 16 to 15-45, so eta better be a lower bound... desirable?
  protected def idx(eta: Double) = math.floor(eta / half_duration).toInt

  // The offer doesn't have to be past any threshold; it's up to the drivers to reroute and see high
  // new toll
  // next_road is the next road the agent plans to use after passing through this tollbooth
  def register(a: Agent, source: Road, eta: Double, next_road: Road, offer: Double) {
    Util.assert_eq(registrations.contains((a, source)), false)
    Util.assert_ge(offer, 0)
    // Note next_road is ignored for now
    add_request(IntersectionRequest(
      a, source, next_road, eta, toll(eta, source, next_road).dollars, offer
    ))
  }

  def cancel(a: Agent, source: Road) {
    val key = (a, source)
    Util.assert_eq(registrations.contains(key), true)
    // TODO is it possible to cancel a request they made earlier during the tick? I think not...
    registrations -= key
    a.toll_broker.spend(cancellation_fee)
  }

  def enter(a: Agent, source: Road) {
    val key = (a, source)
    Util.assert_eq(registrations.contains(key), true)
    val eta = registrations(key).eta

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

  def exit(a: Agent, source: Road) {
    val key = (a, source)
    Util.assert_eq(registrations.contains(key), true)
    registrations -= key
  }

  def verify_done() {
    Util.assert_eq(registrations.isEmpty, true)
  }

  def react() {
    end_batch_step()
    for (r <- request_queue) {
      val conflicts = intersection.v.road_conflicts(r.source, r.next_road)
      registrations((r.a, r.source)) = Registration(r.eta, r.offer, conflicts)
      // What should people spend? Should it be proportional to number of conflicts?
      r.a.toll_broker.spend(r.offer)
    }
    request_queue.clear()
  }

  // If the agent is rerouting and may have already registered here, don't count their own
  // contribution to the toll
  def toll_with_discount(eta: Double, a: Agent, source: Road, dest: Road): Price = {
    val base_price = toll(eta, source, dest).dollars
    val discounted_price = registrations.get((a, source)) match {
      case Some(Registration(prev_eta, offer, _)) if idx(eta) == idx(prev_eta) =>
        (registrations - ((a, source))).values
          .filter(r => idx(r.eta) == idx(eta) && r.conflicts.contains((source, dest)))
          .map(_.offer)
          .reduceOption(_ max _).getOrElse(0.0)
      case _ => base_price
    }
    return new Price(discounted_price)
  }
}

// We, the tollbooth, set the toll at the time the request is made
case class IntersectionRequest(
  a: Agent, source: Road, next_road: Road, eta: Double, old_toll: Double, offer: Double
) extends Ordered[IntersectionRequest]
{
  override def compare(other: IntersectionRequest) = Ordering[Tuple2[Int, Double]].compare(
    (a.id.int, eta), (other.a.id.int, other.eta)
  )
}
