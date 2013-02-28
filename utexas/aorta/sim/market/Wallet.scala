// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, WalletType, IntersectionType}
import utexas.aorta.sim.policies.Phase

import utexas.aorta.{Util, cfg}

// Express an agent's preferences of trading between time and cost.
abstract class Wallet(a: Agent, initial_budget: Double) {
  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  def spend(amount: Double) = {
    Util.assert_ge(budget, amount)
    budget -= amount
  }

  // How much is this agent willing to spend on some choice?
  // TODO most reasonable implementations will want a notion of continuity,
  // grouping all auctions for one of its turns together...
  def bid[T](choices: Iterable[T], ours: Ticket, itype: IntersectionType.Value): Option[(T, Double)] = itype match {
    case IntersectionType.StopSign =>
      bid_stop_sign(
        choices.asInstanceOf[Iterable[Ticket]], ours
      ).asInstanceOf[Option[(T, Double)]]
    case IntersectionType.Signal =>
      bid_signal(
        choices.asInstanceOf[Iterable[Phase]], ours
      ).asInstanceOf[Option[(T, Double)]]
    case IntersectionType.Reservation =>
      bid_reservation(
        choices.asInstanceOf[Iterable[Ticket]], ours
      ).asInstanceOf[Option[(T, Double)]]
    case _ => throw new Exception(s"Dunno how to bid on $itype ordering")
  }
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): Option[(Ticket, Double)]
  def bid_signal(phases: Iterable[Phase], ours: Ticket): Option[(Phase, Double)]
  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket): Option[(Ticket, Double)]

  // TODO maybe just have default behavior of bidding for the relevant whatever

  // May fail when the agent is bidding ahead more than one step, or when nobody
  // in their queue is ready yet. TODO hmm?
  def relevant_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.find(t => t.a == ours.a)

  // TODO if there are multiple...
  def relevant_phase(phases: Iterable[Phase], ours: Ticket) =
    phases.find(p => p.has(ours.turn)).get

  def wallet_type(): WalletType.Value
}

// Bids a random amount on any turn that helps the agent.
class RandomWallet(a: Agent, initial_budget: Double)
  extends Wallet(a, initial_budget)
{
  override def toString = f"RND $budget%.2f"
  def wallet_type = WalletType.Random

  def rng = a.rng

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    relevant_ticket(tickets, ours) match {
      case Some(t) => Some((t, rng.double(0.0, budget)))
      case None => None
    }

  def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    Some(relevant_phase(phases, ours), rng.double(0.0, budget))

  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    relevant_ticket(tickets, ours) match {
      case Some(b) => Some((b, rng.double(0.0, budget)))
      case None => None
    }
}

// Always bid the full budget, but never lose any money.
class StaticWallet(a: Agent, budget: Double) extends Wallet(a, budget) {
  override def toString = "STATIC"
  def wallet_type = WalletType.Static

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    relevant_ticket(tickets, ours) match {
      case Some(t) => Some((t, budget))
      case None => None
    }

  def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    Some((relevant_phase(phases, ours), budget))

  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    relevant_ticket(tickets, ours) match {
      case Some(b) => Some((b, budget))
      case None => None
    }

  override def spend(amount: Double) = {
    Util.assert_eq(amount, budget)
  }
}

// Never participate.
class FreeriderWallet(a: Agent) extends Wallet(a, 0.0) {
  override def toString = "FR"
  def wallet_type = WalletType.Freerider

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = None
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = None
  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = None
}
