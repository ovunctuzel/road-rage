// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, WalletType}
import utexas.aorta.sim.policies.{Phase, TurnBatch}

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
  def bid[T](choices: Iterable[T], ours: Ticket): (T, Double) = choices.head match {
    // TODO this kind of dispatch is hokey
    case _: Ticket => bid_stop_sign(choices.asInstanceOf[Iterable[Ticket]], ours).asInstanceOf[(T, Double)]
    case _: Phase => bid_signal(choices.asInstanceOf[Iterable[Phase]], ours).asInstanceOf[(T, Double)]
    case _: TurnBatch => bid_reservation(choices.asInstanceOf[Iterable[TurnBatch]], ours).asInstanceOf[(T, Double)]
    case _ => throw new Exception("Dunno how to bid on " + choices)
  }
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double)
  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double)
  def bid_reservation(batches: Iterable[TurnBatch], ours: Ticket): (TurnBatch, Double)

  // TODO maybe just have default behavior of bidding for the relevant whatever

  // May fail when the agent is bidding ahead more than one step, or when nobody
  // in their queue is ready yet.
  def relevant_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.find(t => t.turn.from == ours.turn.from)

  // TODO if there are multiple...
  def relevant_phase(phases: Iterable[Phase], ours: Ticket) =
    phases.find(p => p.has(ours.turn)).get

  // TODO if there are multiple possible...
  // TODO for now, theres always some batch that will match
  def relevant_batch(batches: Iterable[TurnBatch], ours: Ticket) =
    batches.find(b => b.has_ticket(ours.a, ours.turn))

  def wallet_type(): WalletType.Value
}

// Bids a random amount on any turn that helps the agent.
class RandomWallet(a: Agent, initial_budget: Double)
  extends Wallet(a, initial_budget)
{
  override def toString = f"RND $budget%.2f"
  def wallet_type = WalletType.Random

  def rng = a.rng

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double) = {
    return relevant_stop_sign(tickets, ours) match {
      case Some(t) => (t, rng.double(0.0, budget))
      case None => (tickets.head, 0.0)
    }
  }

  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double) = {
    return (relevant_phase(phases, ours), rng.double(0.0, budget))
  }

  def bid_reservation(batches: Iterable[TurnBatch], ours: Ticket): (TurnBatch, Double) = {
    return relevant_batch(batches, ours) match {
      case Some(b) => (b, rng.double(0.0, budget))
      case None => (batches.head, 0.0)
    }
  }
}

// Always bids some high amount.
class EmergencyVehicleWallet(a: Agent, amount: Double = 1000.0)
  extends Wallet(a, Double.PositiveInfinity)
{
  override def toString = "EMERG"
  def wallet_type = WalletType.Emergency

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double) = {
    return relevant_stop_sign(tickets, ours) match {
      case Some(t) => (t, amount)
      case None => (tickets.head, 0.0)
    }
  }

  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double) = {
    return (relevant_phase(phases, ours), amount)
  }

  def bid_reservation(batches: Iterable[TurnBatch], ours: Ticket): (TurnBatch, Double) = {
    return relevant_batch(batches, ours) match {
      case Some(b) => (b, amount)
      case None => (batches.head, 0.0)
    }
  }

  // TODO Fixed high bid means multiple ambulances just compete based on how
  // many are in each flow.
  // TODO dont count payment from this towards revenue
}

// Never participate.
class FreeriderWallet(a: Agent) extends Wallet(a, 0.0) {
  override def toString = "FR"
  def wallet_type = WalletType.Freerider

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = (tickets.head, 0.0)
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = (phases.head, 0.0)
  def bid_reservation(batches: Iterable[TurnBatch], ours: Ticket) = (batches.head, 0.0)
}
