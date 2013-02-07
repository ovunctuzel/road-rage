// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket}
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
  def bid[T](choices: Iterable[T], ours: Ticket): (T, Double) = choices.head match {
    // TODO this kind of dispatch is hokey
    case _: Ticket => bid_stop_sign(choices.asInstanceOf[Iterable[Ticket]], ours).asInstanceOf[(T, Double)]
    case _: Phase => bid_signal(choices.asInstanceOf[Iterable[Phase]], ours).asInstanceOf[(T, Double)]
    case _ => throw new Exception("Dunno how to bid on " + choices)
  }
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double)
  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double)

  // May fail when the agent is bidding ahead more than one step, or when nobody
  // in their queue is ready yet.
  def relevant_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.find(t => t.turn.from == ours.turn.from)

  // TODO if there are multiple...
  def relevant_phase(phases: Iterable[Phase], ours: Ticket) =
    phases.find(p => p.has(ours.turn)).get
}

// Bids a random amount on any turn that helps the agent.
class RandomWallet(a: Agent, initial_budget: Double)
  extends Wallet(a, initial_budget)
{
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double) = {
    return relevant_stop_sign(tickets, ours) match {
      case Some(t) => (t, Util.rand_double(0.0, budget))
      case None => (tickets.head, 0.0)
    }
  }

  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double) = {
    return (relevant_phase(phases, ours), Util.rand_double(0.0, budget))
  }
}

// Always bids some high amount.
class EmergencyVehicleWallet(a: Agent, amount: Double = 1000.0)
  extends Wallet(a, Double.PositiveInfinity)
{
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): (Ticket, Double) = {
    return relevant_stop_sign(tickets, ours) match {
      case Some(t) => (t, amount)
      case None => (tickets.head, 0.0)
    }
  }

  def bid_signal(phases: Iterable[Phase], ours: Ticket): (Phase, Double) = {
    return (relevant_phase(phases, ours), amount)
  }

  // TODO Fixed high bid means multiple ambulances just compete based on how
  // many are in each flow.
  // TODO dont count payment from this towards revenue
}

// Never participate.
class FreeriderWallet(a: Agent) extends Wallet(a, 0.0) {
  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = (tickets.head, 0.0)
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = (phases.head, 0.0)
}
