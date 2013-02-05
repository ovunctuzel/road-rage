// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket}

import utexas.aorta.{Util, cfg}

// Express an agent's preferences of trading between time and cost.
abstract class Wallet(a: Agent, initial_budget: Double) {
  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  def spend(amount: Double) = {
    Util.assert_ge(budget, amount)
    budget -= amount
  }

  // How much is this agent willing to spend on some choice of tickets?
  def bid_stop_sign(tickets: Set[Ticket], ours: Ticket): (Ticket, Double)
  // TODO most reasonable implementations will want a notion of continuity,
  // grouping all auctions for one of its turns together...

  // TODO or perhaps the auction should determine this for us and only ask for
  // our bid on the one ticket that makes sense?
  def relevant_ticket(tickets: Set[Ticket], ours: Ticket) =
    tickets.find(t => t.turn.from == ours.turn.from).get
}

// Bids a random amount on any turn that helps the agent.
class RandomWallet(a: Agent, initial_budget: Double)
  extends Wallet(a, initial_budget)
{
  def bid_stop_sign(tickets: Set[Ticket], yours: Ticket): (Ticket, Double) = {
    return (relevant_ticket(tickets, yours), Util.rand_double(0.0, budget))
  }
}
