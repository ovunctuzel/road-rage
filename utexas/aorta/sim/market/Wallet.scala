// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, WalletType, IntersectionType,
                         Route_Event, EV_Transition, EV_Reroute}
import utexas.aorta.map.{Turn, Vertex}
import utexas.aorta.sim.policies.Phase
import utexas.aorta.analysis.{Stats, Turn_Pay_Stat}

import utexas.aorta.{Util, cfg}

// Express an agent's preferences of trading between time and cost.
abstract class Wallet(a: Agent, initial_budget: Double) {
  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  def spend(amount: Double, ticket: Ticket) = {
    Util.assert_ge(budget, amount)
    budget -= amount
    ticket.stat = ticket.stat.copy(cost_paid = amount)
  }

  // How much is this agent willing to spend on some choice?
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

  def my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.find(t => t.a == ours.a)

  // TODO if there are multiple... then what?
  def my_phase(phases: Iterable[Phase], ours: Ticket) =
    phases.find(p => p.has(ours.turn))

  def wallet_type(): WalletType.Value
}

// Bids a random amount on our turn.
class RandomWallet(a: Agent, initial_budget: Double)
  extends Wallet(a, initial_budget)
{
  override def toString = f"RND $budget%.2f"
  def wallet_type = WalletType.Random

  def rng = a.rng

  private def bid_rnd[T](choice: Option[T]): Option[(T, Double)] = choice match
  {
    case Some(thing) => Some((thing, rng.double(0.0, budget)))
    case None => None
  }

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_rnd(my_ticket(tickets, ours))

  def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_rnd(my_phase(phases, ours))

  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_rnd(my_ticket(tickets, ours))
}

// Always bid the full budget, but never lose any money.
class StaticWallet(a: Agent, budget: Double) extends Wallet(a, budget) {
  override def toString = "STATIC"
  def wallet_type = WalletType.Static

  private def bid_full[T](choice: Option[T]): Option[(T, Double)] = choice match
  {
    case Some(thing) => Some((thing, budget))
    case None => None
  }

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(my_ticket(tickets, ours))

  def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_full(my_phase(phases, ours))

  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(my_ticket(tickets, ours))

  override def spend(amount: Double, ticket: Ticket) = {
    Util.assert_ge(budget, amount)
    ticket.stat = ticket.stat.copy(cost_paid = amount)
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

// Bid once per intersection some amount proportional to the rest of the trip.
class FairWallet(a: Agent, budget: Double) extends Wallet(a, budget) {
  override def toString = "FAIR"
  def wallet_type = WalletType.Fair

  private var total_weight = 0.0

  a.route.listen("fair_wallet", (ev: Route_Event) => { ev match {
    case EV_Reroute(path) => {
      total_weight = path.map(r => weight(r.to)).sum
    }
    case EV_Transition(from, to) => to match {
      case t: Turn => {
        total_weight -= weight(t.vert)
      }
      case _ =>
    }
  } })

  private def bid_fair[T](choice: Option[T], v: Vertex): Option[(T, Double)] = choice match
  {
    case Some(thing) => Some((thing, budget * (weight(v) / total_weight)))
    case None => None
  }

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(my_ticket(tickets, ours), ours.turn.vert)

  def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_fair(my_phase(phases, ours), ours.turn.vert)

  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(my_ticket(tickets, ours), ours.turn.vert)

  // How much should we plan on spending, or actually spend, at this
  // intersection?
  // TODO better heuristic? base it on the policy, too!
  private def weight(v: Vertex): Double = {
    val (big, small) = v.roads.partition(_.is_major)
    val policy_weight = v.intersection.policy.policy_type match {
      case IntersectionType.StopSign => 1.0
      case IntersectionType.Signal => 2.5
      case IntersectionType.Reservation => 1.5
      case IntersectionType.CommonCase => 0.0
    }
    return (1.0 * big.size) + (0.5 * small.size) + (1.0 * policy_weight)
  }
}
