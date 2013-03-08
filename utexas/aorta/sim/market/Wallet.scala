// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, WalletType, Policy, IntersectionType,
                         Route_Event, EV_Transition, EV_Reroute, OrderingType}
import utexas.aorta.map.{Turn, Vertex}
import utexas.aorta.sim.policies.{Phase, ReservationPolicy, SignalPolicy}

import utexas.aorta.{Util, cfg}

// Express an agent's preferences of trading between time and cost.
// TODO dont require an agent, ultimately
abstract class Wallet(a: Agent, initial_budget: Double) {
  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  def spend(amount: Double, ticket: Ticket) = {
    Util.assert_ge(budget, amount)
    budget -= amount
    ticket.stat = ticket.stat.copy(cost_paid = amount + ticket.stat.cost_paid)
  }

  // How much is this agent willing to spend on some choice?
  def bid[T](choices: Iterable[T], ours: Ticket, policy: Policy): Option[(T, Double)] = policy.policy_type match {
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
    case _ => throw new Exception(s"Dunno how to bid on $policy")
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
    // Grant me the serenity to accept the things I can't change...
    v.intersection.ordering_type match {
      case OrderingType.FIFO => return 0
      case _ =>
    }

    val (big, small) = v.roads.partition(_.is_major)
    // Yes, these are arbitrary numbers.
    val policy_weight = v.intersection.policy.policy_type match {
      case IntersectionType.StopSign => 1.0
      case IntersectionType.Signal => 2.5
      case IntersectionType.Reservation => 1.5
      case IntersectionType.CommonCase => 0.0
    }
    return (1.0 * big.size) + (0.5 * small.size) + (1.0 * policy_weight)
  }
}

// Bids to maintain "fairness."
class SystemWallet() extends Wallet(null, 0.0) {
  override def toString = "SYS"
  def wallet_type = WalletType.System

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = None
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = None
  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = None

  // Infinite budget.
  override def spend(amount: Double, ticket: Ticket) = {}
}

object SystemWallets {
  // Keep these separate just for book-keeping

  val thruput = new SystemWallet()
  val thruput_bonus = 5.00

  val capacity = new SystemWallet()
  val capacity_threshold = .75
  val capacity_bonus = 2.00

  val far_away = new SystemWallet()
  val time_rate = 2.00

  val dependency = new SystemWallet()
  val dependency_rate = 0.50

  def meta_bid[T](items: List[T], policy: Policy): List[Bid[T]] =
    (bid_thruput(items, policy) ++ bid_pointless_impatience(items, policy) ++
     bid_far_away(items, policy) ++ bid_dependency(items, policy))

  // Promote bids that don't conflict
  def bid_thruput[T](items: List[T], policy: Policy) = policy match {
    case p: ReservationPolicy =>
      for (ticket <- items if !p.accepted_conflicts(ticket.asInstanceOf[Ticket].turn))
        yield Bid(thruput, ticket, thruput_bonus, null)

    case _ => Nil
  }

  // Reward individual tickets that aren't trying to rush into a queue already
  // filled to some percentage of its capacity
  def bid_pointless_impatience[T](items: List[T], policy: Policy) = policy match
  {
    // TODO maybe look at all target queues for the phase?
    case _: SignalPolicy => Nil
    case _ => items.flatMap(ticket => {
      val target = ticket.asInstanceOf[Ticket].turn.to.queue
      if (target.avail_slots / target.capacity.toDouble < capacity_threshold)
        Some(Bid(capacity, ticket, capacity_bonus, null))
      else
        None
    })
  }

  // Stop signs and signals already take this into account. Help reservations by
  // penalizing people the farther away they are from doing their turn.
  def bid_far_away[T](items: List[T], policy: Policy) = policy match {
    case _: ReservationPolicy =>
      for (ticket <- items)
        yield Bid(
          far_away, ticket,
          time_rate / ticket.asInstanceOf[Ticket].time_till_arrival, null
        )

    case _ => Nil
  }

  // Reward the lane with the most people.
  // TODO for full queues, recursing to find ALL dependency would be cool.
  def bid_dependency[T](items: List[T], policy: Policy) = policy match {
    case p: SignalPolicy =>
      for (phase <- items)
        yield Bid(dependency, phase, dependency_rate * num_phase(phase), null)
    case _ =>
      // Just bid for the head of the queue, aka, for multi-auction
      // reservations, just start things right.
      for (ticket <- items)
        yield Bid(dependency, ticket, dependency_rate * num_ticket(ticket), null)
  }
  private def num_phase(phase: Any) =
    phase.asInstanceOf[Phase].turns.map(_.from.queue.agents.size).sum
  private def num_ticket(ticket: Any): Int = {
    val t = ticket.asInstanceOf[Ticket]
    return if (t.a.cur_queue.head.get == t.a)
      t.turn.from.queue.all_in_range(0.0, t.a.at.dist).size
    else
      0
  }
}
