// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.market

import utexas.aorta.sim.{Agent, Ticket, WalletType, Policy, IntersectionType,
                         EV_Transition, EV_Reroute, OrderingType, Factory}
import utexas.aorta.map.{Turn, Vertex}
import utexas.aorta.sim.policies.{Phase, ReservationPolicy, SignalPolicy}

import utexas.aorta.common.{Util, Common, cfg, StateReader, StateWriter}

// Express an agent's preferences of trading between time and cost.
// TODO dont require an agent, ultimately
abstract class Wallet(initial_budget: Int, val priority: Int) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  protected var a: Agent = null

  // How much the agent may spend during its one-trip lifetime
  var budget = initial_budget

  // How much is this agent willing to spend on some choice?
  var tooltip: List[String] = Nil
  // Dark indicates they won and paid.
  var dark_tooltip = false

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.int(wallet_type.id)
    // TODO initial budget vs current... will it matter?
    w.int(budget)
    w.int(priority)
    w.int(tooltip.size)
    tooltip.foreach(line => w.string(line))
    w.bool(dark_tooltip)
  }

  def setup(agent: Agent) {
    a = agent
  }

  protected def unserialize(r: StateReader) {}
  
  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def spend(amount: Int, ticket: Ticket) = {
    Util.assert_ge(budget, amount)
    budget -= amount
    ticket.stat = ticket.stat.copy(cost_paid = amount + ticket.stat.cost_paid)
  }

  def reset_tooltip() = {
    tooltip = Nil
    dark_tooltip = false
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def wallet_type(): WalletType.Value

  def bid[T](choices: Iterable[T], ours: Ticket, policy: Policy): Iterable[(T, Int)] = {
    val result = policy.policy_type match {
      case IntersectionType.StopSign =>
        bid_stop_sign(
          choices.asInstanceOf[Iterable[Ticket]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case IntersectionType.Signal =>
        bid_signal(
          choices.asInstanceOf[Iterable[Phase]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case IntersectionType.Reservation =>
        bid_reservation(
          choices.asInstanceOf[Iterable[Ticket]], ours
        ).asInstanceOf[Iterable[(T, Int)]]
      case _ => throw new Exception(s"Dunno how to bid on $policy")
    }
    // TODO ideally not the latest bid, but the one for the prev turn or
    // something. also, hard to represent what we're bidding for each thing...
    // If we bid for multiple items, just show the different prices.
    tooltip = result.map(_._2.toString).toSet.toList
    return result
  }
  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket): Iterable[(Ticket, Int)]
  protected def bid_signal(phases: Iterable[Phase], ours: Ticket): Iterable[(Phase, Int)]
  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket): Iterable[(Ticket, Int)]

  // TODO tmp hack
  protected def my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    if (Wallet.tmp_bid_ahead)
      greedy_my_ticket(tickets, ours)
    else
      just_my_ticket(tickets, ours)

  // This is for just our ticket.
  protected def just_my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.filter(t => t == ours)
  // Pay for people ahead of us.
  protected def greedy_my_ticket(tickets: Iterable[Ticket], ours: Ticket) =
    tickets.filter(t => t.turn.from == ours.turn.from)
  // Return all that match, wind up just paying for one if it wins.
  protected def my_phases(phases: Iterable[Phase], ours: Ticket) =
    phases.filter(p => p.has(ours.turn))
}

object Wallet {
  // TODO this is hacky, give fair wallet a parameter
  var tmp_bid_ahead = false

  def unserialize(r: StateReader): Wallet = {
    val wallet = Factory.make_wallet(WalletType(r.int), r.int, r.int)
    val num_tooltips = r.int
    wallet.tooltip = Range(0, num_tooltips).map(_ => r.string).toList
    wallet.dark_tooltip = r.bool
    wallet.unserialize(r)
    return wallet
  }
}

// Bids a random amount on our turn.
class RandomWallet(initial_budget: Int, p: Int)
  extends Wallet(initial_budget, p)
{
  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"RND $budget"
  def wallet_type = WalletType.Random

  private def rng = a.rng

  private def bid_rnd[T](choice: Iterable[T]) =
    choice.map(thing => (thing, rng.int(0, budget)))

  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_rnd(my_ticket(tickets, ours))

  protected def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_rnd(my_phases(phases, ours))

  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_rnd(my_ticket(tickets, ours))
}

// Always bid the full budget, but never lose any money.
class StaticWallet(initial_budget: Int, p: Int)
  extends Wallet(initial_budget, p)
{
  //////////////////////////////////////////////////////////////////////////////
  // Actions

  override def spend(amount: Int, ticket: Ticket) = {
    Util.assert_ge(budget, amount)
    ticket.stat = ticket.stat.copy(cost_paid = amount)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"STATIC $budget"
  def wallet_type = WalletType.Static

  private def bid_full[T](choice: Iterable[T]) =
    choice.map(thing => (thing, budget))

  // Be greedier. We have infinite budget, so contribute to our queue.
  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(greedy_my_ticket(tickets, ours))

  protected def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_full(my_phases(phases, ours))

  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_full(greedy_my_ticket(tickets, ours))
}

// Never participate.
class FreeriderWallet(p: Int) extends Wallet(0, p) {
  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = "FR"
  def wallet_type = WalletType.Freerider

  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = Nil
  protected def bid_signal(phases: Iterable[Phase], ours: Ticket) = Nil
  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = Nil
}

// Bid once per intersection some amount proportional to the rest of the trip.
class FairWallet(initial_budget: Int, p: Int)
  extends Wallet(initial_budget, p)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  private var total_weight = 0.0

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.double(total_weight)
  }

  override protected def unserialize(r: StateReader) {
    total_weight = r.double
  }

  override def setup(agent: Agent) {
    super.setup(agent)
    a.route.listen("fair_wallet", _ match {
      case EV_Reroute(path, _) => {
        total_weight = path.map(r => weight(r.to)).sum
      }
      case EV_Transition(from, to) => to match {
        case t: Turn => {
          total_weight -= weight(t.vert)
        }
        case _ =>
      }
    })
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"FAIR $budget"
  def wallet_type = WalletType.Fair

  private def bid_fair[T](choice: Iterable[T], v: Vertex) = choice.map(
    thing => (thing, math.floor(budget * (weight(v) / total_weight)).toInt)
  )

  protected def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(greedy_my_ticket(tickets, ours), ours.turn.vert)

  protected def bid_signal(phases: Iterable[Phase], ours: Ticket) =
    bid_fair(my_phases(phases, ours), ours.turn.vert)

  protected def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) =
    bid_fair(greedy_my_ticket(tickets, ours), ours.turn.vert)

  // How much should we plan on spending, or actually spend, at this
  // intersection?
  // TODO better heuristic? base it on the policy, too!
  private def weight(v: Vertex): Double = {
    // Grant me the serenity to accept the things I can't change...
    v.intersection.ordering_type match {
      case OrderingType.FIFO => return 0.0
      case _ =>
    }

    // TODO this is untuned, so make it unweighted
    return 1.0

    /*val (big, small) = v.roads.partition(_.is_major)
    // Yes, these are arbitrary numbers.
    val policy_weight = v.intersection.policy.policy_type match {
      case IntersectionType.StopSign => 1.0
      case IntersectionType.Signal => 2.5
      case IntersectionType.Reservation => 1.5
      case IntersectionType.CommonCase => 0.0
    }
    return (1.0 * big.size) + (0.5 * small.size) + (1.0 * policy_weight)*/
  }
}

// Just used for bookkeeping... how much is some system bid affecting things?
class SystemWallet() extends Wallet(0, 0) {
  override def toString = "SYS"
  def wallet_type = WalletType.System

  def bid_stop_sign(tickets: Iterable[Ticket], ours: Ticket) = Nil
  def bid_signal(phases: Iterable[Phase], ours: Ticket) = Nil
  def bid_reservation(tickets: Iterable[Ticket], ours: Ticket) = Nil

  // Infinite budget.
  override def spend(amount: Int, ticket: Ticket) = {}
}

// Bids to maintain "fairness."
object SystemWallets {
  // TODO this singleton is a prime example of why multiple simulations per process dont work well
  // with the scala singleton pattern.

  // Keep these separate just for book-keeping
  val thruput = new SystemWallet()
  val capacity = new SystemWallet()
  val dependency = new SystemWallet()
  val waiting = new SystemWallet()
  val ready = new SystemWallet()
  private def rates = Common.scenario.system_wallet

  def meta_bid[T](items: List[T], policy: Policy): List[Bid[T]] =
    (bid_thruput(items, policy) ++ bid_pointless_impatience(items, policy) ++
     bid_dependency(items, policy) ++ bid_waiting(items, policy) ++
     bid_ready(items, policy))

  // Promote bids that don't conflict
  def bid_thruput[T](items: List[T], policy: Policy) = policy match {
    case p: ReservationPolicy =>
      for (ticket <- items if !p.accepted_conflicts(ticket.asInstanceOf[Ticket].turn))
        yield Bid(thruput, ticket, rates.thruput_bonus, null)

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
      if (target.percent_avail >= rates.avail_capacity_threshold)
        Some(Bid(capacity, ticket, rates.capacity_bonus, null))
      else
        None
    })
  }

  // Reward the lane with the most people.
  // TODO for full queues, recursing to find ALL dependency would be cool.
  def bid_dependency[T](items: List[T], policy: Policy) = policy match {
    case p: SignalPolicy =>
      for (phase <- items)
        yield Bid(dependency, phase, rates.dependency_rate * num_phase(phase), null)
    case _ =>
      for (ticket <- items)
        yield Bid(dependency, ticket, rates.dependency_rate * num_ticket(ticket), null)
  }
  private def num_phase(phase: Any) =
    phase.asInstanceOf[Phase].turns.map(_.from.queue.agents.size).sum
  /*private def num_ticket(ticket: Any) =
    ticket.asInstanceOf[Ticket].a.cur_queue.agents.size*/
  // Just bid for the head of the queue, aka, for multi-auction
  // reservations, just start things right.
  private def num_ticket(ticket: Any): Int = {
    val t = ticket.asInstanceOf[Ticket]
    return if (t.a.cur_queue.head.get == t.a)
      t.turn.from.queue.agents.size
    else
      0
  }

  // Help drivers who've been waiting the longest.
  def bid_waiting[T](items: List[T], policy: Policy) = policy match {
    case p: SignalPolicy =>
      for (phase <- items)
        yield Bid(dependency, phase, (rates.waiting_rate * waiting_phase(phase)).toInt, null)
    case _ =>
      for (ticket <- items)
        yield Bid(
          dependency, ticket,
          (rates.waiting_rate * ticket.asInstanceOf[Ticket].how_long_waiting).toInt, null
        )
  }
  private def waiting_phase(phase: Any) =
    phase.asInstanceOf[Phase].all_tickets.map(_.how_long_waiting).sum

  // Promote bids of agents close enough to usefully start the turn immediately
  def bid_ready[T](items: List[T], policy: Policy) = policy match {
    case p: ReservationPolicy =>
      for (ticket <- items if ticket.asInstanceOf[Ticket].close_to_start)
        yield Bid(ready, ticket, rates.ready_bonus, null)

    case _ => Nil
  }
}
