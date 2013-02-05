// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket}

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  private var current_owner: Option[Ticket] = None
  // TODO with an auction or not?
  private val ordering = new FIFO_Ordering()

  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  def is_waiting(a: Agent)
    = (a.how_long_idle >= cfg.pause_at_stop &&
       // TODO head of their queue doesnt imply nobody's blocking them
       a.cur_queue.head.get == a &&
       a.how_far_away(intersection) <= cfg.end_threshold)

  // Add agent to the queue if they satisfy our requirements.
  def react_body() = {
    for (ticket <- waiting_agents) {
      if (is_waiting(ticket.a)) {
        ordering.add(ticket)
        waiting_agents -= ticket // TODO mod while iterate?
      }
    }
    if (!current_owner.isDefined) {
      approve_next
    }
  }

  def validate_entry(agent: Agent, turn: Turn) = current_owner match {
    case Some(Ticket(a, t)) => agent == a && turn == t
    case None => false
  }

  def handle_exit(a: Agent, turn: Turn) = {
    Util.assert_eq(current_owner.get.a, a)
    approve_next
  }

  def unregister_body(a: Agent) = {
    ordering.remove_agent(a)
    if (current_owner.isDefined && current_owner.get.a == a) {
      approve_next
    }
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log("Current owner: " + current_owner)
  }

  private def approve_next = {
    current_owner = ordering.shift_next(waiting_agents)
    if (current_owner.isDefined) {
      current_owner.get.a.approve_turn(intersection)
    }
  }
}

// TODO generalize to other policies, probably by generalizing ticket.
abstract class TicketOrdering() {
  protected var queue = List[Ticket]()

  def shift_next(waiting_participants: Iterable[Ticket]): Option[Ticket]

  def add(ticket: Ticket) = {
    queue :+= ticket
  }
  def remove_agent(a: Agent) = {
    queue = queue.filter(_.a != a)
  }
}

class FIFO_Ordering() extends TicketOrdering() {
  def shift_next(waiting_participants: Iterable[Ticket]): Option[Ticket] = {
    if (queue.nonEmpty) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      return None
    }
  }
}

// Who's willing to pay what
case class Bid(who: Ticket, amount: Double)

class AuctionOrdering() extends TicketOrdering() {
  def shift_next(waiting_participants: Iterable[Ticket]): Option[Ticket] = {
    // Handle degenerate cases where we don't hold auctions.
    if (queue.isEmpty) {
      return None
    }
    if (queue.size == 1) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    }

    // Collect bids, remembering what each agent bids, and group by ticket.
    val bids = new HashMap[Ticket, MutableSet[Bid]] with MultiMap[Ticket, Bid]
    (waiting_participants ++ queue).foreach(who => {
      who.a.wallet.bid_stop_sign(queue, who) match {
        case (ticket, amount) if amount > 0.0 => {
          bids.addBinding(ticket, Bid(who, amount))
        }
        case _ =>
      }
    })

    if (bids.isEmpty) {
      // They're all apathetic, so just do FIFO.
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      val winner = bids.keys.map(t => (bids(t).map(_.amount).sum, t)).max
      collect_payment(winner._1, bids(winner._2))
      queue = queue.filter(_ != winner._2)
      return Some(winner._2)
    }
  }

  private def collect_payment(total: Double, participants: Iterable[Bid]) = {
    // TODO track revenue.
    // TODO Simple division for now -- make them pay their full bid.
    participants.foreach(bid => bid.who.a.wallet.spend(bid.amount))
  }
}
