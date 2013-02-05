// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket}

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

// TODO generalize to other policies, probably.
abstract class TicketOrdering() {
  def add(ticket: Ticket)
  def shift_next(waiting_participants: Iterable[Ticket]): Option[Ticket]
  def remove_agent(a: Agent)
}

class FIFO_Ordering() extends TicketOrdering() {
  private var queue = List[Ticket]()

  def add(ticket: Ticket) = {
    queue :+= ticket
  }
  def shift_next(waiting_participants: Iterable[Ticket]): Option[Ticket] = {
    if (queue.nonEmpty) {
      val next = queue.head
      queue = queue.tail
      return Some(next)
    } else {
      return None
    }
  }
  def remove_agent(a: Agent) = {
    queue = queue.filter(_.a != a)
  }
}

//class AuctionOrdering() extends TicketOrdering() {
//}
