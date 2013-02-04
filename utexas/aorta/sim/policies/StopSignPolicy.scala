// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // Head is the current owner.
  private var queue = List[Ticket]()

  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  def is_waiting(a: Agent)
    = (a.how_long_idle >= cfg.pause_at_stop &&
       // TODO head of their queue doesnt imply nobody's blocking them
       a.cur_queue.head.get == a &&
       a.how_far_away(intersection) <= cfg.end_threshold)

  // Add agent to the queue if they satisfy our requirements.
  def react_body() = {
    val orig_empty = queue.isEmpty
    for (ticket <- waiting_agents) {
      if (is_waiting(ticket.a)) {
        queue :+= ticket
        waiting_agents -= ticket // TODO mod while iterate?
      }
    }
    if (orig_empty && queue.nonEmpty) {
      approve_head
    }
  }

  def validate_entry(agent: Agent, turn: Turn) = queue.headOption match {
    case Some(Ticket(a, t)) => agent == a && turn == t
    case None => false
  }

  def handle_exit(a: Agent, turn: Turn) = {
    Util.assert_eq(queue.head.a, a)
    queue = queue.tail
    approve_head
  }

  def unregister_body(a: Agent) = {
    val old_owner = queue.headOption
    queue = queue.filter(_.a != a)
    if (old_owner.isDefined && old_owner.get.a == a) {
      approve_head
    }
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log("Current queue: " + queue)
  }

  private def approve_head = queue.headOption match {
    case Some(Ticket(a, _)) => a.approve_turn(intersection)
    case None =>
  }
}
