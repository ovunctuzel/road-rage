// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket}
import utexas.aorta.sim.market.IntersectionOrdering

import scala.collection.mutable.{HashMap, MultiMap}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection,
                     ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  private var current_owner: Option[Ticket] = None

  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  private def is_waiting(a: Agent)
    = (a.how_long_idle >= cfg.pause_at_stop &&
       // TODO head of their queue doesnt imply nobody's blocking them
       a.cur_queue.head.get == a &&
       a.how_far_away(intersection) <= cfg.end_threshold)

  // Add agent to the queue if they satisfy our requirements.
  def react() = {
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
    ordering.queue = ordering.queue.filter(_.a != a)
    if (current_owner.isDefined && current_owner.get.a == a) {
      approve_next
    }
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log(s"Stop sign policy for $intersection")
    Util.log("Current owner: " + current_owner)
  }

  private def approve_next = {
    current_owner = ordering.shift_next(waiting_agents ++ ordering.queue)
    if (current_owner.isDefined) {
      current_owner.get.a.approve_turn(intersection)
    }
  }
}
