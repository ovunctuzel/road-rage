// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket, IntersectionType}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection,
                     ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  private def is_waiting(a: Agent)
    = (a.is_stopped &&
       a.cur_queue.head.get == a &&
       // TODO * 1.5 is a tmp hack
       a.how_far_away(intersection) <= 1.5 * cfg.end_threshold)

  // Add agent to the queue if they satisfy our requirements.
  def react() = {
    if (accepted.isEmpty) {
      approve_next()
    }
  }

  def policy_type = IntersectionType.StopSign

  private def approve_next() {
    ordering.clear
    for (ticket <- request_queue if is_waiting(ticket.a) && !ticket.turn_blocked) {
      ordering.add(ticket)
    }
    ordering.shift_next(request_queue, this) match {
      case Some(ticket) => accept(ticket)
      case None =>
    }
  }
}
