// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.{Turn, Edge}
import utexas.aorta.sim.{Intersection, Policy, Agent, Ticket, IntersectionType}
import utexas.aorta.sim.market.IntersectionOrdering

import scala.collection.mutable.{Queue => MutableQueue}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection,
                     ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  private var current_owner: Option[Ticket] = None
  // Remember the order of request
  private val queue = new MutableQueue[Ticket]()

  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  private def is_waiting(a: Agent)
    = (a.is_stopped &&
       a.cur_queue.head.get == a &&
       // TODO * 1.5 is a tmp hack
       a.how_far_away(intersection) <= 1.5 * cfg.end_threshold)

  // Add agent to the queue if they satisfy our requirements.
  def react() = {
    for (ticket <- waiting_agents) {
      queue += ticket
    }
    waiting_agents.clear
    if (!current_owner.isDefined) {
      approve_next
    }
  }

  def validate_entry(ticket: Ticket) = current_owner.getOrElse(null) == ticket

  def handle_exit(ticket: Ticket) = {
    Util.assert_eq(current_owner.get, ticket)
    current_owner = None
  }

  def approveds_to(e: Edge) = current_owner match {
    case Some(t) if t.turn.to == e => List(t)
    case _ => Nil
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log(s"Stop sign policy for $intersection")
    Util.log("Current owner: " + current_owner)
    Util.log(s"Queue: $queue")
    Util.log(s"Waiting agents: $waiting_agents")
  }
  def policy_type = IntersectionType.StopSign

  private def approve_next = {
    ordering.clear
    for (ticket <- queue if is_waiting(ticket.a) && !ticket.turn_blocked) {
      ordering.add(ticket)
    }
    current_owner = ordering.shift_next(queue, this)
    current_owner match {
      case Some(ticket) => {
        ticket.approve
        queue.dequeueFirst((t) => t == ticket)
      }
      case None =>
    }
  }
}
