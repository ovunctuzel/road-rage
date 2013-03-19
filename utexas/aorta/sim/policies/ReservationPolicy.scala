// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.{Queue => MutableQueue}

import utexas.aorta.map.{Turn, Edge}
import utexas.aorta.sim.{Intersection, Policy, Ticket, Agent, IntersectionType}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, Common, cfg}

// Accept as many compatible turns as possible, until an interruption occurs.
// (To get the old greedy behavior, add the constraint back to candidates, or
// reimplement it with an ordering with "inertia.")
class ReservationPolicy(intersection: Intersection,
                        ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  // Remember the order of request
  // TODO refactor?
  private val queue = new MutableQueue[Ticket]()

  private val accepted = new MutableSet[Ticket]()
  def accepted_conflicts(turn: Turn)
    = accepted.find(t => t.turn.conflicts_with(turn)).isDefined
  def approveds_to(e: Edge) = accepted.filter(_.turn.to == e)
  // Prevent more from being accepted until this ticket is approved.
  private var interruption: Option[Ticket] = None

  // TODO and are reasonably close? otherwise somebody who looks ahead a tick
  // earlier than another gets an advantage, even if theyre really far away
  private def candidates = queue.filter(ticket => !ticket.turn_blocked)

  def react(): Unit = {
    for (ticket <- waiting_agents) {
      queue += ticket
    }
    waiting_agents.clear

    interruption match {
      case Some(ticket) => {
        // Can we admit them now?
        if (accepted_conflicts(ticket.turn)) {
          // Not yet, and don't let anyone else in either.
          return
        } else {
          // Yes! Resume admitting others now, too.
          ticket.approve
          accepted += ticket
          interruption = None
        }
      }
      case None =>
    }

    // Approve candidates as long as there are candidates.
    while (!interruption.isDefined) {
      ordering.clear
      candidates.foreach(o => ordering.add(o))
      ordering.shift_next(queue, this) match {
        case Some(ticket) => {
          queue.dequeueFirst((t) => t == ticket)
          // Admit them immediately and continue, or reserve an interruption?
          if (accepted_conflicts(ticket.turn)) {
            interruption = Some(ticket)
            ticket.is_interruption = true
            return
          } else {
            ticket.approve
            accepted += ticket
          }
        }
        case None => return
      }
    }
  }

  def validate_entry(ticket: Ticket) = accepted.contains(ticket)

  def handle_exit(ticket: Ticket) = {
    accepted -= ticket
    queue.dequeueFirst((t) => t == ticket)
  }

  def cancel_turn(ticket: Ticket) = {
    queue.dequeueFirst((t) => t == ticket)
  }

  def current_greens = accepted.map(_.turn).toSet

  def dump_info() = {
    Util.log(s"Reservation policy for $intersection")
    Util.log(s"Currently accepted (${accepted.size}): $accepted")
    Util.log(s"Interruption by $interruption")
    Util.log(s"Waiting agents (${waiting_agents.size}): $waiting_agents")
    Util.log(s"Queue (${queue.size}): $queue")
  }
  def policy_type = IntersectionType.Reservation
}
