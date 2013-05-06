// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

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
  // Prevent more from being accepted until this ticket is approved.
  private var interruption: Option[Ticket] = None

  def accepted_conflicts(turn: Turn)
    = accepted.find(t => t.turn.conflicts_with(turn)).isDefined

  // TODO and are reasonably close? otherwise somebody who looks ahead a tick
  // earlier than another gets an advantage, even if theyre really far away
  private def candidates = request_queue.filter(ticket => !ticket.turn_blocked)

  def react(): Unit = {
    interruption match {
      case Some(ticket) => {
        // Can we admit them now?
        if (accepted_conflicts(ticket.turn)) {
          // Not yet, and don't let anyone else in either.
          return
        } else {
          // Yes! Resume admitting others now, too.
          ticket.approve()
          accepted += ticket
          interruption = None
        }
      }
      case None =>
    }

    // Approve candidates as long as there are candidates.
    while (!interruption.isDefined) {
      ordering.choose(candidates, request_queue, this) match {
        case Some(ticket) => {
          // Admit them immediately and continue, or reserve an interruption?
          if (accepted_conflicts(ticket.turn)) {
            interruption = Some(ticket)
            ticket.is_interruption = true
            unqueue(ticket)
            // Furthermore, grab a spot for them and keep it!
            ticket.turn.to.queue.allocate_slot
            return
          } else {
            accept(ticket)
          }
        }
        case None => return
      }
    }
  }

  override def dump_info() = {
    super.dump_info()
    Util.log(s"Interruption by $interruption")
  }
  def policy_type = IntersectionType.Reservation
}
