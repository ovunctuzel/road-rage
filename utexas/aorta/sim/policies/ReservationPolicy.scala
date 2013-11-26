// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Ticket, Simulation, EV_IntersectionOutcome}
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.common.{Util, StateWriter, StateReader, TurnID, Common}

// Accept as many compatible turns as possible, until an interruption occurs.
// (To get the old greedy behavior, add the constraint back to candidates, or
// reimplement it with an ordering with "inertia.")
class ReservationPolicy(intersection: Intersection,
                        ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  // Prevent more from being accepted until this ticket is approved.
  private var interruption: Option[Ticket] = None

  override def serialize(w: StateWriter) {
    super.serialize(w)
    interruption match {
      case Some(ticket) => {
        w.int(ticket.a.id.int)
        w.int(ticket.turn.id.int)
      }
      case None => {
        w.int(-1)
        w.int(-1)
      }
    }
  }

  override protected def unserialize(r: StateReader, sim: Simulation) {
    val agent_id = r.int
    val turn_id = new TurnID(r.int)
    if (agent_id != -1) {
      interruption = Some(Policy.find_ticket(sim, agent_id, turn_id))
    }
  }

  def accepted_conflicts(turn: Turn) = accepted.exists(t => t.turn.conflicts_with(turn))

  // TODO and are reasonably close? otherwise somebody who looks ahead a tick
  // earlier than another gets an advantage, even if theyre really far away
  // TODO thats why waiting a bit to accept turns makes sense.. get more people involved.
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
          Common.sim.tell_listeners(EV_IntersectionOutcome(
            policy_type, request_queue.filter(t => t.turn.conflicts(ticket.turn))
          ))
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

  override def cancel_turn(ticket: Ticket) {
    interruption match {
      case Some(t) if t == ticket => {
        interruption = None
      }
      case _ => super.cancel_turn(ticket)
    }
  }

  override def dump_info() = {
    super.dump_info()
    Util.log(s"Interruption by $interruption")
  }
  def policy_type = IntersectionType.Reservation
}
