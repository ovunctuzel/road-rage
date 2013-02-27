// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashSet => MutableSet}

import utexas.aorta.map.{Turn, Edge}
import utexas.aorta.sim.{Intersection, Policy, Ticket, Agent, IntersectionType}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, cfg}

// Accept as many compatible turns as possible.
class ReservationPolicy(intersection: Intersection,
                        ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  private val accepted = new MutableSet[Ticket]()
  private def accepted_agent(a: Agent) = accepted.find(t => t.a == a).isDefined
  def approveds_to(e: Edge) = accepted.filter(_.turn.to == e).map(_.a)

  // Turn can't be blocked, nobody unaccepted in front of them, have a
  // compatible turn
  private def candidates = waiting_agents.filter(ticket => {
    val c1 = !accepted.find(t => t.turn.conflicts_with(ticket.turn)).isDefined
    lazy val c2 = !turn_blocked(ticket)
    lazy val steps = ticket.a.route.steps_to(ticket.a.at.on, ticket.turn.vert)
    // TODO dont search behind us
    lazy val c3 = !steps.head.queue.all_agents.find(
      a => a.at.dist > ticket.a.at.dist && !accepted_agent(a)
    ).isDefined
    lazy val c4 = !steps.tail.find(step => step.queue.all_agents.find(
      a => !accepted_agent(a)
    ).isDefined).isDefined
    c1 && c2 && c3 && c4
  })

  def react(): Unit = {
    // Approve candidates as long as there are candidates.
    while (true) {
      // TODO should payment change for this?
      ordering.clear
      candidates.foreach(o => ordering.add(o))
      ordering.shift_next(waiting_agents, IntersectionType.Reservation) match {
        case Some(ticket) => {
          ticket.approve
          accepted += ticket
          waiting_agents -= ticket
        }
        case None => return
      }
    }
  }

  def validate_entry(a: Agent, turn: Turn) =
    accepted.find(t => t.a == a && t.turn == turn).isDefined

  def handle_exit(a: Agent, t: Turn) = {
    unregister_body(a)
  }

  def unregister_body(a: Agent) = {
    accepted.retain(t => t.a != a)
  }

  def current_greens = accepted.map(_.turn).toSet

  def dump_info() = {
    Util.log(s"Reservation policy for $intersection")
    Util.log(s"Currently accepted: $accepted")
    Util.log(s"Waiting agents: $waiting_agents")
  }
  def policy_type = IntersectionType.Reservation
}
