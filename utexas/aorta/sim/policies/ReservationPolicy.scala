// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.sim.{Intersection, Policy, Agent}
import utexas.aorta.map.Turn

import utexas.aorta.{Util, cfg}

// FIFO based on request, batched by non-conflicting turns.
// TODO make it generalizable to lots of ordering/batching/liveness rules
class ReservationPolicy(intersection: Intersection) extends Policy(intersection)
{
  private var reservations = new TurnBatch() :: Nil

  def react_body() = {
    // Process new requests
    waiting_agents.foreach(req => add_agent(req._1, req._2))
    waiting_agents = waiting_agents.empty

    // TODO flush stalled agents to avoid gridlock?

    shift_batches

    // TODO preempt reservations that perservere too long, or enforce whatever
    // load balancing strategy...
  }


  def validate_entry(a: Agent, turn: Turn) = current_batch.has_ticket(a, turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_batch.has_ticket(a, turn))
    current_batch.remove_ticket(a, turn)
    shift_batches
  }

  def current_greens = current_batch.tickets.keys.toSet

  def unregister_body(a: Agent) = {
    reservations.foreach(b => b.remove_agent(a))
  }

  def dump_info() = {
    Util.log(reservations.size + " reservations pending")
    Util.log("Currently:")
    Util.log_push
    for (t <- current_batch.tickets.keys) {
      Util.log(t + ": " + current_batch.tickets(t))
    }
    Util.log_pop
  }

  private def current_batch = reservations.head

  private def shift_batches() = {
    if (current_batch.all_done) {
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.tail.nonEmpty) {
        reservations = reservations.tail
        current_batch.agents.foreach(a => a.approve_turn(intersection))
      }
    }
  }

  private def add_agent(a: Agent, turn: Turn) = {
    if (current_batch.add_ticket(a, turn)) {
      a.approve_turn(intersection)
    } else {
      // A conflicting turn. Add it to the reservations.

      // Is there an existing batch of reservations that doesn't conflict?
      if (!reservations.find(r => r.add_ticket(a, turn)).isDefined) {
        // New batch!
        val batch = new TurnBatch()
        batch.add_ticket(a, turn)
        reservations :+= batch
      }
    }
  }
}

// Count what agents are doing each type of turn, and add turns that don't
// conflict
class TurnBatch() {
  val tickets = new MutableMap[Turn, MutableSet[Agent]] with MultiMap[Turn, Agent]

  // false if it conflicts with this group
  def add_ticket(a: Agent, t: Turn): Boolean =
    if (tickets.contains(t)) {
      // existing turn
      tickets.addBinding(t, a)
      true
    } else if (!tickets.keys.find(c => t.conflicts_with(c)).isDefined) {
      // new turn that doesn't conflict
      tickets.addBinding(t, a)
      true
    } else {
      // conflict
      false
    }

  def has_ticket(a: Agent, t: Turn) =
    tickets.contains(t) && tickets(t).contains(a)

  def remove_ticket(a: Agent, t: Turn) = tickets.removeBinding(t, a)

  def remove_agent(a: Agent) = {
    for (turn <- tickets.keys) {
      tickets.removeBinding(turn, a)
    }
  }

  def all_done = tickets.isEmpty

  def agents = tickets.values.flatten
}
