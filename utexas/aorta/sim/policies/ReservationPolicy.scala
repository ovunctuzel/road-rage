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
  private var lock_cur_batch = false    // because we're trying to preempt
  private val accepted_agents = new MutableSet[Agent]()
  // When did the first group of reservations start waiting?
  private var others_started_waiting = -1.0

  def react() = {
    // TODO flush stalled agents to avoid gridlock?


  }

  def shift_batches() = {
    if (current_batch.all_done) {
      lock_cur_batch = false
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.size != 0) {
        current_batch = reservations.head
        reservations = reservations.tail
        if (reservations.nonEmpty) {
          // If we had a previous preempt notification on the way, just ignore
          // it when it arrives
          dont_be_greedy
        }
      }
    }
  }

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    val first_req = !current_agents.contains(a)

    // check everyone in the current batch. if any are not moving and not in
    // their turn, then cancel them -- they're almost definitely stuck behind
    // somebody who wants to do something different.
    current_agents --= current_batch.flush_stalled
    shift_batches

    return if (first_req) {
      current_agents += a
      if ((!lock_cur_batch) && current_batch.add_ticket(a, turn)) {
        true
      } else {
        // A conflicting turn. Add it to the reservations.

        // Is there an existing batch of reservations that doesn't conflict?
        if (!reservations.find(r => r.add_ticket(a, turn)).isDefined) {
          // new batch!
          val batch = new TurnBatch()
          batch.add_ticket(a, turn)
          // Make sure these guys don't wait too long. Schedule the event system
          // to poke us after a certain delay.
          if (reservations.isEmpty) {
            dont_be_greedy
          }
          reservations :+= batch
        }

        false
      }
    } else {
      current_batch.has_ticket(a, turn)
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_batch.has_ticket(a, turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_batch.has_ticket(a, turn))
    current_batch.remove_ticket(a, turn)
    current_agents -= a
    shift_batches
  }

  override def current_greens = current_batch.tickets.keys.toSet

  def unregister(a: Agent) = {
    if (current_agents.contains(a)) {
      // TODO what turn do they want to do? nuke-agent doesn't work till we get
      // this right.
    }
  }

  override def dump_info() = {
    Util.log(reservations.size + " reservations pending")
    Util.log("Currently:")
    Util.log_push
    for (t <- current_batch.tickets.keys) {
      Util.log(t + ": " + current_batch.tickets(t))
    }
    Util.log_pop
  }

  def dont_be_greedy() = {
    // TODO swap based on recursive dependency count
    others_started_waiting = Agent.sim.tick
    // TODO there are similarities with signal cycle policy. perhaps refactor.
    Agent.sim.schedule(Agent.sim.tick + cfg.signal_duration, { this.preempt })
  }

  // Delay the current cycle, they're hogging unfairly
  def preempt() = {
    if (Agent.sim.tick - others_started_waiting >= cfg.signal_duration
        && reservations.nonEmpty && lock_cur_batch == false)
    {
      // it's quite the edge case when there are no current_agents... because
      // then the intersection would have shifted to the next anyway.
      assert(!current_batch.all_done)

      // cant do it if there are some that can't stop. aka, just lock current
      // cycle.
      lock_cur_batch = true
      //Util.log("locking " + intersection + " for preemption")
      
      // then all the other handling is normal, just don't let agents enter
      // current_batch
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

  def all_done = tickets.isEmpty
}
