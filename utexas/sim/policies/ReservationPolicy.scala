package utexas.sim.policies

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}

import utexas.sim.{Intersection, Policy, Agent}
import utexas.map.Turn

import utexas.Util

// FIFO based on request, batched by non-conflicting turns.  Possible liveness
// violation, since new agents can pour into the current_turns, and the ones
// that have conflicts wait indefinitely.
// If we found the optimal number of batches, that would be an instance of graph
// coloring.
class ReservationPolicy(intersection: Intersection) extends Policy(intersection) {
  var current_batch = new TurnBatch()
  var reservations = List[TurnBatch]()
  // used to determine if it's an agent's first requent or not
  val current_agents = new MutableHashSet[Agent]()

  def shift_batches() = {
    if (current_batch.all_done) {
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.size != 0) {
        current_batch = reservations.head
        reservations = reservations.tail
      }
    }
  }

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    val first_req = !current_agents.contains(a)

    // check everyone in the current batch. if any are not moving and not in their
    // turn, then cancel them -- they're almost definitely stuck behind somebody
    // who wants to do something different.
    // TODO alternative is to not schedule them until there's nobody else in front
    // trying to do something different. this is hard because agents don't call us
    // in any particular order.
    current_agents --= current_batch.flush_stalled
    shift_batches

    if (first_req) {
      current_agents += a
      if (current_batch.add_ticket(a, turn)) {
        return true
      } else {
        // A conflicting turn. Add it to the reservations.

        // Is there an existing batch of reservations that doesn't conflict?
        if (!reservations.find(r => r.add_ticket(a, turn)).isDefined) {
          // new batch!
          val batch = new TurnBatch()
          batch.add_ticket(a, turn)
          reservations :+= batch
        }

        return false
      }
    } else {
      return current_batch.has_ticket(a, turn)
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
}

// Count what agents are doing each type of turn, and add turns that don't
// conflict
// TODO maybe generalizable.
class TurnBatch() {
  val tickets = new MutableMap[Turn, MutableSet[Agent]] with MultiMap[Turn, Agent]

  // false if it conflicts with this group
  def add_ticket(a: Agent, t: Turn): Boolean = {
    if (tickets.contains(t)) {
      // existing turn
      tickets.addBinding(t, a)
      return true
    } else if (tickets.keys.filter(c => t.conflicts(c)).size == 0) {
      // new turn that doesn't conflict
      tickets.addBinding(t, a)
      return true
    } else {
      // conflict
      return false
    }
  }

  def flush_stalled(): MutableSet[Agent] = {
    val canceled = new MutableHashSet[Agent]()
    // Nix agents who haven't started turn and are not moving.
    for (t <- tickets.keys) {
      for (a <- tickets(t)) {
        if (a.at.on != t && a.speed == 0.0) {
          remove_ticket(a, t)
          canceled += a
        }
      }
    }
    return canceled
  }

  def has_ticket(a: Agent, t: Turn) = tickets.contains(t) && tickets(t).contains(a)

  def remove_ticket(a: Agent, t: Turn) = {
    tickets.removeBinding(t, a)
  }

  def all_done = tickets.isEmpty
}
