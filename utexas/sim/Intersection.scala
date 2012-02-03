package utexas.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.ListBuffer

import utexas.map.{Vertex, Turn}

import utexas.Util

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex) {
  //val policy: Policy = new NeverGoPolicy(this)
  val policy: Policy = new StopSignPolicy(this)
  //val policy: Policy = new ReservationPolicy(this)
  //val policy: Policy = new SignalCyclePolicy(this)

  // Just delegate.
  def should_stop(a: Agent, turn: Turn, first_req: Boolean) = policy.should_stop(a, turn, first_req)

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  var turns = MutableMap[Turn, Int]()

  // Check for collisions by detecting agents abnormal changes in ordering.
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight optimization
    // by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns.keys; t2 <- turns.keys if t1.id < t2.id) {
      if (t1.conflicts(t2)) {
        Util.log("!!! Collision1 in an intersection: " + t1 + " and " + t2)
        assert(t2.conflicts(t1))
      }
    }

    // TODO something more clever with equivalence sets, even though this isn't
    // a transitive relation?
  }

  def enter(a: Agent, t: Turn) = {
    if (!turns.contains(t)) {
      turns(t) = 0
    }
    turns(t) += 1
    if (!policy.validate_entry(a, t)) {
      Util.log("!!! Agent illegally entered intersection, going at " + a.speed)
      Util.log("  Incident was near " + t.from + " and " + t.to)
      Util.log("  Origin lane length: " + t.from.length)
    }
  }

  def exit(a: Agent, t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
    }
    policy.handle_exit(a, t)
  }
}

abstract class Policy(intersection: Intersection) {
  def should_stop(a: Agent, turn: Turn, first_req: Boolean): Boolean
  def validate_entry(a: Agent, turn: Turn): Boolean
  def handle_exit(a: Agent, turn: Turn)
}

// Great for testing to see if agents listen to this.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def should_stop(a: Agent, turn: Turn, first_req: Boolean) = true
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // owner of the intersection!
  var current_owner: Option[Agent] = None
  var queue = List[Agent]()

  def should_stop(a: Agent, turn: Turn, first_req: Boolean): Boolean = {
    current_owner match {
      case Some(owner) if a == owner => return false
      case _       =>
    }

    // Already scheduled?
    if (queue.contains(a)) {
      return true
    }

    // Do we add them to the queue? They have to be at the end.
    val can_add_to_queue = a.at_end_of_edge

    if (can_add_to_queue) {
      // If they're the first one, let them go now. They've stopped validly.
      if (queue.size == 0 && !current_owner.isDefined) {
        current_owner = Some(a)
        return false
      } else {
        queue :+= a
      }
    }

    return true
  }

  def validate_entry(a: Agent, turn: Turn) = current_owner match {
    case Some(a) => true
    case _       => false
  }

  def handle_exit(a: Agent, turn: Turn) = {
    assert(a == current_owner.get)
    /*if (current_owner.get != a) {
      Util.log(a + " is leaving, but current owner is " + current_owner.get)
      Util.log("Crazy guy was attempting " + turn)
      Util.log("Time was " + Agent.sim.tick)
    }*/
    if (queue.size > 0) {
      current_owner = Some(queue.head)
      queue = queue.tail
    } else {
      current_owner = None
    }
  }
}

// FIFO based on request, batched by non-conflicting turns.
// Possible deadlock, since new agents can pour into the current_turns, and the
// ones that have conflicts wait indefinitely.
// If we found the optimal number of batches, that would be an instance of graph
// coloring.
class ReservationPolicy(intersection: Intersection) extends Policy(intersection) {
  var current_batch = new TurnBatch()
  var reservations = List[TurnBatch]()

  def should_stop(a: Agent, turn: Turn, first_req: Boolean): Boolean = {
    if (first_req) {
      if (current_batch.add_turn(turn)) {
        return false
      } else {
        // A conflicting turn. Add it to the reservations.

        // Is there an existing batch of reservations that doesn't conflict?
        val batch = reservations.find(r => r.add_turn(turn))
        if (!batch.isDefined) {
          // new batch!
          val b = new TurnBatch()
          b.add_turn(turn)
          reservations :+= b
        }

        return true
      }
    } else {
      return !current_batch.has_turn(turn)
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_batch.has_turn(turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_batch.has_turn(turn))
    current_batch.remove_turn(turn)
    if (current_batch.all_done) {
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.size != 0) {
        current_batch = reservations.head
        reservations = reservations.tail
      }
    }
  }
}

// Count how many agents are doing each type of turn, and add turns that don't
// conflict
class TurnBatch() {
  val turns = new MutableMap[Turn, Int]

  // false if it conflicts with this group
  def add_turn(t: Turn): Boolean = {
    if (turns.contains(t)) {
      // existing turn
      turns(t) += 1
      return true
    } else if (turns.keys.filter(c => t.conflicts(c)).size == 0) {
      // new turn that doesn't conflict
      turns(t) = 1
      return true
    } else {
      // conflict
      return false
    }
  }

  def has_turn(t: Turn) = turns.contains(t)

  def remove_turn(t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
    }
  }

  def all_done = turns.size == 0
}

// A cycle-based light.
class SignalCyclePolicy(intersection: Intersection) extends Policy(intersection) {
  // Assign turns to each cycle
  val cycles: List[Set[Turn]] = find_cycles
  // And have a fixed duration for all of them for now
  val duration = 20.0   // TODO cfg
  
  // note that toInt is floor
  def current_cycle = cycles((Agent.sim.tick / duration).toInt % cycles.size)

  // Least number of cycles can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach.
  def find_cycles(): List[Set[Turn]] = {
    var turns_left = intersection.v.turns.toSet
    val cycle_list = new ListBuffer[Set[Turn]]()
    while (!turns_left.isEmpty) {
      val canonical = turns_left.head
      // TODO conflict relation is symmetric, but not transitive... just because
      // it's fine with the canonical, doesn't mean it's fine with all of em
      turns_left.partition(t => canonical.conflicts(t)) match {
        case (more_left, this_group) => {
          assert(this_group(canonical))
          cycle_list += this_group
          turns_left = more_left
        }
      }
    }
    return cycle_list.toList
  }

  // TODO need to deal with "yellow lights" -- tell an agent to stop early
  // enough.
  def should_stop(a: Agent, turn: Turn, first_req: Boolean) = !current_cycle(turn)

  def validate_entry(a: Agent, turn: Turn) = current_cycle(turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_cycle(turn))
  }
}
