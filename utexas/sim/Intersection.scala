package utexas.sim

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.map.{Vertex, Turn}

import utexas.Util

// Reason about collisions from conflicting simultaneous turns.
class Intersection(v: Vertex) {
  // TODO mix and match!
  val policy: Policy = new NeverGoPolicy(this)

  // Just delegate.
  def should_stop(a: Agent, turn: Turn) = policy.should_stop(a, turn)

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
      Util.log("!!! Agent illegally entered intersection")
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
  def should_stop(a: Agent, turn: Turn): Boolean

  def validate_entry(a: Agent, turn: Turn): Boolean
  def handle_exit(a: Agent, turn: Turn)
}

// Great for testing to see if agents listen to this.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def should_stop(a: Agent, turn: Turn) = true
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
}

// Always stop, then FIFO. Totally unoptimized.
// TODO untested.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // owner of the intersection!
  var current_owner: Option[Agent] = None
  val queue = new List[Agent]

  def should_stop(a: Agent, turn: Turn): Boolean = {
    current_owner match {
      case Some(a) => return true
    }

    // Already scheduled?
    if (queue.contains(a)) {
      return false
    }

    // Do we add them to the queue? They have to be at the end.
    val can_add_to_queue = a.at.dist_left == 0.0  // TODO < epsilon

    if (can_add_to_queue) {
      queue += a
      // And if they're the first in the queue, immediately promote them and let
      // them go.
      if (queue.size == 1) {
        return true
      }
    }

    return false
  }

  def validate_entry(a: Agent, turn: Turn) = current_owner.get == a

  def handle_exit(a: Agent, turn: Turn) = {
    assert(a == current_owner.get)
    if (queue.size > 0) {
      current_owner = Some(queue.head)
      queue = queue.tail
    } else {
      current_owner = None
    }
  }
}
