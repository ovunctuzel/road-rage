package utexas.sim

import utexas.map.{Vertex, Turn}

import utexas.Util

// Reason about collisions from conflicting simultaneous turns.
class Intersection(v: Vertex) {
  // TODO mix and match!
  val policy: Policy = new AlwaysStopPolicy(this)

  // Just delegate.
  def should_stop(a: Agent, turn: Turn) = policy.should_stop(a, turn)

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  var turns = Set[Turn]()

  // Check for collisions by detecting agents abnormal changes in ordering.
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight optimization
    // by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns; t2 <- turns if t1.id < t2.id) {
      if (t1.conflicts(t2)) {
        Util.log("!!! Collision1 in an intersection: " + t1 + " and " + t2)
        assert(t2.conflicts(t1))
      }
    }

    // TODO something more clever with equivalence sets, even though this isn't
    // a transitive relation?
  }

  def enter(t: Turn) = {
    turns += t
  }

  def exit(t: Turn) = {
    turns -= t
  }
}

abstract class Policy(intersection: Intersection) {
  def should_stop(a: Agent, turn: Turn): Boolean
}

// Great for testing to see if agents listen to this.
class AlwaysStopPolicy(intersection: Intersection) extends Policy(intersection) {
  def should_stop(a: Agent, turn: Turn) = true
}
