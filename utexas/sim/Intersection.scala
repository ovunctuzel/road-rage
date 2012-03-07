package utexas.sim

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.sim.policies._
import utexas.map.{Vertex, Turn}

import utexas.{Util, cfg}

// Common stuff goes here, particular implementations are in utexas.sim.policies

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex) {
  //val policy: Policy = new NeverGoPolicy(this)
  //val policy: Policy = new StopSignPolicy(this)
  //val policy: Policy = new SignalCyclePolicy(this)
  val policy: Policy = new ReservationPolicy(this)

  override def toString = "Intersection(" + v + ")"

  // Just delegate. TODO or make caller do that themselves.
  def can_go(a: Agent, turn: Turn, far_away: Double) = policy.can_go(a, turn, far_away)
  def unregister(a: Agent) = policy.unregister(a)

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  val turns = MutableMap[Turn, Int]()

  // Check for collisions by detecting agents abnormal changes in ordering.
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight optimization
    // by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns.keys; t2 <- turns.keys if t1.id < t2.id) {
      if (t1.conflicts(t2)) {
        Util.log("!!! Collision in an intersection: " + t1 + " and " + t2)
        assert(t2.conflicts(t1))
      }
    }

    // TODO something more clever with equivalence sets, even though this isn't
    // a transitive relation?
  }

  def enter(a: Agent, t: Turn) = {
    if (!turns.contains(t)) {
      // We don't care until there are at least two... and this only changes when
      // we add a new one...
      // It's not really that much better to do the checking in-place and only
      // atomic-check when there could be a problem.
      if (turns.size == 1) {
        Agent.sim.active_intersections += this
      }

      turns(t) = 0
    }
    turns(t) += 1
    if (!policy.validate_entry(a, t)) {
      Util.log("!!! " + a + " illegally entered intersection, going at " + a.speed)
      Util.log("  Incident was near " + t.from + " and " + t + " (vert " + t.from.to.id + ")")
      Util.log("  Origin lane length: " + t.from.length + "; time " + Agent.sim.tick)
      Util.log("  Happened at " + Agent.sim.tick)
      sys.exit
    }
  }

  def exit(a: Agent, t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
      // Potentially we now don't care...
      if (turns.size == 1) {
        Agent.sim.active_intersections -= this
      }
    }
    policy.handle_exit(a, t)
  }
}

abstract class Policy(intersection: Intersection) {
  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean
  def validate_entry(a: Agent, turn: Turn): Boolean
  def handle_exit(a: Agent, turn: Turn)
  def unregister(a: Agent)
  def current_greens(): Set[Turn] = Set()
  def dump_info() = {}

  // Since we lookahead over small edges, we maybe won't/can't stop on the edge
  // right before the turn. As long as we validly stopped for us, then fine.
  def is_waiting(a: Agent, t: Turn, far_away: Double) = far_away <= cfg.end_threshold
}

// Simplest base-line ever.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def can_go(a: Agent, turn: Turn, far_away: Double) = false
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
  def unregister(a: Agent) = {}
}
