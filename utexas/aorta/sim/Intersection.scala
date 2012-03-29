// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.ListBuffer

import utexas.aorta.sim.policies._
import utexas.aorta.map.{Vertex, Turn, UberVertex, UberTurn, Edge, Traversable,
                         TurnLike}

import utexas.aorta.{Util, cfg, Stats, Intersection_Throughput_Stat}

// Common stuff goes here, particular implementations are in utexas.aorta.sim.policies

// generalization of anywhere where agents meet in conflicting ways
abstract class Junction() {
}

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex) extends Junction() {
  var policy: Policy = null   // set when one's created

  override def toString = "Intersection(" + v + ")"

  // For stats.
  var started_counting: Option[Double] = None
  var stats_requested = new MutableSet[Agent]()
  var cnt_entered = 0

  // Delegate and log.
  def unregister(a: Agent) = policy.unregister(a)
  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    stats_requested += a
    started_counting match {
      case Some(time) =>
      case None => {
        started_counting = Some(Agent.sim.tick)
        Agent.sim.schedule(
          Agent.sim.tick + cfg.thruput_stat_time, { this.count_stat }
        )
      }
    }
    return policy.can_go(a, turn, far_away)
  }

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
    cnt_entered += 1
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

  def count_stat() = {
    // TODO I'm also not convinced this is the best way to measure this idea.
    Stats.record(Intersection_Throughput_Stat(
      v.id, stats_requested.size, cnt_entered, Agent.sim.tick
    ))
    started_counting = None
    stats_requested.clear
    cnt_entered = 0
    // if there are others that have requested and not entered, they'll be
    // re-added during the next interval correctly.
  }
}

class UberSection(val v: UberVertex) extends Junction() {
  // remember what uberturn an agent is performing
  val currently = new MutableMap[Agent, UberTurn]()
  // TODO how to handle if they change their route after the first poll?

  // figure out (and then remember) what this agent wants to do
  def uberturn_of(a: Agent, start: TurnLike): UberTurn = {
    if (currently.contains(a)) {
      return currently(a)
    } else {
      val ut = find_uberturn(a, start)
      currently(a) = ut
      return ut
    }
  }

  def forget(a: Agent) = {
    currently -= a
  }

  private def find_uberturn(a: Agent, start: TurnLike): UberTurn = {
    // lookahead in their route until we are out of the ubersection

    // Detect cycles in routes by seeing duplicate turns
    val seen = new MutableSet[Turn]()
    
    // TODO move some of this work to route or use an iterator pattern?
    var idx = a.route.find_idx(start) + 1 // so this indexes an edge
    val steps = new ListBuffer[Turn]()
    var result: Option[UberTurn] = None
    while (!result.isDefined) {
      val step = a.route.lookahead_step(idx)
      if (step.isDefined) {
        val steps_turn = Traversable.toTurn(a.route.lookahead_step(idx - 1).get)
        if (seen(steps_turn)) {
          throw new Exception("Route with a cycle in an ubersection!")
        } else {
          seen += steps_turn
        }
        steps += steps_turn   // TODO here?
        val e = Traversable.toEdge(step.get)
        if (!v.verts.contains(e.to)) {
          // found a way out!
          result = Some(new UberTurn(steps.toList, v))
        } else {
          idx += 2  // skip the turn, go to the next edge
        }
      } else {
        // their route ends in the middle of us
        result = Some(new UberTurn(steps.toList, v))
      }
    }
    return result.get
  }
}

// TODO it turns out we don't seem to know junction usually...
abstract class Policy(val junction: Junction) {
  def can_go(a: Agent, turn: TurnLike, far_away: Double): Boolean
  def validate_entry(a: Agent, turn: TurnLike): Boolean
  def handle_exit(a: Agent, turn: TurnLike)
  def unregister(a: Agent)
  def current_greens(): Set[TurnLike] = Set()
  def dump_info() = {}

  // Since we lookahead over small edges, we maybe won't/can't stop on the edge
  // right before the turn. As long as we validly stopped for us, then fine.
  def is_waiting(a: Agent, t: TurnLike, far_away: Double) = far_away <= cfg.end_threshold
}

// Simplest base-line ever.
class NeverGoPolicy(junction: Junction) extends Policy(junction) {
  def can_go(a: Agent, turn: TurnLike, far_away: Double) = false
  def validate_entry(a: Agent, turn: TurnLike) = false
  def handle_exit(a: Agent, turn: TurnLike) = {}
  def unregister(a: Agent) = {}
}
