// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.TreeSet

import utexas.aorta.sim.policies._
import utexas.aorta.map.{Vertex, Turn}

import utexas.aorta.{Util, Common, cfg}

// Common stuff goes here, particular implementations are in utexas.aorta.sim.policies

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex, policy_type: IntersectionType.Value,
                   ordering_type: OrderingType.Value)
{
  val policy = Factory.make_policy(this, policy_type, ordering_type)

  override def toString = "Intersection(" + v + ")"

  // Delegate and log.
  def unregister(a: Agent) = policy.unregister(a)
  def react = policy.react
  def request_turn(a: Agent, turn: Turn) = {
    // Sanity check...
    Util.assert_eq(turn.vert, v)
    policy.request_turn(a, turn)
  }

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  val turns = MutableMap[Turn, Int]()

  // Check for collisions
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight
    // optimization by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns.keys; t2 <- turns.keys if t1.id < t2.id) {
      if (t1.conflicts_with(t2)) {
        throw new Exception(s"Intersection collision: $t1 and $t2 conflict")
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
        Common.sim.active_intersections += this
      }

      turns(t) = 0
    }
    turns(t) += 1
    if (!policy.validate_entry(a, t)) {
      // Misleading error message. They may be going 0 speed, but the agent step
      // code hasn't finished moving them.
      Util.log("!!! %s illegally entered intersection, going %f m/s".format(a, a.speed))
      Util.log("  Illegal entry was near " + t.from + " and " + t + " (vert " + t.from.to.id + ")")
      Util.log("  Origin lane length: " + t.from.length + "; time " + Common.tick)
      Util.log("  Happened at " + Common.tick)
      sys.exit
    }
  }

  def exit(a: Agent, t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
      // Potentially we now don't care...
      if (turns.size == 1) {
        Common.sim.active_intersections -= this
      }
    }
    policy.handle_exit(a, t)
  }
}

case class Ticket(a: Agent, turn: Turn) extends Ordered[Ticket] {
  override def compare(other: Ticket) = a.compare(other.a)

  // TODO remember time requested, number of agents in front, etc
}

abstract class Policy(val intersection: Intersection) {
  // When intersections pull agents off this list, the order is arbitrary but
  // deterministic.
  protected var waiting_agents = new TreeSet[Ticket]()
  // Agents inform intersections of their intention ONCE and receive a lease
  // eventually.
  def request_turn(a: Agent, turn: Turn) = {
    synchronized {
      waiting_agents += Ticket(a, turn)
      // TODO do extra book-keeping to verify agents aren't double requesting?
    }
  }

  def unregister(a: Agent) = {
    synchronized {
      waiting_agents = waiting_agents.filter(ticket => ticket.a != a)
      unregister_body(a)
    }
  }

  // The intersection grants leases to waiting_agents
  def react(): Unit
  // TODO insist the client hands us a ticket for entry, exit?
  def validate_entry(a: Agent, turn: Turn): Boolean
  def handle_exit(a: Agent, turn: Turn)
  def unregister_body(a: Agent)
  def current_greens(): Set[Turn]
  def dump_info()
}

// Simplest base-line ever.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def react = {}
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
  def unregister_body(a: Agent) = {}
  def current_greens = Set()
  def dump_info = {
    Util.log(s"Never go policy for $intersection")
  }
}
