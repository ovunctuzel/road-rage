// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.map.{Turn, Edge, Vertex, Road}
import utexas.aorta.sim.{Intersection, Policy, Agent, EV_Signal_Change}

import utexas.aorta.{Util, cfg}

// A phase-based light.
class SignalPolicy(intersection: Intersection) extends Policy(intersection) {
  private var phases: Stream[Phase] = setup_phases
  // Tracks when the current phase began
  private var started_at = Agent.sim.tick
  // accumulated delay for letting vehicles finish turns
  private var delay = 0.0
  val accepted_agents = new MutableSet[Agent]

  def react() = {
    // TODO Flush out stalled slowpokes that can definitely stop and aren't
    // already in their turn? Helps prevent gridlock, that's all.

    if (in_overtime && accepted_agents.isEmpty) {
      delay += Agent.sim.tick - end_at
    }

    if (Agent.sim.tick >= end_at && accepted_agents.isEmpty) {
      // switch to the next phase
      phases = phases.tail
      started_at = Agent.sim.tick
      // TODO could account for delay and try to get back on schedule by
      // decreasing duration

      // callback for UI usually
      Agent.sim.tell_listeners(EV_Signal_Change(current_phase.turns.toSet))
    }
  }

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    if (in_overtime) {
      return accepted_agents(a)
    } else if (current_phase.has(turn) && could_make_light(a, far_away)) {
      accepted_agents += a
      return true
    } else {
      return false
    }
  }

  def validate_entry(a: Agent, turn: Turn) = accepted_agents(a)

  def handle_exit(a: Agent, turn: Turn) = unregister(a)

  def unregister(a: Agent) = {
    accepted_agents -= a
  }

  def current_greens =
    if (in_overtime)
      Set() // nobody should go right now
    else
      current_phase.turns.toSet

  def dump_info() = {
    if (in_overtime) {
      Util.log("Waiting on: " + accepted_agents)
    }
  }

  private def current_phase = phases.head
  // Ideally, ignoring overtime for slow agents.
  private def end_at = started_at + current_phase.duration
  // May be negative if we're in overtime
  private def time_left = end_at - Agent.sim.tick
  private def in_overtime = Agent.sim.tick > end_at

  private def setup_phases(): Stream[Phase] = {
    val phase_ls = Phase.phases_for(intersection)

    val turns_seen = new MutableSet[Turn]
    var expect_offset = phase_ls.head.offset
    for (phase <- phases) {
      Util.assert_eq(phase.offset, expect_offset)
      expect_offset += phase.duration
      turns_seen ++= phase.turns
    }
    Util.assert_eq(turns_seen.size, intersection.v.turns.size)

    return Stream.continually(phase_ls).flatten
  }

  // If we admit agents that run up overtime, that's safe but adds to our delay
  // and makes the light unfair.
  private def could_make_light(a: Agent, far_away: Double): Boolean = {
    // TODO choose a policy. tend to accept and let delay creep in, or ban
    // people who could've made it in order to stay on schedule?
    return true

    // if our worst-case speeding-up distance still lets us back out and stop,
    // then fine, allow it. <-- the old policy
  }
}

class Phase(val turns: Set[Turn], val offset: Double, val duration: Double) {
  Util.assert_ge(offset, 0)
  Util.assert_gt(duration, 0)
  for (t1 <- turns; t2 <- turns if t1 < t2) {
    Util.assert_eq(t1.conflicts_with(t2), false)
  }

  def has(turn: Turn) = turns(turn)
}

object Phase {
  // SignalPolicy asks us, so we can do some one-time work and dole out the
  // results or lazily compute
  def phases_for(i: Intersection): List[Phase] = {
    // for now...
    return arbitrary_phases(i.v)
    //return group_by_roads(i.v)
  }
  
  // Assign the same arbitrary duration to everything
  def turn_groups_to_phases(groups: List[Set[Turn]]): List[Phase] = {
    // TODO duration has to let agents have time to cross the intersection at a
    // reasonable speed
    val duration = 30  // TODO cfg
    var offset = 0
    return groups.map(group => {
      val phase = new Phase(group, offset, duration)
      offset += duration
      phase
    })
  }

  // Least number of phases can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach.
  def arbitrary_phases(vert: Vertex): List[Phase] = {
    // TODO needs to be sorted for determinism
    val turns_remaining = new MutableSet[Turn]()
    turns_remaining ++= vert.turns

    val groups = new ListBuffer[Set[Turn]]()
    while (turns_remaining.nonEmpty) {
      val this_group = new MutableSet[Turn]()
      this_group += turns_remaining.head
      turns_remaining -= this_group.head

      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      for (candidate <- turns_remaining) {
        if (!this_group.find(t => t.conflicts_with(t)).isDefined) {
          this_group += candidate
          turns_remaining -= candidate
        }
      }

      groups += this_group.toSet
    }
    return turn_groups_to_phases(groups.toList)
  }

  // TODO all turns from some directed roads, except turns merge into one lane
  /*def group_by_roads(vert: Vertex): List[Phase] = {
  }*/

  // TODO other groupings for standard 3/4 phase lights
}
