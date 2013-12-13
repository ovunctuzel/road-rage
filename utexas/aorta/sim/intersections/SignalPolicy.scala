// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import scala.collection.mutable

import utexas.aorta.map.{Turn, Vertex, Edge}
import utexas.aorta.sim.{EV_Signal_Change, Simulation, EV_IntersectionOutcome}
import utexas.aorta.sim.make.{IntersectionType, OrderingType}

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader}

// A phase-based light.
class SignalPolicy(
  intersection: Intersection, ordering: IntersectionOrdering[Phase], sim: Simulation
) extends Policy(intersection)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  // phase_order maintains the list of all possible phases, in order of LRU
  var phase_order = new mutable.ListBuffer[Phase]()
  phase_order ++= setup_phases
  private var current_phase = phase_order.head
  phase_order = phase_order.tail ++ List(current_phase)

  // Tracks when the current phase began
  private var started_at = sim.tick

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.double(started_at)
    phase_order.foreach(p => w.int(p.id))
  }

  override protected def unserialize(r: StateReader, sim: Simulation) {
    started_at = r.double
    // Learn our phases
    val phases = phase_order.map(p => p.id -> p).toMap
    phase_order.clear()
    phase_order ++= Range(0, phases.size).map(_ => phases(r.int))
    current_phase = phase_order.last
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def react() {
    // Switch to the next phase
    if (sim.tick >= end_at && accepted.isEmpty) {
      // In auctions, we may not have a viable next phase at all...
      ordering.choose(candidates, request_queue, this) match {
        case Some(p) => {
          sim.publish(
            EV_IntersectionOutcome(policy_type, request_queue.filter(t => !p.has(t.turn)))
          )
          current_phase = p
          phase_order = phase_order.filter(phase => phase != p)
          phase_order += p

          started_at = sim.tick

          sim.publish(EV_Signal_Change(current_phase.turns.toSet))
        }
        case None =>  // shouldn't happen...
      }
    }

    // Accept new agents into the current phase
    if (!in_overtime) {
      // Because we have to maintain turn invariants as we accept, do a fixpoint
      // approach and accept till there's nobody left that we can.
      val candidates_now = new mutable.TreeSet[Ticket]()
      candidates_now ++= request_queue.filter(
        ticket => current_phase.has(ticket.turn) && could_make_light(ticket)
      )
      var changed = true
      while (changed && candidates_now.nonEmpty) {
        changed = false
        for (ticket <- candidates_now) {
          if (!ticket.turn_blocked) {
            accept(ticket)
            candidates_now -= ticket
            changed = true
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def policy_type = IntersectionType.Signal
  override def dump_info() {
    super.dump_info()
    Util.log(s"Current phase: $current_phase")
    Util.log(s"${phase_order.size} phases total")
    Util.log("Time left: " + time_left)
    Util.log("Viable phases right now: " + candidates)
  }

  // If we're holding auctions, we risk agents repeatedly bidding for and
  // winning a phase that doesn't have any agents that can actually go yet.
  // Avoid that. In the FIFO case, pretend we're not autonomous and pick useless
  // phases.
  def candidates =
    if (ordering.ordering_type == OrderingType.FIFO)
      phase_order
    else
      phase_order.filter(p => p.all_tickets.exists(t => !t.turn_blocked))

  override def current_greens =
    if (in_overtime)
      Set() // nobody should go right now
    else
      current_phase.turns.toSet

  // Ideally, ignoring overtime for slow agents.
  private def end_at = started_at + current_phase.duration
  // May be negative if we're in overtime
  private def time_left = end_at - sim.tick
  private def in_overtime = sim.tick > end_at

  private def setup_phases(): List[Phase] = {
    val phase_ls = Phase.phases_for(intersection)

    val turns_seen = new mutable.HashSet[Turn]
    var expect_offset = phase_ls.head.offset
    for (phase <- phase_ls) {
      Util.assert_eq(phase.offset, expect_offset)
      expect_offset += phase.duration
      turns_seen ++= phase.turns
    }
    Util.assert_eq(turns_seen.size, intersection.v.turns.size)

    return phase_ls
  }

  // If we admit agents that run up overtime, that's safe but adds to our delay
  // and makes the light unfair.
  private def could_make_light(ticket: Ticket): Boolean = {
    // TODO choose a policy. tend to accept and let delay creep in, or ban
    // people who could've made it in order to stay on schedule?

    // No control, admit everyone?
    //return true

    // Simple heuristic: can we make it in time at the speed limit?
    return ticket.eta_earliest < time_left

    // if our worst-case speeding-up distance still lets us back out and stop,
    // then fine, allow it. <-- the old policy
  }
}

class Phase(val turns: Set[Turn], val offset: Double, val duration: Double)
  extends Ordered[Phase]
{
  val id = Phase.id
  Phase.id += 1

  // Can only order phases at one intersection! Offsets must be unique!
  override def compare(other: Phase) = offset.compare(other.offset)
  //override def toString = s"Phase($id, $turns, offset $offset, duration $duration)"
  override def toString = s"Phase $id"

  Util.assert_ge(offset, 0)
  Util.assert_gt(duration, 0)
  for (t1 <- turns; t2 <- turns if t1 < t2) {
    Util.assert_eq(t1.conflicts_with(t2), false)
  }

  def has(turn: Turn) = turns(turn)

  def all_agents = turns.flatMap(_.from.queue.all_agents)
  def all_tickets: Set[Ticket] = {
    val i = turns.head.vert.intersection
    return all_agents.flatMap(a => a.all_tickets(i)).filter(t => has(t.turn))
  }
}

object Phase {
  var id = 0

  def phases_for(i: Intersection): List[Phase] = {
    return arbitrary_phases(i.v)
    //return group_by_roads(i.v)
  }
  
  // Assign the same arbitrary duration to everything
  def turn_groups_to_phases(groups: List[Set[Turn]]): List[Phase] = {
    // TODO duration has to let agents have time to cross the intersection at a
    // reasonable speed
    var offset = 0
    return groups.map(group => {
      val phase = new Phase(group, offset, cfg.signal_duration)
      offset += cfg.signal_duration
      phase
    })
  }

  // Add turns to every group that don't conflict
  def maximize_groups(groups: List[Set[Turn]], turns: List[Turn]) =
    groups.map(g => maximize(g, turns))
  private def maximize(group: Set[Turn], turns: List[Turn]): Set[Turn] = {
    val g = new mutable.HashSet[Turn]()
    g ++= group
    for (turn <- turns) {
      if (!g.exists(t => t.conflicts_with(turn))) {
        g += turn
      }
    }
    return g.toSet
  }

  // Least number of phases can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach.
  def arbitrary_phases(vert: Vertex): List[Phase] = {
    val turns_remaining = new mutable.TreeSet[Turn]()
    turns_remaining ++= vert.turns

    val groups = new mutable.ListBuffer[Set[Turn]]()
    while (turns_remaining.nonEmpty) {
      val this_group = new mutable.HashSet[Turn]()
      this_group += turns_remaining.head
      turns_remaining -= this_group.head

      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      for (candidate <- turns_remaining) {
        if (!this_group.exists(t => t.conflicts_with(candidate))) {
          this_group += candidate
          turns_remaining -= candidate
        }
      }

      groups += this_group.toSet
    }
    return turn_groups_to_phases(maximize_groups(groups.toList, vert.turns))
  }

  // Try to group turns from the same road
  // TODO refactor
  def group_by_roads(vert: Vertex): List[Phase] = {
    val turns_remaining = new mutable.TreeSet[Turn]()
    turns_remaining ++= vert.turns

    val groups = new mutable.ListBuffer[Set[Turn]]()
    while (turns_remaining.nonEmpty) {
      val this_group = new mutable.HashSet[Turn]()
      this_group += turns_remaining.head
      turns_remaining -= this_group.head

      // Try to add remaining turns in the same road first
      // Then do the rest, normally
      val road = this_group.head.from.road
      val consider_order = turns_remaining.partition(t => t.from.road == road)
      for (candidate <- consider_order._1.toList ++ consider_order._2.toList) {
        if (!this_group.exists(t => t.conflicts_with(candidate))) {
          this_group += candidate
          turns_remaining -= candidate
        }
      }

      groups += this_group.toSet
    }
    return turn_groups_to_phases(maximize_groups(groups.toList, vert.turns))
  }

  // TODO other groupings for standard 3/4 phase lights
}
