// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // Head is the current owner.
  private var queue = List[Agent]()

  // Require agents to pause a moment
  override def is_waiting(a: Agent) =
    super.is_waiting(a) && a.how_long_idle >= cfg.pause_at_stop

  // Add agent to the queue if they satisfy our requirements.
  def react() = {
    val orig_empty = queue.isEmpty
    for ((a, turn) <- waiting_agents) {
      if (is_waiting(a)) {
        queue :+= a
        waiting_agents -= ((a, turn)) // TODO mod while iterate?
      }
    }
    if (orig_empty && queue.nonEmpty) {
      approve_head
    }
  }

  def validate_entry(a: Agent, turn: Turn) =
    queue.headOption.getOrElse(null) == a

  def handle_exit(a: Agent, turn: Turn) = {
    Util.assert_eq(queue.head, a)
    queue = queue.tail
    approve_head
  }

  def unregister(a: Agent) = {
    val old_owner = queue.headOption
    queue = queue.filter(_ != a)
    waiting_agents = waiting_agents.filter(req => req._1 != a)
    if (old_owner.getOrElse(null) == a) {
      approve_head
    }
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log("Current queue: " + queue)
  }

  private def approve_head = queue.headOption match {
    case Some(a) => a.approve_turn(intersection)
    case None =>
  }
}
