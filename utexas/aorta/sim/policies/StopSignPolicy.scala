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
    super.is_waiting(a, far_away) && a.how_long_idle >= cfg.pause_at_stop

  // Add agent to the queue if they satisfy our requirements.
  def react() = {
    val orig_empty = queue.isEmpty
    for ((a, _) <- waiting_agents) {
      if (is_waiting(a)) {
        queue :+= a
      }
    }
    if (orig_empty) {
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
    if (queue.headOption == old_owner) {
      approve_head
    }
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log("Current queue: " + queue)
  }

  private def approve_head = queue.headOption match {
    case Some(a) => a.allow_turn(intersection)
    case None =>
  }
}
