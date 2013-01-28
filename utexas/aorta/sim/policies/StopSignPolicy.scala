// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.SortedSet

import utexas.aorta.map.{Turn, Edge, Vertex}
import utexas.aorta.sim.{Intersection, Policy, Agent}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // Since we only add somebody to our queue when they're sufficiently close,
  // flood out to discover what edges we need to watch. This is independent of
  // lookahead and dt.
  private val monitor_edges = SortedSet((find_edges(intersection.v, cfg.end_threshold)): _*)
  // We only add an agent to this once they satisfy our entrance requirements.
  // Head is the current owner.
  private var queue = List[Agent]()
  // This just backs our queue
  private val queued_agents = new MutableSet[Agent]()

  override def is_waiting(a: Agent, far_away: Double) =
    super.is_waiting(a, far_away) && a.how_long_idle >= cfg.pause_at_stop

  def react() = {
    // If multiple agents get to enter the queue this step, the order is
    // arbitrary but deterministic.
    for (e <- monitor_edges) {
      e.queue.head match {
        case Some(a) if !queued_agents(a) && is_waiting(a) => {
          queue :+= a
          queued_agents += a
        }
        case _ =>
      }
    }
  }

  def can_go(a: Agent, turn: Turn, far_away: Double) =
    queue.headOption.getOrElse(null) == a

  def validate_entry(a: Agent, turn: Turn) = can_go(a, turn, 0)

  def handle_exit(a: Agent, turn: Turn) = {
    queue = queue.tail
    queued_agents -= a
  }

  def unregister(a: Agent) = {
    queue = queue.filter(_ != a)
    queued_agents -= a
  }

  def current_greens = intersection.turns.keys.toSet

  def dump_info() = {
    Util.log("Current queue: " + queue)
    Util.log("Edges we watch: " + monitor_edges)
  }

  private def find_edges(v: Vertex, distance: Double): List[Edge] = {
    // TODO functionally?
    val buffer = new MutableSet[Edge]()
    for (e <- v.in_edges) {
      buffer += e
      if (distance >= e.length) {
        buffer ++= find_edges(e.from, distance - e.length)
      }
    }
    return buffer.toList
  }
}
