// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Agent}

import utexas.aorta.{Util, cfg}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // owner of the intersection! may be None when the queue has members. In that
  // case, the first person has to pause a bit longer before continuing.
  var current_owner: Option[Agent] = None
  var queue = List[Agent]()

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // Do they have the lock?
    current_owner match {
      case Some(owner) if a == owner => return true
      case _       =>
    }

    // Schedule them if needed and if they're at the end of the edge.
    if (!queue.contains(a) && is_waiting(a, turn, far_away)) {
      queue :+= a
    }

    // Can we promote them now?
    val ready = !current_owner.isDefined && !queue.isEmpty && a == queue.head &&
                a.how_long_idle >= cfg.pause_at_stop
    if (ready) {
      // promote them!
      current_owner = Some(queue.head)
      queue = queue.tail
      return true
    } else {
      return false
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_owner match {
    case Some(a) => true
    case _       => false
  }

  def handle_exit(a: Agent, turn: Turn) = {
    //assert(a == current_owner.get)    // TODO
    if (!current_owner.isDefined || current_owner.get != a) {
      Util.log(a + " is leaving, but current owner is " + current_owner)
      Util.log("  Crazy guy was attempting " + turn)
      Util.log("  Time was " + Agent.sim.tick)
    }
    current_owner = None
    // Next time queue.head, if it exists, polls, we'll let them go if they've
    // waited patiently.
  }

  def unregister(a: Agent) = {
    current_owner match {
      case Some(agent) if a == agent => {
        // release the lock
        current_owner = None
      }
      case _ => {
        // don't bother with us
        queue = queue.filter(x => x != a)
      }
    }
  }

  override def dump_info() = {
    Util.log("Current owner: " + current_owner)
    Util.log("Queue: " + queue)
  }
}
