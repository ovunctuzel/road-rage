// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.analysis

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import utexas.aorta.sim.Agent

import utexas.aorta.{Util, cfg}

// Maintain a graph of what agents are not moving due to other agents, whether
// because they're directly following them or they're blocked from completing a
// turn by them
object Gridlock {
  // TODO reduce size by caring about head agents only

  // An agent can only depend on one other agent. Thus, our representation can
  // be really simple.
  val deps = new MutableMap[Agent, Agent]()
  val last_stalled_by = new MutableMap[Agent, Option[Agent]]()

  // We'd do it here statically, but simulation probably doesn't exist yet
  var schedule_check = true
  val check_time = 5.0 // TODO cfg

  def add_dep(a1: Agent, a2: Agent) = {
    if (schedule_check) {
      Agent.sim.schedule(Agent.sim.tick + check_time, { Gridlock.detect_cycles })
      schedule_check = false
    }

    assert(!deps.contains(a1))  // did they call remove_dep first?
    deps(a1) = a2
  }

  // TODO need to call this when an agent finishes? remove their dep, not
  // someone's dep on them.
  def remove_dep(a1: Agent, a2: Agent) = {
    assert(deps.contains(a1) && deps(a1) == a2)
    deps -= a1
  }

  // Tarjan's isn't needed; there's a really simple degenerate case if every
  // node's only got one out-edge
  def detect_cycles(): Unit = {
    // First and foremost...
    Agent.sim.schedule(Agent.sim.tick + check_time, { Gridlock.detect_cycles })

    val visited = new MutableSet[Agent]()
    for (a <- deps.keys) {
      if (!visited(a)) {
        // flood from a
        var cur = a
        val this_group = new MutableSet[Agent]()
        // as soon as we hit an agent we've already visited, we know there can't
        // be a cycle -- UNLESS this_group also contains them!
        var found_gridlock = false
        while (!found_gridlock && deps.contains(cur) && (!visited(a) || this_group(a))) {
          visited += cur
          // Detect self-cycles
          if (cur == deps(cur)) {
            this_group += cur
          }
          if (this_group(cur)) {
            // Cycle! Gridlock!
            Util.log("Gridlock detected among: " + this_group)
            found_gridlock = true   // break the loop
            // TODO pause if we're in UI?
          }
          this_group += cur
          cur = deps(cur)
        }
      }
    }
  }

  // Record dependencies between agents in case gridlock could occur
  def handle_agent(a: Agent, accel: Double, follow_agent: Option[Agent],
                   turn_blocked_by: Option[Agent]) =
  {
    // TODO garbage collect these when the agent goes away
    if (!last_stalled_by.contains(a)) {
      last_stalled_by(a) = None
    }

    // TODO it'd rock to clean this up by separating it and the required state
    // out
    val is_stalled = a.speed == 0.0 && accel == 0.0
    if (is_stalled) {
      // It's either one or the other, or neither if it's the intersection.
      val now_stalled_by = if (follow_agent.isDefined)
                             follow_agent
                           else if (turn_blocked_by.isDefined)
                             turn_blocked_by
                           else
                             None
      // So record the change
      (last_stalled_by(a), now_stalled_by) match {
        case (Some(old), Some(now)) if old != now => {
          // a change! that was quick
          remove_dep(a, old)
          add_dep(a, now)
        }
        case (Some(old), None) => remove_dep(a, old)
        case (None, Some(now)) => add_dep(a, now)
        case _ => // No change; last_stalled_by == now_stalled_by already
      }
      last_stalled_by(a) = now_stalled_by
    } else {
      last_stalled_by(a) match {                                           
        case Some(other) => {
          remove_dep(a, other)
          last_stalled_by(a) = None
        }
        case None =>
      }
    }
  }
}
