// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.analysis

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import utexas.aorta.sim.Agent
import utexas.aorta.map.{Edge, Turn}

import utexas.aorta.{Util, cfg}

// Maintain a graph of what lanes depend on another due to agents in one
// blocking the turns in another.
object Gridlock {
  // A lane can only depend on one other lane. Thus, our representation can be
  // really simple.
  val deps = new MutableMap[Edge, Edge]()
  // But because of the weird order for adding and removing, "blame" the
  // dependency on multiple agents and maintain that here.
  val dep_counter = new MutableMap[(Edge, Edge), Int]()
  // Agent => (where they were, what lane they were stalled by)
  val last_stalled_by = new MutableMap[Agent, (Edge, Edge)]()

  // We'd do it here statically, but simulation probably doesn't exist yet
  var schedule_check = true
  val check_time = 5.0 // TODO cfg

  def add_dep(e1: Edge, e2: Edge) = {
    if (schedule_check) {
      Agent.sim.schedule(Agent.sim.tick + check_time, { Gridlock.detect_cycles })
      schedule_check = false
    }

    deps(e1) = e2
    if (!dep_counter.contains((e1, e2))) {
      dep_counter((e1, e2)) = 0
    }
    dep_counter((e1, e2)) += 1
  }

  // TODO need to call this when an agent finishes? remove their dep, not
  // someone's dep on them.
  def remove_dep(e1: Edge, e2: Edge) = {
    dep_counter((e1, e2)) -= 1
    if (dep_counter((e1, e2)) == 0) {
      dep_counter -= ((e1, e2))
      deps -= e1
    }
  }

  // Tarjan's isn't needed; there's a really simple degenerate case if every
  // node's only got one out-edge
  def detect_cycles(): Unit = {
    // First and foremost...
    Agent.sim.schedule(Agent.sim.tick + check_time, { Gridlock.detect_cycles })

    for (e <- deps.keys) {
      Util.log(e.id + " depends on " + deps(e).id + " with cnt " + dep_counter((e, deps(e))))
    }
    Util.log("")

    val visited = new MutableSet[Edge]()
    val in_gridlock = new MutableSet[Edge]()
    for (e <- deps.keys) {
      if (!visited(e)) {
        // flood from a
        var cur = e
        val this_group = new MutableSet[Edge]()
        // as soon as we hit a lane we've already visited, we know there can't
        // be a cycle -- UNLESS this_group also contains them!
        var found_gridlock = false
        while (!found_gridlock && deps.contains(cur) && (!visited(e) || this_group(e))) {
          visited += cur
          // Detect self-cycles
          if (cur == deps(cur)) {
            this_group += cur
          }
          if (this_group(cur)) {
            // Cycle! Gridlock!
            // Don't report it if we're discovering more things that lead to
            // already-reported gridlock. & is intersect.
            if ((in_gridlock & this_group).isEmpty) {
              Util.log("Gridlock detected at " + Agent.sim.tick + " among: " + this_group)
            } else {
              Util.log("not reporting new case with " + cur)
            }
            found_gridlock = true   // break the loop
            in_gridlock ++= this_group
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
                   turn_blocked_by: Option[Agent]): Unit =
  {
    // Do we need to remove an old dependency?
    if (last_stalled_by.contains(a)) {
      last_stalled_by(a) match {
        case (e1: Edge, e2: Edge) => {
          remove_dep(e1, e2)
        }
      }
      last_stalled_by -= a
    }

    if (a.speed == 0.0 && accel == 0.0 && a.cur_queue.head.get == a) {
      val now_lane = a.at.on match {
        case e: Edge => e
        case _       => return  // woo, hope we're not blocked in a turn ;)
      }
      // follow_agent is never relevant at the front of the queue
      (turn_blocked_by match {
        case Some(other) => other.at.on match {
          case e: Edge => Some(e)
          case t: Turn => Some(t.to)  // this can happen temporarily, but not permanently
        }
        case None => None
      }) match {
        case Some(now_blocked) => {
          add_dep(now_lane, now_blocked)
          last_stalled_by(a) = (now_lane, now_blocked)
        }
        case None => // an intersection, then
      }
    }
  }
}
