// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import utexas.aorta.Util

// Ironically, using timers in tight loops has caused up to 3x slowdown before.
// Java profilers might be safer.

object Profiling {
  // TODO the ability to disable
  def timer(msg: String) = new Timer(msg)
  def stopwatch(name: String = "") = new Stopwatch(name)

  // Figure out what eats the most time simulating each tick
  // TODO generalize this idea of breaking down
  val whole_step = stopwatch("entire ticks")
  val agent_step = stopwatch("agent steps")
  val react = stopwatch("agent reactions")
  val choose_act = stopwatch("  choose_act")
  val desired_lane = stopwatch("    desired_lane")
  val safe_lane = stopwatch("    safe_to_lanechange")
  val react_accel = stopwatch("    max_safe_accel")
  val constraint_agents = stopwatch("      agent constraints")
  val constraint_stops = stopwatch("      intersection constraints")
  val lookahead = stopwatch("      determining next steps")
  private val watches = List(
    agent_step, react, choose_act, desired_lane, safe_lane, react_accel,
    constraint_agents, constraint_stops, lookahead
  )
  // TODO break down more. GUI? iterating over agents vs calling methods?

  def shutdown() = {
    if (whole_step.seconds != 0.0) {
      Util.log(f"Cumulatively, ${whole_step.seconds}%.2f seconds spent simulating ticks")
      for (watch <- watches) {
        val percentage = (watch.seconds / whole_step.seconds) * 100.0
        Util.log(f"${watch.name}: $percentage%.2f%   (${watch.seconds}%.2fs)")
      }
    }
  }
}

// One use
class Timer(val msg: String = "") {
  private val start = System.nanoTime

  def so_far = (System.nanoTime - start) / 1000000000.0

  def stop = {
    Util.log("\"" + msg + "\": " + so_far + "s")
  }
}

// Multi-use
class Stopwatch(val name: String) {
  private var from: Long = 0
  var seconds: Double = 0.0

  @elidable(ASSERTION) def start = {
    from = System.nanoTime
  }

  @elidable(ASSERTION) def stop = {
    val now = System.nanoTime
    seconds += (now - from) / 1000000000.0
  }

  // TODO replace with what?
  def time[A](thunk: () => A): A = {
    try {
      start
      thunk()
    } finally {
      stop
    }
  }
}
