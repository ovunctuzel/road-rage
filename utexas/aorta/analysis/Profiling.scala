// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.Util

object Profiling {
  // TODO the ability to disable
  def timer(msg: String) = new Timer(msg)
  def stopwatch(name: String = "") = new Stopwatch(name)

  // Figure out what eats the most time simulating each tick
  // TODO generalize this idea of breaking down
  val whole_step = stopwatch("entire ticks")
  val agent_step = stopwatch("agent steps")
  val queue_check = stopwatch("queue checks")
  val intersection_check = stopwatch("intersection checks")
  val react = stopwatch("agent reactions")
  val desired_lane = stopwatch("  reactions (desired_lane)")
  val safe_lane = stopwatch("  reactions (safe_to_lanechange)")
  val react_accel = stopwatch("  reactions (max_safe_accel)")
  private val watches = List(agent_step, queue_check, intersection_check, react,
                             desired_lane, safe_lane, react_accel)
  // TODO break down more. GUI? iterating over agents vs calling methods?

  def shutdown() = {
    Util.log(f"Cumulatively, ${whole_step.seconds}%.2f seconds spent simulating ticks")
    for (watch <- watches) {
      val percentage = (watch.seconds / whole_step.seconds) * 100.0
      Util.log(f"${watch.name}: $percentage%.2f%")
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

  def start = {
    from = System.nanoTime
  }

  def stop = {
    val now = System.nanoTime
    seconds += (now - from) / 1000000000.0
  }

  def time[A](thunk: () => A): A = {
    try {
      start
      thunk()
    } finally {
      stop
    }
  }
}
