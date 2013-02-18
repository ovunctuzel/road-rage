// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.annotation.elidable

import utexas.aorta.Util

// Ironically, using timers in tight loops has caused up to 3x slowdown before.
// Java profilers might be safer.

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

  @elidable(elidable.ASSERTION) def start(): Stopwatch = {
    from = System.nanoTime
    return this
  }

  @elidable(elidable.ASSERTION) def stop = {
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

  def describe() = {
    Util.log(s"$name took $seconds seconds")
  }
}
