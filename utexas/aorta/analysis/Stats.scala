// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.FileWriter

import utexas.aorta.Util

sealed trait Measurement {}
final case class Wasted_Time_Stat(agent: Int, intersection: Int, lag: Double,
                                  time: Double) extends Measurement
{
  override def toString = "s1 %d %d %.2f %d".format(
    agent, intersection, lag, time.toInt
  )
}
final case class Total_Trip_Stat(agent: Int, time: Double, dist: Double)
  extends Measurement
{
  // TODO want average speed?
  override def toString = "s2 %d %.2f %.2f".format(agent, time, dist)
}
final case class Intersection_Throughput_Stat(intersection: Int, requests: Int,
                                              entered: Int, time: Double)
  extends Measurement
{
  // TODO should entered > requests?
  override def toString = "s3 %d %d %d %d".format(
    intersection, requests, entered, time.toInt
  )
}
final case class Active_Agents_Stat(time: Int, cnt: Int) extends Measurement
{
  override def toString = "s4 %d %d".format(time, cnt)
}
final case class Simulator_Speedup_Stat(factor: Double, time: Double)
  extends Measurement
{
  override def toString = "s5 %.2f %.2f".format(factor, time)
}

class Aggregate_Stat(name: String) {
  var total: Double = 0.0
  var count = 0
  var min: Double = 0.0
  var max: Double = 0.0

  def update(entry: Double) = {
    if (count == 0) {
      min = entry
      max = entry
    }
    total += entry
    count += 1
    min = math.min(min, entry)
    max = math.max(max, entry)
  }

  def mean = total / count.toDouble

  def describe = "%s: %.2f average (%.2f min, %.2f max, %d samples)".format(
    name, mean, min, max, count
  )
}

// TODO move to analysis/
object Stats {
  var log: FileWriter = null
  var use_log = false
  var use_print = false
  var initialized = false

  val trip_time = new Aggregate_Stat("trip time (s)")
  val simulator_speedup = new Aggregate_Stat("simulator speedup (factor)")
  val time_wasted = new Aggregate_Stat("time wasted at individual intersections (s)")

  def setup_experiment(name: String) = {
    initialized = true
    if (use_log) {
      Util.assert_eq(log, null)
      log = new FileWriter("stats_log")
      log.write(name + "\n")
    }
    if (use_print) {
      Util.log("Experiment name: " + name)
    }
  }

  def record(item: Measurement) = {
    if (use_log) {
      log.write(item.toString + "\n")
    }
    if (use_print) {
      Util.log("Stat: " + item)
    }

    item match {
      case Wasted_Time_Stat(_, _, lag, _) => time_wasted.update(lag)
      case Total_Trip_Stat(_, time, _) => trip_time.update(time)
      case Simulator_Speedup_Stat(factor, _) => simulator_speedup.update(factor)
      case _ =>
    }
  }

  // flush any logs we were writing
  def shutdown() = {
    if (initialized) {
      if (log != null) {
        log.close
      }

      // emit a summary
      // TODO tabulate it?
      println(trip_time.describe)
      println(time_wasted.describe)
      println(simulator_speedup.describe)
    }
  }
}
