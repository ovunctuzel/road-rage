// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.io.Source
import java.util.zip.GZIPInputStream
import java.io.{BufferedInputStream, FileInputStream}

import utexas.aorta.common.Util

trait MetricReader {
  private def read(fn: String) =
    if (fn.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(fn))))
    else
      Source.fromFile(fn)

  def read_times(fn: String): ScenarioTimes = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_time"))
    return ScenarioTimes(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripTimeResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  def read_distances(fn: String): ScenarioDistances = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_distance"))
    return ScenarioDistances(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripDistanceResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  def read_turn_delay(fn: String): ScenarioTurnDelays = {
    val lines = read(fn).getLines
    Util.assert_eq(lines.next, "mode turn_delay_bin count")
    return ScenarioTurnDelays(
      ScenarioTag(fn), lines.map(l => TurnDelayResult(l.split(" "))).toArray
    )
  }

  def read_paths(fn: String): ScenarioPaths = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_spawn_time"))
    return ScenarioPaths(
      ScenarioTag(fn), header.drop(3), lines.map(l => AgentPath(l.split(" "))).toArray
    )
  }
}

case class ScenarioTag(id: String, map: String)

case class TripTimeResult(id: Int, priority: Double, ideal_time: Double, times: Array[Double])
case class TripDistanceResult(id: Int, priority: Double, ideal_distance: Double, distances: Array[Double])
case class TurnDelayResult(mode: String, bin: Double, count: Double)
case class Crossing(road: Int, entry: Double, exit: Double)
case class AgentPath(id: Int, priority: Double, ideal_spawn_time: Double, paths: Array[List[Crossing]])

case class ScenarioTimes(tag: ScenarioTag, modes: Array[String], agents: Array[TripTimeResult])
case class ScenarioDistances(tag: ScenarioTag, modes: Array[String], agents: Array[TripDistanceResult])
case class ScenarioTurnDelays(tag: ScenarioTag, delays: Array[TurnDelayResult])
case class ScenarioPaths(tag: ScenarioTag, modes: Array[String], agents: Array[AgentPath])

// TODO stuff here is the dual of stuff in Metrics. pair them together somehow?
object ScenarioTag {
  // fn is of the form $metric.$scenario.$map, possibly with a trailing .gz
  def apply(fn: String): ScenarioTag = {
    val pieces = fn.split("\\.")
    return new ScenarioTag(pieces(1), pieces(2))
  }
}
object TripTimeResult {
  def apply(fields: Array[Double]) = new TripTimeResult(
    fields(0).toInt, fields(1), fields(2), fields.drop(3)
  )
}
object TripDistanceResult {
  def apply(fields: Array[Double]) = new TripDistanceResult(
    fields(0).toInt, fields(1), fields(2), fields.drop(3)
  )
}
object TurnDelayResult {
  def apply(fields: Array[String]) =
    new TurnDelayResult(fields(0), fields(1).toDouble, fields(2).toDouble)
}
object AgentPath {
  def apply(fields: Array[String]) = new AgentPath(
    fields(0).toInt, fields(1).toDouble, fields(2).toDouble,
    fields.drop(3).map(s => Crossing.list(s)).toArray
  )
}
object Crossing {
  def list(raw: String) = raw.split(",").grouped(3).map(triple => single(triple)).toList
  def single(triple: Array[String]) = Crossing(triple(0).toInt, triple(1).toDouble, triple(2).toDouble)
}
