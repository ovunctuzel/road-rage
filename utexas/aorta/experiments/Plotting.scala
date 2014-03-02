// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.collection.JavaConverters.seqAsJavaListConverter
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{JFreeChart, ChartFactory, ChartFrame}
import org.jfree.data.statistics.{HistogramDataset, DefaultBoxAndWhiskerCategoryDataset, Statistics}
import org.jfree.chart.plot.PlotOrientation
import scala.io.Source
import java.util.zip.GZIPInputStream
import java.io.{BufferedInputStream, FileInputStream}

import utexas.aorta.common.Util

// TODO auto-titling and saving plots with good filenames

// Used for showing histograms or boxplots from a bunch of individual values
case class DistributionData(
  values_per_mode: Map[String, Array[Double]], title: String, label: String
) {
  def cap(limit: Double) = copy(values_per_mode = values_per_mode.mapValues(
    ls => ls.filter(_ < limit)
  ))
  def select(mode: String) = copy(values_per_mode = Map(mode -> values_per_mode(mode)))
  def vs_baseline() = copy(values_per_mode = (values_per_mode - "baseline").mapValues(agents =>
    agents.zip(values_per_mode("baseline")).map(_ match { case (x, baseline) => x - baseline })
  ))
}
// Histograms and boxplots from pre-binned values. Arrays of (bin, count)
case class PreBinnedData(
  bins_per_mode: Map[String, Array[(Double, Double)]], title: String, label: String
)
// (X, Y) pairs for association
case class ScatterData(
  points_per_mode: Map[String, Array[(Double, Double)]], title: String, x: String, y: String
) {
  // TODO mod title in cap, vs_baseline, etc
  def cap(limit: Double) = copy(points_per_mode = points_per_mode.mapValues(
    ls => ls.filter(pt => pt._2 < limit)
  ))
  def select(mode: String) = copy(points_per_mode = Map(mode -> points_per_mode(mode)))
  // Assumes higher is worse, so it does baseline - each mode. In the result, more positive is
  // better.
  def vs_baseline() = copy(points_per_mode = (points_per_mode - "baseline").mapValues(agents =>
    agents.zip(points_per_mode("baseline"))
      .map(_ match { case (pt, baseline) => (pt._1, pt._2 - baseline._2) })
  ))
}

trait PlotUtil {
  private val num_bins = 20

  private def add_xy(chart: XYSeriesCollection, name: String, data: Array[(Double, Double)]) {
    val series = new XYSeries(name)
    for ((x, y) <- data) {
      series.add(x, y)
    }
    chart.addSeries(series)
  }

  def scatterplot(data: ScatterData): JFreeChart = {
    val chart = new XYSeriesCollection()
    Util.log(s"Correlation between ${data.x} and ${data.y}")
    for ((mode, points) <- data.points_per_mode) {
      add_xy(chart, mode, points)
      val coefficient = Statistics.getCorrelation(
        points.map(_._1.asInstanceOf[java.lang.Number]),
        points.map(_._2.asInstanceOf[java.lang.Number])
      )
      Util.log(s"  $mode: r = $coefficient")
    }
    return ChartFactory.createScatterPlot(
      data.title, data.x, data.y, chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  // TODO hollow style would rock
  def histogram(data: DistributionData): JFreeChart = {
    val chart = new HistogramDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.addSeries(mode, values, num_bins)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  def histogram(data: PreBinnedData): JFreeChart = {
    val chart = new XYSeriesCollection()
    for ((mode, bin_counts) <- data.bins_per_mode) {
      add_xy(chart, mode, bin_counts)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  def boxplot(data: DistributionData): JFreeChart = {
    val chart = new DefaultBoxAndWhiskerCategoryDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.add(values.toList.asJava, mode, "")
    }
    return ChartFactory.createBoxAndWhiskerChart(data.title, "Mode", data.label, chart, true)
  }

  def show(chart: JFreeChart) {
    val frame = new ChartFrame("A nefarious plot", chart)
    frame.pack()
    frame.setVisible(true)
  }
}

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
