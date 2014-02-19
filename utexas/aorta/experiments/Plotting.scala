// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.collection.JavaConverters.seqAsJavaListConverter
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{JFreeChart, ChartFactory, ChartFrame}
import org.jfree.data.statistics.{HistogramDataset, DefaultBoxAndWhiskerCategoryDataset}
import org.jfree.chart.plot.PlotOrientation
import scala.io.Source
import java.util.zip.GZIPInputStream
import java.io.{BufferedInputStream, FileInputStream}

import utexas.aorta.common.Util

// Used for showing histograms or boxplots from a bunch of individual values
case class DistributionData(
  values_per_mode: Map[String, Array[Double]], title: String, label: String
)
// Histograms and boxplots from pre-binned values. Arrays of (bin, count)
case class PreBinnedData(
  bins_per_mode: Map[String, Array[(Double, Double)]], title: String, label: String
)
// (X, Y) pairs for association
case class ScatterData(
  points_per_mode: Map[String, Array[(Double, Double)]], title: String, x: String, y: String
)

trait PlotUtil {
  private val num_bins = 20

  private def add_xy(chart: XYSeriesCollection, name: String, data: Array[(Double, Double)]) {
    val series = new XYSeries(name)
    for ((x, y) <- data) {
      series.add(x, y)
    }
    chart.addSeries(series)
  }

  protected def scatterplot(data: ScatterData): JFreeChart = {
    val chart = new XYSeriesCollection()
    for ((mode, points) <- data.points_per_mode) {
      add_xy(chart, mode, points)
    }
    return ChartFactory.createScatterPlot(
      data.title, data.x, data.y, chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  // TODO hollow style would rock
  protected def histogram(data: DistributionData): JFreeChart = {
    val chart = new HistogramDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.addSeries(mode, values, num_bins)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  protected def histogram(data: PreBinnedData): JFreeChart = {
    val chart = new XYSeriesCollection()
    for ((mode, bin_counts) <- data.bins_per_mode) {
      add_xy(chart, mode, bin_counts)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  protected def boxplot(data: DistributionData): JFreeChart = {
    val chart = new DefaultBoxAndWhiskerCategoryDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.add(values.toList.asJava, mode, "")
    }
    return ChartFactory.createBoxAndWhiskerChart(data.title, "Mode", data.label, chart, true)
  }

  protected def show(chart: JFreeChart) {
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

  protected def read_times(fn: String): ScenarioTimes = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_time"))
    return ScenarioTimes(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripTimeResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  protected def read_distances(fn: String): ScenarioDistances = {
    val lines = read(fn).getLines
    val header = lines.next.split(" ")
    Util.assert_eq(header.take(3).toList, List("agent", "priority", "ideal_distance"))
    return ScenarioDistances(
      ScenarioTag(fn), header.drop(3),
      lines.map(l => TripDistanceResult(l.split(" ").map(_.toDouble))).toArray
    )
  }

  protected def read_turn_delay(fn: String): ScenarioTurnDelays = {
    val lines = read(fn).getLines
    Util.assert_eq(lines.next, "mode turn_delay_bin count")
    return ScenarioTurnDelays(
      ScenarioTag(fn), lines.map(l => TurnDelayResult(l.split(" "))).toArray
    )
  }
}

case class ScenarioTag(id: String, map: String)

case class TripTimeResult(priority: Double, ideal_time: Double, times: Array[Double])
case class ScenarioTimes(tag: ScenarioTag, modes: Array[String], agents: Array[TripTimeResult])

case class TripDistanceResult(priority: Double, ideal_distance: Double, distances: Array[Double])
case class ScenarioDistances(tag: ScenarioTag, modes: Array[String], agents: Array[TripDistanceResult])

case class TurnDelayResult(mode: String, bin: Double, count: Double)
case class ScenarioTurnDelays(tag: ScenarioTag, delays: Array[TurnDelayResult])

// TODO stuff here is the dual of stuff in Metrics. pair them together somehow?
object ScenarioTag {
  // fn is of the form $metric.$scenario.$map, possibly with a trailing .gz
  def apply(fn: String): ScenarioTag = {
    val pieces = fn.split("\\.")
    return new ScenarioTag(pieces(1), pieces(2))
  }
}
object TripTimeResult {
  def apply(fields: Array[Double]) = new TripTimeResult(fields(1), fields(2), fields.drop(3))
}
object TripDistanceResult {
  def apply(fields: Array[Double]) = new TripDistanceResult(fields(1), fields(2), fields.drop(3))
}
object TurnDelayResult {
  def apply(fields: Array[String]) =
    new TurnDelayResult(fields(0), fields(1).toDouble, fields(2).toDouble)
}
