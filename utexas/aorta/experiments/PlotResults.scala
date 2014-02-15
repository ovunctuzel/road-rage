// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.collection.JavaConverters.seqAsJavaListConverter
import org.jfree.data.xy.{XYSeries, XYSeriesCollection, IntervalXYDataset}
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

object PlotResults extends PlotUtil {
  def main(args: Array[String]) {
    args(0) match {
      // TODO mark all of these as for single scenarios only
      // TODO separate out somehow!
      case "time" => {
        val data = time_distr(read_times(args(2)))
        args(1) match {
          case "histogram" => show(histogram(data))
          case "boxplot" => show(boxplot(data))
        }
      }
      case "turn_delay" => {
        val data = turn_delay_distr(read_turn_delay(args(2)))
        args(1) match {
          case "histogram" => show(histogram(data))
        }
      }
      case "time_vs_priority" => show(scatterplot(time_vs_priority(read_times(args(1)))))
    }
  }

  private def time_vs_priority(s: ScenarioTimes) = ScatterData(
    s.modes.zipWithIndex.map(_ match {
      case (mode, idx) => mode -> s.agents.map(a => (a.priority, a.times(idx) / a.ideal_time))
    }).toMap, "Time vs priority", "Priority", "Trip time / ideal time"
  )

  private def time_distr(s: ScenarioTimes) = DistributionData(
    s.modes.zipWithIndex.map(_ match {
      case (mode, idx) => mode -> s.agents.map(a => a.times(idx) / a.ideal_time)
    }).toMap, "Trip time distribution", "Trip time / ideal time"
  )

  private def turn_delay_distr(s: ScenarioTurnDelays) = PreBinnedData(
    s.delays.groupBy(_.mode).map(_ match {
      case (mode, ls) => mode -> ls.map(d => (d.bin, d.count))
    }).toMap, "Turn delay distribution", "Turn delay (s)"
  )
}

trait PlotUtil {
  private val num_bins = 20

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

  protected def read_turn_delay(fn: String): ScenarioTurnDelays = {
    val lines = read(fn).getLines
    Util.assert_eq(lines.next, "mode turn_delay_bin count")
    return ScenarioTurnDelays(
      ScenarioTag(fn), lines.map(l => TurnDelayResult(l.split(" "))).toArray
    )
  }

  protected def add_xy(chart: XYSeriesCollection, name: String, data: Array[(Double, Double)]) {
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

case class ScenarioTag(id: String, map: String)
case class TripTimeResult(priority: Double, ideal_time: Double, times: Array[Double])
case class TurnDelayResult(mode: String, bin: Double, count: Double)
case class ScenarioTimes(tag: ScenarioTag, modes: Array[String], agents: Array[TripTimeResult])
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
object TurnDelayResult {
  def apply(fields: Array[String]) =
    new TurnDelayResult(fields(0), fields(1).toDouble, fields(2).toDouble)
}
