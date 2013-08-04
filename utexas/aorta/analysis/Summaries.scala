// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.io.Source
import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.ListBuffer

import org.jfree.data.statistics.DefaultStatisticalCategoryDataset
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.{JFreeChart, LegendItemCollection}
import org.jfree.chart.axis.{NumberAxis, CategoryAxis}
import org.jfree.chart.renderer.category.StatisticalBarRenderer
import java.io.File
import javax.imageio.ImageIO

import utexas.aorta.common.Util

class RunSummary() {
  def metrics = Summaries.metrics
  private val data = metrics.map(metric => metric -> new ListBuffer[BigDecimal]()).toMap

  private def num(n: String) = BigDecimal(n.replace(",", ""))

  def add(input: Array[String]): Unit = {
    for ((metric, idx) <- metrics.zipWithIndex) {
      data(metric) += num(input(idx))
    }
  }

  private def n = BigDecimal(data(metrics.head).size)

  def mean(metric: String, rescale: Double = 1.0) =
    (rescale * data(metric).sum) / n

  def std_dev(metric: String, rescale: Double = 1.0): BigDecimal = {
    val avg = mean(metric, rescale = rescale)
    val square_diffs = data(metric).map(datum => ((datum * rescale) - avg).pow(2))
    val sigma_squared = square_diffs.sum / n
    // We shouldn't overflow if the metric is normalized
    return BigDecimal(math.sqrt(sigma_squared.toDouble))
  }

  private def scale(metric: String) =
    if (metric == Summaries.metrics(1) || metric == Summaries.metrics(4))
      1.0 / Summaries.avg_priority
    else
      1.0

  def table(metric: String) =
    Util.comma_num_big(mean(metric, rescale = scale(metric)).toBigInt) +
    " $\\pm$ " +
    Util.comma_num_big(std_dev(metric, rescale = scale(metric)).toBigInt)
}

object Summaries {
  val cities = List("austin", "baton_rouge", "seattle", "sf")
  val modes = List("fifo", "equal-no-sys", "auction-no-sys", "fixed-no-sys",
                   "equal-sys", "auction-sys", "fixed-sys")
  val trials = 1 to 3
  val avg_priority = 0.5 * 500.0
  val names = Map(
    "austin" -> "Austin", "baton_rouge" -> "BR",
    "seattle" -> "Seattle", "sf" -> "SF"
  )
  // Weighted and unweighted take up too much space, comment them out for now.
  val metrics = List(
    "%unweighted (not normalized)", "%weighted (not normalized)", "strays",
    "unweighted", "weighted"
  )

  val data = cities.map(
    city => city -> modes.map(mode => mode -> new RunSummary()).toMap
  ).toMap

  private def get_tuple(fn: String): Array[String] = {
    for (line <- Source.fromFile(fn).getLines()) {
      if (line.startsWith("TABLE:")) {
        return line.trim.split("TABLE:")(1).split(":")
      }
    }
    throw new Exception(s"Summary $fn didn't have TABLE: line!")
  }

  def main(args: Array[String]) = {
    // First gather the data...
    for (city <- cities) {
      for (mode <- modes) {
        for (trial <- trials) {
          data(city)(mode).add(get_tuple(s"final/${city}-${trial}-${mode}/summary"))
        }
      }
    }

    // Now print a LaTeX table...
    for (city <- cities) {
      println(s"  \\textbf{${names(city)}} &&&&&&& \\\\")
      for (metric <- metrics) {
        println(
          s"  $metric " +
          modes.map(mode => s"& ${data(city)(mode).table(metric)} ").mkString("") +
          "\\\\"
        )
      }
      println("  \\hline\n")
    }

    // Make bar charts for each city!
    for (city <- cities) {
      // Two datasets... normalized weighted and normalized unweighted
      val dataset1 = new DefaultStatisticalCategoryDataset()
      val dataset2 = new DefaultStatisticalCategoryDataset()
      val cat1 = metrics(3)
      val cat2 = metrics(4)
      for (mode <- modes) {
        dataset1.add(
          data(city)(mode).mean(cat1), data(city)(mode).std_dev(cat1),
          "unweighted", mode
        )
        dataset1.add(null, null, "Dummy 1", mode)

        dataset2.add(null, null, "Dummy 2", mode)
        dataset2.add(
          data(city)(mode).mean(cat2, rescale = 1.0 / avg_priority),
          data(city)(mode).std_dev(cat2, rescale = 1.0 / avg_priority),
          "weighted", mode
        )
      }

      val plot = new CategoryPlot(
        dataset1, new CategoryAxis("Ordering"),
        new NumberAxis("Normalized unweighted time (seconds / agent)"),
        new StatisticalBarRenderer()
      ) {
        override def getLegendItems(): LegendItemCollection = {
          val ls = new LegendItemCollection()
          ls.add(getRenderer(0).getLegendItem(0, 0))
          ls.add(getRenderer(1).getLegendItem(1, 1))
          return ls
        }
      }
      val chart = new JFreeChart(
        s"Comparison of orderings in ${names(city)}", plot
      )
      plot.setDataset(1, dataset2)
      plot.mapDatasetToRangeAxis(1, 1)
      plot.setRangeAxis(1, new NumberAxis("Normalized weighted time (seconds / agent) scaled to match unweighted times"))
      plot.setRenderer(1, new StatisticalBarRenderer())

      val img = chart.createBufferedImage(800, 600)
      ImageIO.write(img, "png", new File(s"final/barchart_${city}.png"))
    }
  }
}
