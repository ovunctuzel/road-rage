// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.io.Source
import scala.collection.mutable.{HashMap => MutableMap}

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import java.io.File
import javax.imageio.ImageIO

import utexas.aorta.Util

object Summaries {
  val cities = List("austin", "baton_rouge", "seattle", "sf")
  val modes = List("fifo", "equal-no-sys", "auction-no-sys", "fixed-no-sys",
                   "equal-sys", "auction-sys", "fixed-sys")
  val names = Map(
    "austin" -> "Austin", "baton_rouge" -> "Baton Rouge",
    "seattle" -> "Seattle", "sf" -> "San Francisco"
  )
  val metrics = List("unweighted    ", "weighted      ", "stray agents  ")

  private def get_tuple(fn: String): Array[String] = {
    for (line <- Source.fromFile(fn).getLines()) {
      if (line.startsWith("TABLE:")) {
        return line.trim.split("TABLE:")(1).split(":")
      }
    }
    throw new Exception(s"Summary $fn didn't have TABLE: line!")
  }

  def main(args: Array[String]) = {
    val data = cities.map(city => city -> new MutableMap[String, Array[String]]()).toMap

    // Gather the data and print a LaTeX table...
    for (city <- cities) {
      println(s"  \\\\textbf{${names(city)}} &&&&&&& \\\\")
      for (mode <- modes) {
        data(city)(mode) = get_tuple(s"final/${city}-${mode}/summary")
      }

      for ((metric, idx) <- metrics.zipWithIndex) {
        println(
          s"  $metric" +
          modes.map(mode => s"& ${data(city)(mode)(idx)} ").mkString("") +
          "\\\\"
        )
      }
      println("  \\hline\n")
    }

    // Make a bar chart!
  }
}

    /*val chart = ChartFactory.createHistogram(
      title, x_axis, "Frequency", dataset, PlotOrientation.VERTICAL, true,
      false, false
    )
    val img = chart.createBufferedImage(800, 600)
    ImageIO.write(img, "png", new File(fn))
    */
