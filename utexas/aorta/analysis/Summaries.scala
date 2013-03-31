// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.io.Source
import scala.collection.mutable.{HashMap => MutableMap}

import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.{JFreeChart, LegendItemCollection}
import org.jfree.chart.axis.{NumberAxis, CategoryAxis}
import org.jfree.chart.renderer.category.BarRenderer
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

  private def num(n: String) = BigInt(n.replace(",", ""))

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

    // Make bar charts for each city!
    for (city <- cities) {
      // Two datasets... weighted and unweighted
      val dataset1 = new DefaultCategoryDataset()
      val dataset2 = new DefaultCategoryDataset()
      for (mode <- modes) {
        dataset1.addValue(num(data(city)(mode)(0)), "unweighted", mode)
        dataset1.addValue(null, "Dummy 1", mode)

        dataset2.addValue(null, "Dummy 2", mode)
        dataset2.addValue(num(data(city)(mode)(1)), "weighted", mode)
      }

      val plot = new CategoryPlot(
        dataset1, new CategoryAxis("Ordering"),
        new NumberAxis("Unweighted time (s)"), new BarRenderer()
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
      plot.setRangeAxis(1, new NumberAxis("Weighted time (s)"))
      plot.setRenderer(1, new BarRenderer())

      val img = chart.createBufferedImage(800, 600)
      ImageIO.write(img, "png", new File(s"final/barchart_${city}.png"))
    }
  }
}
