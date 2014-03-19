// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import java.io.{BufferedReader, FileReader, PrintWriter}

import utexas.aorta.common.{StateWriter, StateReader}

case class ScrapedData(feature_names: List[String], data: List[RawInstance]) {
  def save_csv(fn: String) {
    val out = new PrintWriter(fn)
    out.println(("label" :: feature_names).mkString(","))
    for (r <- data) {
      out.println((r.label :: r.features.map(_.toString)).mkString(","))
    }
    out.close()
  }
}
object ScrapedData {
  def read_csv(fn: String): ScrapedData = {
    val in = new BufferedReader(new FileReader(fn))
    val features = in.readLine.split(",").tail
    val data = Stream.continually(in.readLine).takeWhile(_ != null).map(line => {
      val fields = line.split(",")
      RawInstance(fields.head, fields.tail.map(_.toDouble).toList)
    })
    return ScrapedData(features.toList, data.toList)
  }
}
case class RawInstance(label: String, features: List[Double])

// min is inclusive, max is exclusive -- that way, bins are [0, num_bins)
case class FeatureSummary(feature: Int, min: Double, max: Double, num_bins: Int) {
  def transform(x: Double): Int = {
    if (x < min || x >= max) {
      // TODO or just cap... testing instance could exceed what we saw in training
      throw new IllegalArgumentException(s"$x not in [$min, $max]")
    }
    val normalized = (x - min) / (max - min)
    return math.floor(normalized * num_bins).toInt
  }
}

case class DataFixer(summaries: Seq[FeatureSummary], labels: Set[String]) {
  def transform(raw: RawInstance) = LabeledInstance(
    raw.label, raw.features.zip(summaries).map(pair => pair._2.transform(pair._1))
  )
}

object Preprocessing {
  def summarize(instances: List[RawInstance], num_bins: Int): DataFixer = {
    val mins = Array.fill(instances.head.features.size)(Double.PositiveInfinity)
    val maxs = Array.fill(instances.head.features.size)(Double.NegativeInfinity)
    for (instance <- instances) {
      for ((value, feature) <- instance.features.zipWithIndex) {
        mins(feature) = math.min(mins(feature), value)
        maxs(feature) = math.max(maxs(feature), value)
      }
    }
    // To make max an exclusive value to get the right number of bins, bump up the max a bit.
    val epsilon = 1e-5
    val summaries = for (
      feature <- mins.indices
    ) yield FeatureSummary(feature, mins(feature), maxs(feature) + epsilon, num_bins)
    val labels = instances.map(_.label).toSet
    println("Feature summaries:")
    for (s <- summaries) {
      println(s"  $s")
    }
    println("")
    return DataFixer(summaries, labels)
  }
}
