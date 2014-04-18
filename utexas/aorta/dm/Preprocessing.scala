// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import java.io.{BufferedReader, FileReader, PrintWriter}

import utexas.aorta.common.{StateWriter, StateReader}

case class ScrapedData(feature_names: List[String], data: List[RawInstance]) {
  def save_csv(fn: String) {
    val out = new PrintWriter(fn)
    out.println(("label" :: "osm_id" :: feature_names).mkString(","))
    for (r <- data) {
      out.println((r.label :: r.osm_id :: r.features.map(_.toString)).mkString(","))
    }
    out.close()
  }

  lazy val labels = data.map(_.label).toSet.toList.sorted
}
object ScrapedData {
  def read_csv(fn: String): ScrapedData = {
    val in = new BufferedReader(new FileReader(fn))
    val features = in.readLine.split(",").drop(2)
    val data = Stream.continually(in.readLine).takeWhile(_ != null).map(line => {
      val fields = line.split(",")
      RawInstance(fields(0), fields(1), fields.drop(2).map(_.toDouble).toList)
    })
    return ScrapedData(features.toList, data.toList)
  }

  // Throws out OSM label
  def join(other: ScrapedData, osm: ScrapedData): ScrapedData = {
    val osm_by_id = osm.data.map(inst => inst.osm_id -> inst).toMap
    return ScrapedData(osm.feature_names, other.data.map(
      inst => inst.copy(features = osm_by_id(inst.osm_id).features)
    ))
  }
}
case class RawInstance(label: String, osm_id: String, features: List[Double])

case class FeatureSummary(feature: Int, interval_maxes: List[Double]) {
  def transform(x: Double): Int = {
    for ((max, bin) <- interval_maxes.zipWithIndex) {
      if (x <= max) {
        return bin
      }
    }
    // Testing instance could exceed what we saw in training. Cap it.
    return interval_maxes.size - 1
  }
}

case class DataFixer(summaries: Seq[FeatureSummary], labels: Set[String]) {
  def transform(raw: RawInstance) = LabeledInstance(
    raw.label, raw.osm_id, raw.features.zip(summaries).map(pair => pair._2.transform(pair._1))
  )
}

object Preprocessing {
  def summarize(instances: List[RawInstance], num_bins: Int): DataFixer = {
    val width = instances.size / num_bins
    val cutoff_indices = Range(1, num_bins + 1).map(x => math.min(x * width, instances.size - 1))

    val summaries = Range(0, instances.head.features.size).map(feature => {
      val sorted = instances.map(_.features(feature)).sorted
      FeatureSummary(feature, cutoff_indices.map(idx => sorted(idx)).toList)
    })
    val labels = instances.map(_.label).toSet
    /*println("Feature summaries:")
    for (s <- summaries) {
      println(s"  $s")
    }
    println("")*/
    return DataFixer(summaries, labels)
  }
}
