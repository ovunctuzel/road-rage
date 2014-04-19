// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import utexas.aorta.sim.Simulation
import utexas.aorta.ui.{GUI, MapCanvas}
import utexas.aorta.common.Util

import scala.collection.mutable
import utexas.aorta.experiments.{ExpConfig, SmartExperiment, Metric, MetricInfo, ScenarioPresets}
import utexas.aorta.sim.EV_Transition
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.map.{Edge, Turn}

object Explorer {
  def main(args: Array[String]) {
    args.head match {
      case "gui" => {
        val canvas = new MapCanvas(Util.process_args(args.tail))
        setup_gui(canvas)
        GUI.run(canvas)
      }
      case "scrape_osm" => scrape_osm(Util.process_args(args.tail))
      case "bayes" => classify_bayes(args.tail.head, args.tail.tail.headOption)
      case "scrape_delay" => scrape_delay(args.tail.head)
    }
  }

  private def setup_gui(canvas: MapCanvas) {
    val graph = canvas.sim.graph
    val osm = OsmGraph.convert(graph)
    val percentile = 1.0

    Util.log("Creating heatmap for way length...")
    canvas.show_heatmap(osm.convert_costs(osm.lengths), percentile, "way length")
    Util.log("Creating heatmap for number of connections...")
    canvas.show_heatmap(
      osm.convert_costs(osm.succs.mapValues(_.size.toDouble)), percentile,
      "number of connections"
    )
    Util.log("Creating heatmap for popularity...")
    canvas.show_heatmap(
      osm.convert_costs(osm.popular_ways), percentile, "popularity of way in shortest path"
    )
    Util.log("Creating heatmap for PageRank...")
    canvas.show_heatmap(osm.convert_costs(osm.pagerank), percentile, "pagerank")
  }

  private def scrape_osm(sim: Simulation) {
    val osm = OsmGraph.convert(sim.graph)
    val scraped = osm.scrape_data()
    scraped.save_csv("dm_osm_" + sim.graph.basename + ".csv")
  }

  private def classify_bayes(data_fn: String, debug_road: Option[String]) {
    val scraped = ScrapedData.read_csv(data_fn)

    val bins = 50
    val fixer = Preprocessing.summarize(scraped.data, bins)
    val instances = scraped.data.map(r => fixer.transform(r))
    val bayes = new NaiveBayesClassifier(fixer.labels, bins)
    bayes.train(instances)
    debug_road match {
      case Some(rd) => {
        println(rd + " is classified as " + bayes.classify(instances.find(_.osm_id == rd).get.for_test))
      }
      case None => {
        bayes.summarize(instances)
        bayes.find_anomalies(instances, data_fn.stripPrefix("dm_osm_").stripSuffix(".csv"))
      }
    }
  }

  private def scrape_delay(map_fn: String) {
    val basename = map_fn.split("/").last.split(".map").head
    new DelayExperiment(
      ExpConfig.dm_delay(map_fn), ScrapedData.read_csv("dm_osm_" + basename + ".csv")
    ).run_experiment()
  }
}

// TODO move to own file?
class DelayExperiment(config: ExpConfig, osm: ScrapedData) extends SmartExperiment(config, "delay") {
  override def get_metrics(info: MetricInfo) = List(new WayDelayMetric(info, osm))

  override def run() {
    run_trial(ScenarioPresets.transform(scenario, "dm_stable"), "delay").head.output(Nil)
  }
}

class WayDelayMetric(info: MetricInfo, osm: ScrapedData) extends Metric(info) {
  override def name = "way_delay"

  // key is osm ID
  private val delay_per_way = new mutable.HashMap[String, Double]().withDefault(_ => 0)
  private val time_per_way = new mutable.HashMap[String, Double]().withDefault(_ => 0)
  // for getting average
  private val count_per_way = new mutable.HashMap[String, Int]().withDefault(_ => 0)

  private val entry_time = new mutable.HashMap[Agent, Double]()

  info.sim.listen(classOf[EV_Transition], _ match {
    // Entering a road
    case EV_Transition(a, from: Turn, to) => entry_time(a) = a.sim.tick
    // Exiting a road that we didn't spawn on
    case EV_Transition(a, from: Edge, to: Turn) if entry_time.contains(a) => {
      // TODO why +1? when we enter a road, we technically sometime between the previous tick and
      // now. so just round up a bit.
      val t = a.sim.tick - entry_time(a)
      delay_per_way(from.road.osm_id) += t - from.road.freeflow_time + 1
      time_per_way(from.road.osm_id) += t
      count_per_way(from.road.osm_id) += 1
    }
    case _ =>
  })

  // Ignore args, just ourself
  override def output(ls: List[Metric]) {
    for ((source, out_name) <- List((delay_per_way, "delay"), (time_per_way, "time"))) {
      val actual = source.keys.map(id => id -> source(id) / count_per_way(id)).toMap
      val sorted = actual.values.toArray.sorted
      val n = sorted.size
      val low_cap = sorted((n * (1.0 / 3)).toInt)
      val mid_cap = sorted((n * (2.0 / 3)).toInt)
      println(out_name)
      //println(sorted.toList)
      println(s"$out_name caps: $low_cap, $mid_cap, ${sorted.last}")
      val instances = actual.keys.map(id => {
        val value = actual(id)
        // What percentile is it in?
        val label =
          if (value <= low_cap)
            "low"
          else if (value <= mid_cap)
            "mid"
          else
            "high"
        RawInstance(label, id, Nil)
      })
      val all_data = ScrapedData.join(ScrapedData(Nil, instances.toList), osm)
      all_data.save_csv(s"dm_${out_name}_${info.sim.graph.basename}.csv")
    }
  }
}
