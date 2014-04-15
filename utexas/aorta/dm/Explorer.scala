// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import utexas.aorta.sim.Simulation
import utexas.aorta.ui.{GUI, MapCanvas}
import utexas.aorta.common.Util

import scala.collection.mutable
import utexas.aorta.experiments.{ExpConfig, SmartExperiment, Metric, MetricInfo}
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
      case "bayes" => classify_bayes(args.tail.head)
      case "test_bayes" => test_bayes()
      case "weka" => classify_weka(args.tail.head)
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

  private def classify_bayes(data_fn: String) {
    val scraped = ScrapedData.read_csv(data_fn)

    val bins = 50
    val fixer = Preprocessing.summarize(scraped.data, bins)
    val instances = scraped.data.map(r => fixer.transform(r))
    val bayes = new NaiveBayesClassifier(fixer.labels, bins)
    bayes.train(instances, Nil)
    bayes.summarize(instances)
  }

  private def test_bayes() {
    /*val scraped = ScrapedData(List("x", "y"),
      (Range(0, 50).map(_ => RawInstance("above", "id", List(rng.double(-10, 10), rng.double(1, 10))))
      ++
      Range(0, 50).map(_ => RawInstance("below", "id", List(rng.double(-10, 10), rng.double(-10, -1))))
      ).toList
    )*/

    val rng = new utexas.aorta.common.RNG()
    val instances =
      (Range(0, 50).map(_ => LabeledInstance("above", List(rng.int(0, 3), rng.int(0, 1))))
      ++
      Range(0, 50).map(_ => LabeledInstance("below", List(rng.int(0, 3), rng.int(2, 3))))
      ).toList
    val bayes = new NaiveBayesClassifier(Set("above", "below"), 4)
    bayes.train(instances, Nil)
    bayes.summarize(instances)
  }

  private def classify_weka(data_fn: String) {
    val scraped = ScrapedData.read_csv(data_fn)
    val weka = new WekaClassifier(scraped)
    weka.find_anomalies()
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
    run_trial(scenario, "delay").head.output(Nil)
  }
}

class WayDelayMetric(info: MetricInfo, osm: ScrapedData) extends Metric(info) {
  override def name = "way_delay"

  // key is osm ID
  private val delay_per_way = new mutable.HashMap[String, Double]().withDefault(_ => 0)
  // for getting average
  private val count_per_way = new mutable.HashMap[String, Int]().withDefault(_ => 0)

  private val entry_time = new mutable.HashMap[Agent, Double]()

  info.sim.listen(classOf[EV_Transition], _ match {
    // Entering a road
    case EV_Transition(a, from: Turn, to) => entry_time(a) = a.sim.tick
    // Exiting a road that we didn't spawn on
    case EV_Transition(a, from: Edge, to: Turn) if entry_time.contains(a) => {
      delay_per_way(from.road.osm_id) += a.sim.tick - entry_time(a)
      count_per_way(from.road.osm_id) += 1
    }
    case _ =>
  })

  // Ignore args, just ourself
  override def output(ls: List[Metric]) {
    val actual_delays = delay_per_way.keys.map(id => id -> delay_per_way(id) / count_per_way(id)).toMap
    val sorted_delays = actual_delays.values.toArray.sorted
    val n = sorted_delays.size
    val low_delay_cap = sorted_delays((n * (1.0 / 3)).toInt)
    val mid_delay_cap = sorted_delays((n * (2.0 / 3)).toInt)
    println(s"Delay caps: $low_delay_cap, $mid_delay_cap, ${sorted_delays.last}")
    val instances = actual_delays.keys.map(id => {
      val delay = actual_delays(id)
      // What percentile is it in?
      val label =
        if (delay <= low_delay_cap)
          "low"
        else if (delay <= mid_delay_cap)
          "mid"
        else
          "high"
      RawInstance(label, id, Nil)
    })
    val all_data = ScrapedData.join_delays(ScrapedData(Nil, instances.toList), osm)
    all_data.save_csv("dm_delay_" + info.sim.graph.basename + ".csv")
  }
}
