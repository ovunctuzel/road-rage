// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import scala.collection.mutable

import utexas.aorta.map.{Graph, Road, FreeflowRouter}
import utexas.aorta.common.{RNG, Util}

// Directed graph
class OsmGraph(
  graph: Graph, val ways: Set[OsmWay], val succs: Map[OsmWay, Set[OsmWay]],
  val preds: Map[OsmWay, Set[OsmWay]], val lengths: Map[OsmWay, Double]
) {
  def pagerank(): Map[OsmWay, Double] = {
    val iterations = 50
    val alpha = .15
    // TODO seed initial_weight with our own notion of importance?
    val initial_weight = 1.0 / ways.size
    val rank_source = alpha / ways.size
    var rank = ways.map(w => w -> initial_weight).toMap
    for (i <- Range(0, iterations)) {
      println(s"Computing PageRank, iteration $i of $iterations...")
      val new_rank = ways.map(w => w ->
        (rank_source + (1.0 - alpha) * preds(w).map(pred => rank(pred) / succs(pred).size).sum)
      ).toMap
      val sum_ranks = new_rank.values.sum
      rank = new_rank.mapValues(x => x / sum_ranks)
    }
    return rank
  }

  def popular_ways(): Map[OsmWay, Double] = {
    def way(r: Road) = OsmWay(r.osm_id, r.road_type)

    val num_routes = 3000
    val router = new FreeflowRouter(graph)
    val rng = new RNG()
    val frequency = new mutable.HashMap[OsmWay, Double]()
    ways.foreach(w => frequency(w) = 0)
    for (i <- Range(0, num_routes)) {
      if (i % 500 == 0) {
        println(s"Visualizing popular roads, $i / $num_routes...")
      }
      for (r <- router.path(rng.choose(graph.roads), rng.choose(graph.roads)).path) {
        frequency(way(r)) += 1
      }
    }
    return frequency.toMap
  }

  def convert_costs(costs: Map[OsmWay, Double]): Map[Road, Double] = {
    val costs_by_id = costs.map({ case (k, v) => k.id -> v}).toMap
    return graph.roads.map(r => r -> costs_by_id(r.osm_id)).toMap
  }

  def scrape_data(): ScrapedData = {
    val popularity = popular_ways()
    val rank = pagerank()
    val data = ways.map(way => {
      val f1 = lengths(way)
      val f2 = succs(way).size
      val f3 = popularity(way)
      val f4 = rank(way)
      RawInstance(way.label, List(f1, f2, f3, f4))
    }).toList
    return ScrapedData(List("length", "num_intersections", "popularity", "pagerank"), data)
  }
}
case class OsmWay(id: String, label: String)

object OsmGraph {
  def convert(orig: Graph): OsmGraph = {
    def way(r: Road) = OsmWay(r.osm_id, r.road_type)
    Util.log("Converting AORTA graph to OSM graph...")

    val ways = orig.roads.map(way).toSet
    val succs = ways.map(w => w -> new mutable.HashSet[OsmWay]()).toMap
    val preds = ways.map(w => w -> new mutable.HashSet[OsmWay]()).toMap
    val lengths = new mutable.HashMap[OsmWay, Double]()
    ways.foreach(w => lengths(w) = 0)
    for (r <- orig.roads) {
      succs(way(r)) ++= r.succs.map(way)
      preds(way(r)) ++= r.preds.map(way)
      lengths(way(r)) += r.length
    }
    return new OsmGraph(orig, ways, succs.mapValues(_.toSet), preds.mapValues(_.toSet), lengths.toMap)
  }
}
