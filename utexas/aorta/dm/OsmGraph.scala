// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import scala.collection.mutable

import utexas.aorta.map.{Graph, Road, FreeflowRouter}
import utexas.aorta.common.RNG

// Directed graph
class OsmGraph(
  ways: Set[OsmWay], connections: Map[OsmWay, Set[OsmWay]], lengths: Map[OsmWay, Double]
) {
  def succs(way: OsmWay) = connections(way)
  def preds(way: OsmWay) = connections.keys.filter(w => connections(w).contains(way)) // TODO slow?

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

  def popular_ways(graph: Graph): Map[OsmWay, Double] = {
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

  def convert_costs(costs: Map[OsmWay, Double], graph: Graph): Map[Road, Double] = {
    val costs_by_id = costs.map({ case (k, v) => k.id -> v}).toMap
    return graph.roads.map(r => r -> costs_by_id(r.osm_id)).toMap
  }
}
case class OsmWay(id: String, label: String)

object OsmGraph {
  def convert(orig: Graph): OsmGraph = {
    def way(r: Road) = OsmWay(r.osm_id, r.road_type)

    val ways = orig.roads.map(way).toSet
    val connections = ways.map(w => w -> new mutable.HashSet[OsmWay]()).toMap
    val lengths = new mutable.HashMap[OsmWay, Double]()
    ways.foreach(w => lengths(w) = 0)
    for (r <- orig.roads) {
      connections(way(r)) ++= r.succs.map(way)
      lengths(way(r)) += r.length
    }
    return new OsmGraph(ways, connections.mapValues(_.toSet), lengths.toMap)
  }
}
