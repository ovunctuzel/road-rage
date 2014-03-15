// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import scala.collection.mutable

import utexas.aorta.map.{Graph, Road}

// Directed graph
class OsmGraph(ways: Set[OsmWay], connections: Map[OsmWay, Set[OsmWay]])
case class OsmWay(id: String, label: String)

object OsmGraph {
  def convert(orig: Graph): OsmGraph = {
    def way(r: Road) = OsmWay(r.osm_id, r.road_type)

    val ways = orig.roads.map(way).toSet
    val connections = ways.map(w => w -> new mutable.HashSet[OsmWay]()).toMap
    for (r <- orig.roads) {
      connections(way(r)) ++= r.succs.map(way)
    }
    return new OsmGraph(ways, connections.mapValues(_.toSet))
  }
}
