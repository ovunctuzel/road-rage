// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.common.algorithms.AStar
import utexas.aorta.common.Util

class ZoneMap(graph: Graph) {
  private val mapping = ZoneMap.partition(graph)

  def apply(dr: DirectedRoad) = mapping(dr)
}

case class Zone(roads: Set[DirectedRoad])

object ZoneMap {
  def partition(graph: Graph): Map[DirectedRoad, Zone] = {
    Util.log("Partitioning the map into zones...")
    val mapping = new mutable.HashMap[DirectedRoad, Zone]()
    val open = new mutable.TreeSet[DirectedRoad]()  // TODO queue?
    open ++= graph.directed_roads
    while (open.nonEmpty) {
      Util.log(s"  ${open.size} roads left to process")
      val base = open.head
      // TODO need an easier way to call this beast!
      val path = AStar.path(
        base, base, (step: DirectedRoad) => step.succs,
        (_: DirectedRoad, next: DirectedRoad, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: DirectedRoad, goal: DirectedRoad) => (state.end_pt.dist_to(goal.end_pt), 0),
        (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
        allow_cycles = true
      ).toSet
      open --= path
      
      // Merge zones if any directed road overlaps a previously used road
      //val common_roads = path.intersect(mapping.keys.toSet)
      val common_roads = Set[DirectedRoad]()  // TODO limit merging! until then, do this...
      Util.log("  Merging zone with " + common_roads.map(dr => mapping(dr)).toSet.size + " others")
      val zone = Zone(path ++ common_roads.flatMap(dr => mapping(dr).roads))
      zone.roads.foreach(dr => mapping(dr) = zone)
    }
    return mapping.toMap
  }
}
