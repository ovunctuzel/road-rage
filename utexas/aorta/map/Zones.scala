// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.map.analysis.Router
import utexas.aorta.sim.make.RouterType
import utexas.aorta.ui.Renderable
import utexas.aorta.common.algorithms.AStar
import utexas.aorta.common.Util

class ZoneMap(graph: Graph) {
  private val mapping = ZoneMap.partition(graph)
  val zones = mapping.values.toSet
  Util.log(s"${graph.roads.size} DRs partitioned into ${zones.size} zones")
  val links = zones.map(zone => zone -> (zone.ports.map(r => mapping(r)).toSet - zone)).toMap

  def apply(r: Road) = mapping(r)

  def zone_path(start: Zone, goal: Zone) = AStar.path(
    start, goal, (step: Zone) => links(step),
    (_: Zone, next: Zone, _: (Double, Double)) => (next.freeflow_time, 0),
    (state: Zone, goal: Zone) => (state.center.dist_to(goal.center), 0),
    (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2)
  )

  def router = new Router(graph) {
    override def router_type = RouterType.Unusable
    override def path(from: Road, to: Road, time: Double): List[Road] = {
      // TODO this router should lazily expand intra-zone steps
      zone_path(mapping(from), mapping(to))
      return Nil
    }
  }
}

class Zone(val roads: Set[Road]) extends Renderable {
  val center = compute_center
  // Member roads that have successors outside the set
  // TODO since partition isnt disjoint, misses some connections
  val ports = roads.filter(r => r.succs.exists(succ => !roads.contains(succ)))

  private def compute_center(): Coordinate = {
    val pts = roads.map(r => r.rightmost.approx_midpt)
    val avg_x = pts.map(_.x).sum / roads.size
    val avg_y = pts.map(_.y).sum / roads.size
    return new Coordinate(avg_x, avg_y)
  }

  override def debug() {
    Util.log(s"Zone with ${roads.size} roads and ${ports.size} ports")
  }

  def freeflow_time = 1.0 // TODO how to define the approx time to cross this zone?
}

object ZoneMap {
  private val max_size = 50

  def partition(graph: Graph): Map[Road, Zone] = {
    Util.log("Partitioning the map into zones...")
    val mapping = new mutable.HashMap[Road, Zone]()
    val open = new mutable.TreeSet[Road]()  // TODO queue?
    open ++= graph.roads
    while (open.nonEmpty) {
      Util.log(s"  ${open.size} roads left to process")
      val base = open.head
      // TODO need an easier way to call this beast!
      val path = AStar.path(
        base, base, (step: Road) => step.succs,
        (_: Road, next: Road, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: Road, goal: Road) => (state.end_pt.dist_to(goal.end_pt), 0),
        (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
        allow_cycles = true
      )
      val new_zone = new mutable.HashSet[Road]()
      new_zone ++= path
      open --= new_zone

      // Merge zones by finding common overlap
      // TODO dont cross different road types?
      val common_roads = new_zone.intersect(mapping.keys.toSet)
      while (new_zone.size < max_size && common_roads.nonEmpty) {
        // TODO should keep common_roads up-to-date as we merge
        val candidate = common_roads.head
        common_roads -= candidate
        if ((new_zone ++ mapping(candidate).roads).size < max_size) {
          new_zone ++= mapping(candidate).roads
        }
      }
      val zone = new Zone(new_zone.toSet)
      // TODO overwrites. make the partitioning disjt.
      zone.roads.foreach(r => mapping(r) = zone)
    }
    return mapping.toMap
  }
}
