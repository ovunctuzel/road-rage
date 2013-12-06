// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import Function.tupled

import utexas.aorta.map.analysis.Router
import utexas.aorta.sim.make.RouterType
import utexas.aorta.ui.Renderable
import utexas.aorta.common.algorithms.AStar
import utexas.aorta.common.{Util, ZoneID}

class ZoneMap(graph: Graph) {
  private val mapping: Map[Road, Set[Zone]] = ZoneMap.partition(graph)
  val zones: Set[Zone] = mapping.values.flatten.toSet
  val links: Map[Zone, Set[Zone]] = zones.map(zone => zone -> zone_succs(zone)).toMap
  Util.log("%,d DRs partitioned into %,d zones with %,d connections".format(
    graph.roads.size, zones.size, links.values.map(_.size).sum
  ))
  // Roads lead to different zones via ports, and some roads are in many zones. No self-cycles.
  private def zone_succs(zone: Zone) = zone.ports.flatMap(_.succs).flatMap(r => mapping(r)).toSet ++
    zone.roads.flatMap(r => mapping(r)) - zone

  def apply(r: Road): Set[Zone] = mapping(r)

  def zone_path(start: Zone, goal: Zone) = AStar.path(
    start, goal, (step: Zone) => links(step),
    (_: Zone, next: Zone, _: (Double, Double)) => (next.freeflow_time, 0),
    (state: Zone, goal: Zone) => (state.center.dist_to(goal.center), 0),
    (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2)
  )

  def router = new Router(graph) {
    // TODO this router should lazily expand intra-zone steps
    override def router_type = RouterType.Unusable
    override def path(from: Road, to: Road, time: Double): List[Road] = {
      // TODO ooh, how to tell which zone we "start" in?
      zone_path(mapping(from).head, mapping(to).head)
      return Nil
    }
  }
}

class Zone(val id: ZoneID, val roads: Set[Road]) extends Renderable {
  val center = compute_center
  // Member roads that have successors outside the set. Members shared with other zones aren't
  // included.
  val ports: Set[Road] = roads.filter(r => r.succs.exists(succ => !roads.contains(succ)))

  override def toString = s"Z$id"

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
  private val max_size = 350

  // TODO dont cross different road types?
  // TODO possibly nondeterministic, use trees?
  def partition(graph: Graph): Map[Road, Set[Zone]] = {
    Util.log("Partitioning the map into zones...")

    // Since "zones" are mutable during the building process, introduce a layer of introduction via
    // temporary zone ID ints.
    var next_id = 0
    val zone_members = new mutable.HashMap[ZoneID, mutable.Set[Road]]()
    val road_mapping = graph.roads.map(r => r -> new mutable.HashSet[ZoneID]()).toMap

    val open = new mutable.TreeSet[Road]()  // TODO queue?
    open ++= graph.roads
    while (open.nonEmpty) {
      Util.log(s"  ${open.size} roads left to process")
      val base = open.head
      // TODO need an easier way to call this beast!
      val path = AStar.path(
        base, base, (step: Road) => step.succs,
        // TODO still try to discourage overlap here? try it out!
        (_: Road, next: Road, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: Road, goal: Road) => (state.end_pt.dist_to(goal.end_pt), 0),
        (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
        allow_cycles = true
      )
      val new_zone = new mutable.HashSet[Road]()
      new_zone ++= path
      open --= new_zone

      // Merge zones by finding common overlap
      val overlapping_zones = path.flatMap(r => road_mapping(r)).toSet
      for (candidate <- overlapping_zones) {
        if ((new_zone ++ zone_members(candidate)).size < max_size) {
          new_zone ++= zone_members(candidate)
          // Since we absorbed this old zone in the new one, delete the existence of the old.
          zone_members(candidate).foreach(r => road_mapping(r) -= candidate)
          zone_members -= candidate
        }
      }
      val new_id = new ZoneID(next_id + 1)
      next_id += 1
      zone_members(new_id) = new_zone
      new_zone.foreach(r => road_mapping(r) += new_id)
    }
    val zones = zone_members.keys.zipWithIndex.map(
      tupled((id, idx) => id -> new Zone(new ZoneID(idx), zone_members(id).toSet))
    ).toMap
    return graph.roads.map(r => r -> road_mapping(r).map(id => zones(id)).toSet).toMap
  }
}
