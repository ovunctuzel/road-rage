// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.{mutable, immutable}
import Function.tupled

import utexas.aorta.map.analysis.Router
import utexas.aorta.sim.make.RouterType
import utexas.aorta.ui.Renderable
import utexas.aorta.common.algorithms.AStar
import utexas.aorta.common.{Util, ZoneID}

// TODO consider dropping the requirement that zones have to be connected. allows for more flexible
// shapes, and for disjoint partitioning.

class ZoneMap(
  val zones: Array[Zone], mapping: Map[Road, immutable.SortedSet[Zone]],
  val links: Map[Zone, immutable.SortedSet[Zone]]
) {
  def apply(r: Road): immutable.SortedSet[Zone] = mapping(r)
  // Since the sets are sorted by ID, this'll be deterministic
  def canonical(r: Road) = mapping(r).head
}

class Zone(val id: ZoneID, val roads: immutable.SortedSet[Road], val center: Coordinate)
  extends Ordered[Zone] with Renderable
{
  override def toString = s"Z$id"
  override def compare(other: Zone) = id.int.compare(other.id.int)

  def freeflow_time = 1.0 // TODO how to define the approx time to cross this zone?

  override def debug() {
    Util.log(s"Zone with ${roads.size}")
  }
}

object ZoneMap {
  private val max_size = 350

  def create(graph: Graph): ZoneMap = {
    val mapping = partition(graph)

    // Roads lead to different zones via ports, and some roads are in many zones. No self-cycles.
    def zone_succs(zone: Zone) = Util.sorted_set(
      ports(zone).flatMap(_.succs).flatMap(r => mapping(r)) ++ zone.roads.flatMap(r => mapping(r))
      - zone
    )
    // Member roads that have successors outside the set. Members shared with other zones aren't
    // included.
    def ports(zone: Zone)
      = zone.roads.filter(r => r.succs.exists(succ => !zone.roads.contains(succ)))

    val zones = mapping.values.flatten.toSet.toArray.sortBy(_.id.int)
    val links = zones.map(zone => zone -> zone_succs(zone)).toMap
    Util.log("%,d DRs partitioned into %,d zones with %,d connections".format(
      graph.roads.size, zones.size, links.values.map(_.size).sum
    ))
    return new ZoneMap(zones, mapping, links)
  }

  // TODO dont cross different road types?
  // TODO possibly nondeterministic, use trees?
  private def partition(graph: Graph): Map[Road, immutable.SortedSet[Zone]] = {
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
      val path = AStar.path(
        base, Set(base), (step: Road) => step.succs,
        (_: Road, next: Road, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: Road) => (state.end_pt.dist_to(base.end_pt), 0),
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
      tupled((id, idx) => id -> new Zone(
        new ZoneID(idx), Util.sorted_set(zone_members(id)), compute_center(zone_members(id))
      ))
    ).toMap
    return graph.roads.map(r => r -> Util.sorted_set(road_mapping(r).map(id => zones(id)))).toMap
  }

  private def compute_center(roads: Iterable[Road]): Coordinate = {
    val pts = roads.map(r => r.rightmost.approx_midpt)
    val avg_x = pts.map(_.x).sum / roads.size
    val avg_y = pts.map(_.y).sum / roads.size
    return new Coordinate(avg_x, avg_y)
  }
}

// Lazily computes an actual path to get through each zone
class ZoneRouter(graph: Graph) extends Router(graph) {
  private val zone_map = graph.zones
  // TODO savestate
  // Caching saves some time, and it also avoids bouncing between local maxima
  private var zpath: List[Zone] = Nil

  override def router_type = RouterType.Zone

  override def path(from: Road, to: Road, time: Double): List[Road] = {
    if (zone_map(from).intersect(zone_map(to)).nonEmpty) {
      // In the same zone! Be normal now.
      return AStar.path(
        from, Set(to), (step: Road) => step.succs,
        // TODO heuristics? congestion?
        (_: Road, next: Road, _: (Double, Double)) => (next.freeflow_time, 0),
        (state: Road) => (state.end_pt.dist_to(to.end_pt), 0)
      )
    } else {
      val current_zone = zone_map.canonical(from)
      var path_tail = zpath.span(z => z != current_zone)._2
      if (path_tail.headOption.getOrElse(null) != current_zone) {
        zpath = current_zone :: zone_path(current_zone, zone_map.canonical(to))
        path_tail = zpath
      }
      // The "next" zone might not be straightforward since roads overlap
      val target_zone = path_tail.find(z => !z.roads.contains(from)).get
      // Route to anywhere in the target zone, but stay in the current zone
      return AStar.path(
        from, target_zone.roads, (step: Road) => step.succs,
        // TODO heuristics? congestion?
        (_: Road, next: Road, _: (Double, Double)) =>
          (Util.bool2binary(!current_zone.roads.contains(next)), next.freeflow_time),
        (state: Road) => (0, state.end_pt.dist_to(target_zone.center))
      )
    }
  }

  // Doesn't include start as the first zone
  private def zone_path(start: Zone, goal: Zone) = AStar.path(
    start, Set(goal), (step: Zone) => zone_map.links(step),
    // TODO some notion of congestion within a zone
    (_: Zone, next: Zone, _: (Double, Double)) => (next.freeflow_time, 0),
    (state: Zone) => (state.center.dist_to(goal.center), 0)
  )
}
