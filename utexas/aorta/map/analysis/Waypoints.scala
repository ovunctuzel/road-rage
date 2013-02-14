// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import java.io.Serializable

import utexas.aorta.map.{Graph, Road, DirectedRoad}
import utexas.aorta.sim.Agent

import utexas.aorta.Util

object WaypointGenerator {
  // TODO this heuristic is completely bogus
  def big_intersections(graph: Graph) = graph.vertices.filter(
    v => !v.roads.find(r => r.road_type == "residential").isDefined
  ).take(10).toList

  def choose_waypoints(graph: Graph) =
    big_intersections(graph).map(v => v.out_edges.head.directed_road)
}

@SerialVersionUID(1)
// TODO make these case classes, yo?
class Waypoint(base_id: Int) extends Serializable {
  def base = Agent.sim.directed_roads(base_id)
  val costs_to_waypt = base.costs_to
  val costs_from_waypt = base.costs_from

  // By visiting this waypoint in between, of course
  def dist_btwn(from: DirectedRoad, to: DirectedRoad) =
    costs_to_waypt(from.id) + costs_from_waypt(to.id)

  // TODO assume from != base
  def step_to_waypt(from: DirectedRoad) =
    from.next_roads.minBy(r => costs_to_waypt(r.id))

  def path_to_waypt(from: DirectedRoad) =
    AbstractGraph.hillclimb(costs_to_waypt, from).asInstanceOf[List[DirectedRoad]]

  def path_from_waypt(to: DirectedRoad) =
    AbstractGraph.hillclimb_backwards(costs_from_waypt, to).reverse.asInstanceOf[List[DirectedRoad]]

  // Trim the common overlap, which is waypt
  def path_btwn(from: DirectedRoad, to: DirectedRoad) =
    path_to_waypt(from) ++ path_from_waypt(to).tail
}

@SerialVersionUID(1)
class WaypointRouter(waypoints: List[Waypoint]) extends Serializable {
  def best_waypt(from: DirectedRoad, to: DirectedRoad) =
    waypoints.minBy(_.dist_btwn(from, to))

  def pathfind(from: DirectedRoad, to: DirectedRoad) =
    best_waypt(from, to).path_btwn(from, to)
}
