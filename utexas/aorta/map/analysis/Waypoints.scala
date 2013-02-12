// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import utexas.aorta.map.{Graph, Road, DirectedRoad}

import utexas.aorta.Util

object WaypointGenerator {
  def big_intersections(graph: Graph) = graph.vertices.filter(
    v => !v.roads.find(r => r.road_type == "residential").isDefined
  )

  // A terribly dumb heuristic
  def choose_waypoints(graph: Graph) =
    big_intersections(graph).map(v => v.out_edges.head.directed_road)
}

class Waypoint(base: DirectedRoad) {
  val costs_to_waypt = base.costs_to
  val costs_from_waypt = base.costs_from
}
