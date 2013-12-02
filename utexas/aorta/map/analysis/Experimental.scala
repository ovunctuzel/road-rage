// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.common.Util
import utexas.aorta.common.algorithms.AStar

// TODO main issue: zones are linear. have to start in the right spot to have actual path.
case class Strand(members: List[DirectedRoad])

// Group directed roads by same OSM ID, and use that for faster pathfinding
object CompressedGraph {
  def form_strands(graph: Graph): List[Strand] = {
    val strands = new mutable.ListBuffer[Strand]()
    val unused_drs = new mutable.HashSet[DirectedRoad]()
    unused_drs ++= graph.directed_roads
    // TODO to have less strands, start "at the beginning" of roads
    while (unused_drs.nonEmpty) {
      val start = unused_drs.head
      unused_drs -= start
      strands += flood(start, unused_drs)
    }
    Util.log(s"Graph with ${graph.directed_roads.size} directed roads has ${strands.size} strands")
    return strands.toList
  }

  private def flood(from: DirectedRoad, unused: mutable.Set[DirectedRoad]): Strand = {
    val members = new mutable.ListBuffer[DirectedRoad]()
    members += from
    var current = from
    while (true) {
      // Each DR should lead to just one other DR of the same OSM ID
      current.succs.find(dr => dr.road.osm_id == current.road.osm_id) match {
        case Some(next) if unused.contains(next) => {
          members += next
          unused -= next
          current = next
        }
        case _ => return Strand(members.toList)
      }
    }
    throw new Exception("satisfy type inference") // TODO rewrite more functionally to avoid
  }
}

class CompressedGraph(strands: List[Strand]) {
  private val dr_to_strand = strands.flatMap(strand => strand.members.map(dr => dr -> strand)).toMap
  private val connections: Map[Strand, Set[Strand]] =
    (for (strand <- strands)
      yield strand -> strand.members.flatMap(_.succs).map(dr => dr_to_strand(dr)).toSet
    ).toMap

  private def add_pairs(a: (Double, Double), b: (Double, Double)) = (a._1 + b._1, a._2 + b._2)

  def path(from: DirectedRoad, to: DirectedRoad) = AStar.path(
    dr_to_strand(from), dr_to_strand(to), (step: Strand) => connections(step),
    // Uniform cost search
    (prev: Strand, next: Strand, cost_sofar: (Double, Double)) => (1, 0),
    // No heuristic
    (from: Strand, goal: Strand) => (0, 0),
    add_pairs
  )
}
