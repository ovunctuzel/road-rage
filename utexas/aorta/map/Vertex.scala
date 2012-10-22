// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import scala.collection.mutable.MutableList

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.Intersection

// TODO var id due to tarjan
class Vertex(val location: Coordinate, var id: Int) {
  // TODO this is a temporary solution
  var intersection: Intersection = null

  // TODO we could keep a map for faster lookup, sure, but determinism's cool
  // too.
  var turns: List[Turn] = Nil

  def turns_from(from: Edge): List[Turn] = turns.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.filter(_.to == to)
  def edges_to(to: DirectedRoad): List[Edge] =
    turns.filter(_.to.directed_road == to).map(_.from)

  // what verts lead to this one?
  def in_verts = turns.map(t => t.from.from).toSet
  // what verts does this one lead to?
  def out_verts = turns.map(t => t.to.to).toSet

  def roads = turns.flatMap(t => List(t.from.road, t.to.road)).toSet
  def edges = turns.flatMap(t => List(t.from, t.to))
  def in_edges = turns.map(t => t.from)

  // these may prove useful; just remember roads are undirected.
  // TODO test for one-ways?
  private def find_roads(r: Road, types: Set[TurnType.TurnType]): Set[Road] =
    turns.filter(
      t => (t.involves_road(r) && types(t.turn_type))
    ).map(t => t.other_road(r)).toSet
  def parallel_roads(r: Road) = find_roads(r, Set(TurnType.CROSS))
  def perp_roads(r: Road)     = find_roads(r, Set(TurnType.LEFT, TurnType.RIGHT))
  def left_roads(r: Road)     = find_roads(r, Set(TurnType.LEFT))
  def right_roads(r: Road)    = find_roads(r, Set(TurnType.RIGHT))
  
  // Priority of a vertex is the sum of speed limits of roads surrounding it
  def get_priority = roads.foldLeft(0.0)((a,b) => a + b.speed_limit)

  // Somewhere due to unordered hashes, the turns list winds up in an
  // inconsistent order. To make diffs of output be the same for the same code,
  // impose an ordering on the turns, sorting first by the 'from' edge and then
  // the 'to' edge.
  def to_xml(out: FileWriter) = {
    out.write(
      "  <vertex id=\"" + id + "\" x=\"" + location.x + "\" y=\"" + location.y + "\">\n"
    )
    turns.sortBy(t => (t.from.id, t.to.id)).foreach(t => t.to_xml(out))
    out.write("  </vertex>\n")
  }

  override def toString = "[V" + id + "]"

  override def equals(other: Any) = other match {
    case other: Vertex => { id == other.id }
    case _ => false
  }
}
