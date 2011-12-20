package utexas.map

import java.io.FileWriter

import scala.collection.mutable.MutableList
import scala.collection.immutable.HashSet

// TODO var id due to tarjan
class Vertex(val location: Coordinate, var id: Int) {
  // TODO we could keep a map for faster lookup, sure
  var turns = new MutableList[Turn]

  // TODO construction sucks
  def turns_from(from: Edge): List[Turn] = turns.toList.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.toList.filter(_.to == to)

  // what verts lead to this one?
  def in_verts = HashSet() ++ turns.map(t => t.from.from)
  // what verts does this one lead to?
  def out_verts = HashSet() ++ turns.map(t => t.to.to)

  def roads = HashSet() ++ turns.flatMap(t => List(t.from.road, t.to.road))
  def edges = turns.flatMap(t => List(t.from, t.to))

  // these may prove useful; just remember roads are undirected.
  // TODO test for one-ways?
  private def find_roads(r: Road, types: Set[TurnType.TurnType]): Set[Road] = {
    return HashSet() ++ turns.filter(
      t => (t.involves_road(r) && types(t.turn_type))
    ).map(t => t.other_road(r))
  }
  def parallel_roads(r: Road) = find_roads(r, Set(TurnType.CROSS))
  def perp_roads(r: Road)     = find_roads(r, Set(TurnType.LEFT, TurnType.RIGHT))
  def left_roads(r: Road)     = find_roads(r, Set(TurnType.LEFT))
  def right_roads(r: Road)    = find_roads(r, Set(TurnType.RIGHT))

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
