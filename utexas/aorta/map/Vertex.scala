// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import scala.collection.mutable.{MutableList, ListBuffer}
import scala.collection.mutable.{HashSet => MutableSet}

// TODO var id due to tarjan
class Vertex(val location: Coordinate, var id: Int) {
  // TODO we could keep a map for faster lookup, sure, but determinism's cool
  // too.
  var turns = new MutableList[Turn]

  // TODO construction sucks
  def turns_from(from: Edge): List[Turn] = turns.toList.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.toList.filter(_.to == to)

  // what verts lead to this one?
  def in_verts = turns.map(t => t.from.from).toSet
  // what verts does this one lead to?
  def out_verts = turns.map(t => t.to.to).toSet

  def roads = turns.flatMap(t => List(t.from.road, t.to.road)).toSet
  def edges = turns.flatMap(t => List(t.from, t.to))
  def in_edges = turns.map(t => t.from)

  // these may prove useful; just remember roads are undirected.
  // TODO test for one-ways?
  private def find_roads(r: Road, types: Set[TurnType.TurnType]): Set[Road] = {
    return turns.filter(
      t => (t.involves_road(r) && types(t.turn_type))
    ).map(t => t.other_road(r)).toSet
  }
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

// A clump of propinquous vertices logically grouped
class UberSection(verts: List[Vertex]) {
  val uber_turns = UberSection.make_turns(verts)

  def to_xml(out: FileWriter) = {
    out.write("  <ubersection verts=\"" + verts.map(_.id).mkString(",") + "\">\n")
    uber_turns.foreach(t => t.to_xml(out))
    out.write("  </ubersection/>\n")
  }
}

object UberSection {
  var next_id = 0

  def make_turns(verts: List[Vertex]): List[UberTurn] = {
    // find all turns incoming into this clump
    val in_turns = verts.flatMap(v => v.turns).filter(t => !verts.contains(t.from.from))

    // for each in-turn, flood from it till we leave the clump. remember every
    // path as an uberturn.
    return in_turns.flatMap(t => flood_turns(t, verts))

    // figure out conflicts between uberturns. this is based on the idea we may
    // want to precompute a matrix of this once instead of computing at runtime
    /*val conflicts = new ListBuffer[(UberTurn, UberTurn)]()
    for (uber1 <- uber_turns) {
      // conflicts are symmetric, so avoid repeats
      for (uber2 <- uber_turns if uber1.id < uber2.id) {
        // TODO quite slow and produces... lots of conflicts
        if (uber1.does_conflict(uber2)) {
          conflicts += ((uber1, uber2))
        }
      }
    }*/
  }

  private def flood_turns(from: Turn, clump: List[Vertex]): List[UberTurn] = {
    val uber_turns = new ListBuffer[UberTurn]()
    val visited = new MutableSet[Turn]()
    visited += from
    // a queue of paths, given by a list of turns
    var queue: List[List[Turn]] = List(List(from))
    while (!queue.isEmpty) {
      val cur_path = queue.head
      queue = queue.tail
      //uber_turns += new UberTurn(next_uber_id, cur_path)
      //next_uber_id += 1

      // expand unless we're already at the border
      if (clump.contains(cur_path.last.to.to)) {
        for (next <- cur_path.last.to.next_turns) {
          if (!visited(next)) {
            visited += next
            // not efficient to append to end of list, but these paths will be
            // short
            queue = (cur_path ++ List(next)) :: queue
          }
        }
      } else {
        // only add uberturns from outside->outside ubersection?
        uber_turns += new UberTurn(UberSection.next_id, cur_path)
        UberSection.next_id += 1
      }
    }
    return uber_turns.toList
  }
}
