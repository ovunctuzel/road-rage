// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.MutableList
import Function.tupled

import utexas.aorta.ui.Renderable

import utexas.aorta.common.{Util, StateWriter, StateReader}

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.Intersection

// TODO var id due to tarjan
class Vertex(val location: Coordinate, var id: Int) extends Renderable
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  // TODO we could keep a map for faster lookup
  var turns: List[Turn] = Nil

  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // TODO messy to have this dependency here.
  var intersection: Intersection = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.double(location.x)
    w.double(location.y)
    w.int(id)
    w.int(turns.length)
    turns.foreach(t => t.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def turns_from(from: Edge): List[Turn] = turns.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.filter(_.to == to)
  def edges_to(to: DirectedRoad): List[Edge] =
    turns.filter(_.to.directed_road == to).map(_.from)

  // what verts lead to this one?
  def in_verts = turns.map(t => t.from.from).toSet
  // what verts does this one lead to?
  def out_verts = turns.map(t => t.to.to).toSet

  def roads = turns.flatMap(t => List(t.from.road, t.to.road)).toSet
  def directed_roads_in = turns.map(_.from.directed_road).toSet
  def edges = turns.flatMap(t => List(t.from, t.to))
  def in_edges = turns.map(t => t.from).toSet
  def out_edges = turns.map(t => t.to).toSet

  // Priority of a vertex is the sum of speed limits of roads surrounding it
  def get_priority = roads.foldLeft(0.0)((a,b) => a + b.speed_limit)

  override def toString = "[V" + id + "]"

  override def equals(other: Any) = other match {
    case other: Vertex => { id == other.id }
    case _ => false
  }

  def debug = {
    Util.log(this + " at " + location)

    val i = intersection

    Util.log("Current turns allowed:")
    Util.log_push
    i.policy.current_greens.foreach(g => Util.log("" + g))
    Util.log_pop
                                                                        
    Util.log("Current turns active:")                                   
    Util.log_push
    i.turns.foreach(tupled((turn, count) => Util.log(s"$count doing $turn")))
    Util.log_pop

    Util.log("Roads: " + roads)

    // anything else
    i.policy.dump_info
  }
}

object Vertex {
  def unserialize(r: StateReader): Vertex = {
    val v = new Vertex(new Coordinate(r.double, r.double), r.int)
    v.turns ++= Range(0, r.int).map(_ => Turn.unserialize(r))
    return v
  }
}
