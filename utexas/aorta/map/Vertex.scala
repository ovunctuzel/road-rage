// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.MutableList
import java.io.Serializable

import utexas.aorta.ui.Renderable

import utexas.aorta.Util

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.Intersection

// TODO var id due to tarjan
@SerialVersionUID(1)
class Vertex(val location: Coordinate, var id: Int) extends Renderable
  with Serializable
{
  // TODO this is a temporary solution
  @transient var intersection: Intersection = null

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
    i.turns.foreach(pair => Util.log(pair._2 + " doing " + pair._1))
    Util.log_pop

    Util.log("Roads: " + roads)

    // anything else
    i.policy.dump_info
  }
}
