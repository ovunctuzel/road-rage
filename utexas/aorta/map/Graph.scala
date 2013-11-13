// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.common.{Util, Common, StateWriter, StateReader, RoadID,
                            VertexID, EdgeID, DirectedRoadID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex], val name: String
) extends GraphLike
{
  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  // TODO if we squish down IDs, it can be an array too!
  val turns = vertices.foldLeft(List[Turn]())(
    (l, v) => v.turns.toList ++ l
  ).map(t => t.id -> t).toMap
  var directed_roads: Array[DirectedRoad] = Array()

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.int(roads.size)
    roads.foreach(r => r.serialize(w))
    w.int(edges.size)
    edges.foreach(e => e.serialize(w))
    w.int(vertices.size)
    vertices.foreach(v => v.serialize(w))
    w.string(name)
  }

  def setup() {
    directed_roads =
      roads.flatMap(r => List(r.pos_group, r.neg_group).flatten).toArray
    for ((dr, id) <- directed_roads.zipWithIndex) {
      dr.id = new DirectedRoadID(id)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def traversables() = edges ++ turns.values

  override def get_r(id: RoadID) = roads(id.int)
  override def get_v(id: VertexID) = vertices(id.int)
  override def get_e(id: EdgeID) = edges(id.int)

  // TODO file library
  def basename = name.replace("maps/", "").replace(".map", "")

  case class Bounds(min_x: Double, min_y: Double, max_x: Double, max_y: Double)
  def bounds = Bounds(
    min_x = roads.flatMap(_.points.map(_.x)).min,
    min_y = roads.flatMap(_.points.map(_.y)).min,
    max_x = roads.flatMap(_.points.map(_.x)).max,
    max_y = roads.flatMap(_.points.map(_.y)).max
  )
}

object Graph {
  private val cached_graphs = new mutable.HashMap[String, Graph]()

  def load(fn: String): Graph = {
    if (!cached_graphs.contains(fn)) {
      Util.log(s"Loading $fn...")
      cached_graphs(fn) = unserialize(Util.reader(fn))
    }
    return cached_graphs(fn)
  }

  def unserialize(r: StateReader): Graph = {
    val g = new Graph(
      Range(0, r.int).map(_ => Road.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Edge.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Vertex.unserialize(r)).toArray,
      r.string
    )
    g.edges.foreach(e => e.setup(g))
    for (v <- g.vertices; t <- v.turns) {
      t.setup(g)
    }
    // Do roads last; they depend on edges.
    g.roads.foreach(r => r.setup(g))
    g.setup()
    return g
  }
}

// This is only so setup routines can reference Graph or PreGraph3.
abstract class GraphLike {
  def get_r(id: RoadID): Road
  def get_v(id: VertexID): Vertex
  def get_e(id: EdgeID): Edge
}
