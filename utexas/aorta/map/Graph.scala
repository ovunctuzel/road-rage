// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{Util, Common, StateReader, RoadID, VertexID, EdgeID, DirectedRoadID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val width: Double, val height: Double, val offX: Double, val offY: Double,
  val scale: Double, val name: String
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

  def serialize(w: MapStateWriter) {
    w.double(width)
    w.double(height)
    w.double(offX)
    w.double(offY)
    w.double(scale)
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
  def get_dr(id: DirectedRoadID) = directed_roads(id.int)

  // TODO file library
  def basename = name.replace("maps/", "").replace(".map", "")
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  var width = 0.0
  var height = 0.0
  var xoff = 0.0
  var yoff = 0.0
  var scale = 0.0

  private val cached_graphs = new mutable.HashMap[String, Graph]()

  // this MUST be set before world_to_gps is called.
  // TODO get rid of this approach once GPS coordinates always retained
  def set_params(w: Double, h: Double, x: Double, y: Double, s: Double) = {
    width = w
    height = h
    xoff = x
    yoff = y
    scale = s
  }

  // inverts what PreGraph1's normalize() does.
  def world_to_gps(x: Double, y: Double) = Coordinate(
    (x / scale) - xoff, ((height - y) / scale) - yoff
  )

  def load(fn: String): Graph = {
    if (!cached_graphs.contains(fn)) {
      Util.log(s"Loading $fn...")
      cached_graphs(fn) = unserialize(Util.reader(fn))
    }
    return cached_graphs(fn)
  }

  def unserialize(r: StateReader): Graph = {
    // Set these before loading any traversables, since length'll be computed from em
    val w = r.double
    val h = r.double
    val xo = r.double
    val yo = r.double
    val s = r.double
    set_params(w, h, xo, yo, s)
    val g = new Graph(
      Range(0, r.int).map(_ => Road.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Edge.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Vertex.unserialize(r)).toArray,
      w, h, xo, yo, s, r.string
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
