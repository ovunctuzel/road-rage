// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue, HashSet}

import utexas.aorta.common.{Util, Common, StateWriter, StateReader, RoadID,
                            VertexID, EdgeID, DirectedRoadID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val width: Double, val height: Double, val offX: Double, val offY: Double,
  val scale: Double, val name: String, val bldg_centers: Array[Coordinate]
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
    w.double(width)
    w.double(height)
    w.double(offX)
    w.double(offY)
    w.double(scale)
    w.string(name)
    w.int(bldg_centers.size)
    bldg_centers.foreach(pt => pt.serialize(w))
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
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  var width = 0.0
  var height = 0.0
  var xoff = 0.0
  var yoff = 0.0
  var scale = 0.0

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
  def world_to_gps(x: Double, y: Double) = new Coordinate(
    (x / scale) - xoff, ((height - y) / scale) - yoff
  )

  def load(fn: String): Graph = {
    Util.log(s"Loading $fn...")
    return unserialize(Util.reader(fn))
  }

  def unserialize(r: StateReader): Graph = {
    val g = new Graph(
      Range(0, r.int).map(_ => Road.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Edge.unserialize(r)).toArray,
      Range(0, r.int).map(_ => Vertex.unserialize(r)).toArray,
      r.double, r.double, r.double, r.double, r.double, r.string,
      Range(0, r.int).map(_ => Coordinate.unserialize(r)).toArray
    )
    set_params(g.width, g.height, g.offX, g.offY, g.scale)
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
