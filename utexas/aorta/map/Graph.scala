// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.common.{Util, StateReader, StateWriter, VertexID, EdgeID, RoadID, TurnID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val turns: Array[Turn], val artifacts: Array[RoadArtifact], val width: Double, val height: Double,
  val offX: Double, val offY: Double, val scale: Double, val name: String
) {
  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.doubles(width, height, offX, offY, scale)
    w.int(roads.size)
    roads.foreach(r => r.serialize(w))
    w.int(edges.size)
    edges.foreach(e => e.serialize(w))
    w.int(vertices.size)
    vertices.foreach(v => v.serialize(w))
    w.int(turns.size)
    turns.foreach(t => t.serialize(w))
    w.string(name)
    w.int(artifacts.size)
    artifacts.foreach(a => a.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def traversables() = edges ++ turns

  def get_v(id: VertexID) = vertices(id.int)
  def get_e(id: EdgeID) = edges(id.int)
  def get_r(id: RoadID) = roads(id.int)
  def get_t(id: TurnID) = turns(id.int)

  // TODO file library
  def basename = name.replace("maps/", "").replace(".map", "")
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  private var width = 0.0
  private var height = 0.0
  private var xoff = 0.0
  private var yoff = 0.0
  private var scale = 0.0

  private val cached_graphs = new mutable.HashMap[String, Graph]()

  // this MUST be set before world_to_gps is called.
  // TODO get rid of this approach once GPS coordinates always retained
  def set_params(w: Double, h: Double, x: Double, y: Double, s: Double) {
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
      print(s"Loading $fn...")
      cached_graphs(fn) = unserialize(Util.reader(fn))
      println(s"\rLoaded $fn.     ")
    }
    return cached_graphs(fn)
  }

  def unserialize(r: StateReader): Graph = {
    val w = r.double
    val h = r.double
    val xo = r.double
    val yo = r.double
    val s = r.double
    val roads = Range(0, r.int).map(_ => Road.unserialize(r)).toArray
    val edges = Range(0, r.int).map(_ => Edge.unserialize(r)).toArray
    val vertices = Range(0, r.int).map(_ => Vertex.unserialize(r)).toArray
    val turns = Range(0, r.int).map(_ => Turn.unserialize(r)).toArray
    val name = r.string
    val artifacts = Range(0, r.int).map(_ => RoadArtifact.unserialize(r)).toArray
    val g = new Graph(roads, edges, vertices, turns, artifacts, w, h, xo, yo, s, name)

    // Length of traversables depends on these being set, but that doesn't really happen during
    // unserialization, so it's fine to do this afterwards
    set_params(w, h, xo, yo, s)
    // Embrace dependencies, but decouple them and setup afterwards.
    // TODO replace with lazy val's and a mutable wrapper around an immutable graph.
    g.edges.foreach(e => e.setup(roads))
    g.roads.foreach(r => r.setup(vertices))
    g.turns.foreach(t => t.setup(edges))
    return g
  }
}
