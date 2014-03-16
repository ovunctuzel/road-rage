// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.common.{Util, StateReader, StateWriter, VertexID, EdgeID, RoadID, TurnID}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val turns: Array[Turn], val artifacts: Array[RoadArtifact], val name: String
) {
  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.int(roads.size)
    roads.foreach(r => r.serialize(w))
    w.int(edges.size)
    edges.foreach(e => e.serialize(w))
    w.int(vertices.size)
    vertices.foreach(v => v.serialize(w))
    w.int(turns.size)
    turns.foreach(t => t.serialize(w))
    w.int(artifacts.size)
    artifacts.foreach(a => a.serialize(w))
    w.string(name)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def traversables() = edges ++ turns

  def get_v(id: VertexID) = vertices(id.int)
  def get_e(id: EdgeID) = edges(id.int)
  def get_r(id: RoadID) = roads(id.int)
  def get_t(id: TurnID) = turns(id.int)

  // TODO not sure if these make sense here. only the UI cares.
  private lazy val minX = roads.flatMap(_.points).map(_.x).min
  private lazy val maxX = roads.flatMap(_.points).map(_.x).max
  private lazy val minY = roads.flatMap(_.points).map(_.y).min
  private lazy val maxY = roads.flatMap(_.points).map(_.y).max
  lazy val scale = 5000.0
  lazy val offX = 0 - minX
  lazy val offY = 0 - minY
  lazy val width = (maxX + offX) * scale
  lazy val height = (maxY + offY) * scale
  // Longitude, Latitude origin is bottom-left; we draw from top-left
  // Hence height - y
  // This is the ONLY place we handle y inversion. Don't make things
  // confusing after this!
  def fix(pt: Coordinate) = Coordinate((pt.x + offX) * scale, height - ((pt.y + offY) * scale))

  // TODO file library
  def basename = name.replace("maps/", "").replace(".map", "")
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  private val cached_graphs = new mutable.HashMap[String, Graph]()

  def load(fn: String): Graph = {
    if (!cached_graphs.contains(fn)) {
      print(s"Loading $fn...")
      cached_graphs(fn) = unserialize(Util.reader(fn))
      println(s"\rLoaded $fn.     ")
    }
    return cached_graphs(fn)
  }

  def unserialize(r: StateReader): Graph = {
    val roads = Range(0, r.int).map(_ => Road.unserialize(r)).toArray
    val edges = Range(0, r.int).map(_ => Edge.unserialize(r)).toArray
    val vertices = Range(0, r.int).map(_ => Vertex.unserialize(r)).toArray
    val turns = Range(0, r.int).map(_ => Turn.unserialize(r)).toArray
    val artifacts = Range(0, r.int).map(_ => RoadArtifact.unserialize(r)).toArray
    val name = r.string
    val g = new Graph(roads, edges, vertices, turns, artifacts, name)
    // Embrace dependencies, but decouple them and setup afterwards.
    // TODO replace with lazy val's and a mutable wrapper around an immutable graph.
    g.edges.foreach(e => e.setup(roads))
    g.roads.foreach(r => r.setup(vertices))
    g.turns.foreach(t => t.setup(edges))
    return g
  }
}
