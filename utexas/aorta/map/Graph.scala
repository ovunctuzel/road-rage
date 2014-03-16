// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable

import utexas.aorta.common.{MagicSerializable, MagicReader, MagicWriter, VertexID, EdgeID,
                            RoadID, TurnID, BinaryMagicReader}

class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val turns: Array[Turn], val artifacts: Array[RoadArtifact], val width: Double, val height: Double,
  val offX: Double, val offY: Double, val scale: Double, val name: String
) {
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
      cached_graphs(fn) = do_magic_load(new BinaryMagicReader(fn))
      println(s"\rLoaded $fn.     ")
    }
    return cached_graphs(fn)
  }

  def do_magic_save(obj: Graph, w: MagicWriter) {
    MagicSerializable.materialize[Graph].magic_save(obj, w)
  }
  def do_magic_load(r: MagicReader): Graph = {
    val g = MagicSerializable.materialize[Graph].magic_load(r)
    // Length of traversables depends on these being set, but that doesn't really happen during
    // unserialization, so it's fine to do this afterwards
    set_params(g.width, g.height, g.offX, g.offY, g.scale)
    // Embrace dependencies, but decouple them and setup afterwards.
    // TODO replace with lazy val's and a mutable wrapper around an immutable graph.
    g.edges.foreach(e => e.setup(g.roads))
    g.roads.foreach(r => r.setup(g.vertices))
    g.turns.foreach(t => t.setup(g.edges))
    return g
  }
}
