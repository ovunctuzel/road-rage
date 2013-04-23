// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue, HashSet}
import java.io.Serializable

import utexas.aorta.map.analysis.{Router, DijkstraRouter, CHRouter}

import utexas.aorta.{Util, Common}

@SerialVersionUID(1)
class Graph(
  val roads: Array[Road], val edges: Array[Edge], val vertices: Array[Vertex],
  val width: Double, val height: Double, val offX: Double, val offY: Double,
  val scale: Double, val name: String
) extends Serializable
{
  val directed_roads = Array.fill[DirectedRoad](Road.num_directed_roads)(null)
  roads.foreach(r => {
    List(r.pos_group, r.neg_group).flatten.foreach(dr => {
      directed_roads(dr.id) = dr
    })
  })

  @transient lazy val router: Router = choose_router
  @transient lazy val dijkstra_router = new DijkstraRouter(this)
  @transient lazy val ch_router = new CHRouter(this)

  // Prefer CH if this map has been prerouted.
  private def choose_router(): Router = {
    if (ch_router.usable) {
      return ch_router
    } else {
      return dijkstra_router
    }
  }

  def turns = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)
  def traversables() = edges ++ turns
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
    val g = Util.unserialize(fn).asInstanceOf[Graph]
    set_params(g.width, g.height, g.offX, g.offY, g.scale)
    Common.edges = g.edges
    return g
  }
}
