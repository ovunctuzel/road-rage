// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue, HashSet}
import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, File}

import utexas.aorta.map.make.BinaryReader
import utexas.aorta.map.analysis.{WaypointGenerator, WaypointRouter, Waypoint}

import utexas.aorta.Util

class Graph(val roads: Array[Road], val edges: Array[Edge],
            val vertices: Array[Vertex], map_fn: String)
{
  val directed_roads = Array.fill[DirectedRoad](Road.num_directed_roads)(null)
  roads.foreach(r => {
    directed_roads(r.pos_group.id) = r.pos_group
    directed_roads(r.neg_group.id) = r.neg_group
  })

  // TODO dont want this to be lazy, but its a workaround so Agent.sim gets set
  lazy val router = load_router

  def turns = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)
  def traversables() = edges ++ turns

  // TODO replace with a general map serializer instead. dont take map_fn.
  def load_router(): WaypointRouter = {
    val fn = map_fn.replace("maps/", "maps/_").replace(".map", ".router")
    if ((new File(fn)).exists) {
      val in = new ObjectInputStream(new FileInputStream(fn))
      val r = in.readObject
      in.close
      // TODO generalize the interface
      return r.asInstanceOf[WaypointRouter]
    } else {
      println("Generating waypoints...")
      val r = new WaypointRouter(
        WaypointGenerator.choose_waypoints(this).map(r => new Waypoint(r.id))
      )
      val out = new ObjectOutputStream(new FileOutputStream(fn))
      out.writeObject(r)
      out.close
      return r
    }
  }
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  var width = 0.0
  var height = 0.0
  var xoff = 0.0
  var yoff = 0.0
  var scale = 0.0

  // this MUST be set before world_to_gps is called.
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

  // TODO deprecate this
  def load(fn: String, with_geometry: Boolean) =
    (new BinaryReader(fn, with_geometry)).load_map
}
