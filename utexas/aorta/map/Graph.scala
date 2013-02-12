// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue, HashSet}

import utexas.aorta.map.make.PlaintextReader

import utexas.aorta.Util

class Graph(val roads: Array[Road], val edges: Array[Edge],
            val vertices: Array[Vertex])
{
  val turns = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)

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

  def load(fn: String, with_geometry: Boolean) =
    (new PlaintextReader(fn, with_geometry)).load_map
}
