package utexas.map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.collection.mutable.HashSet
import math.{min, max}

import utexas.map.Coordinate

import utexas.Util.{log, log_push, log_pop}

class PreGraph1() {
  var edges = new MutableList[PreEdge1]

  // we actually only care about if something is a vertex or not!
  private var vert_lookup = new HashSet[Coordinate]   // TODO var because we map...
  // TODO they're vals, but i don't want to set them yet!
  var width: Double = 0
  var height: Double = 0
  var offX: Double = 0
  var offY: Double = 0
  // TODO Util.cfg.mapmake-scale
  var scale: Double = 5000

  def add_edge(name: String, road_type: String, oneway: Boolean,
               orig_id: Int, points: MutableList[Coordinate]) =
  {
    edges += new PreEdge1(name, road_type, oneway, orig_id, points)
  }

  // idempotent if there is already a vertex
  def add_vertex(where: Coordinate) {
    vert_lookup += where
  }

  def is_vert(pt: Coordinate) = vert_lookup.contains(pt)

  def normalize() = {
    // TODO a better solution for initializing, or safer values? they're low
    // since longitude/latitude don't range that much
    val min_default: Double = 10e10
    val max_default: Double = -10e10

    // Calculate the bounds of the map from only the nodes we use
    var minX = min_default
    var minY = min_default
    var maxX = max_default
    var maxY = max_default
    // TODO make an iter_pts
    for (e <- edges; pt: Coordinate <- e.points) {
      minX = min(minX, pt.x)
      minY = min(minY, pt.y)
      maxX = max(maxX, pt.x)
      maxY = max(maxY, pt.y)
    }
    log("Bounds: %f .. %f, %f .. %f".format(minX, maxX, minY, maxY))

    // so to make (minX, minY) the new origin...
    offX = 0 - minX
    offY = 0 - minY

    // Scale everything up by some fixed ratio...
    width = (maxX + offX) * scale
    height = (maxY + offY) * scale

    // TODO this is so close to a one-liner now...
    def normalize(pt: Coordinate): Coordinate = {
      // Longitude, Latitude origin is bottom-left; we draw from top-left
      // Hence height - y
      // This is the ONLY place we handle y inversion. Don't make things
      // confusing after this!
      return new Coordinate(
        (pt.x + offX) * scale,
        height - (pt.y + offY) * scale
      )

      //if (x < 0 || x > width || y < 0 || y > height) {
      //  log("OOB coordinate after normalizing! " + pt)
      //}
    }

    vert_lookup = vert_lookup map normalize
    for (e <- edges) {
      e.points = e.points map normalize
    }
  }
}

// TODO this is now a glorified struct. O_O
class PreEdge1(val name: String, val road_type: String, val oneway: Boolean,
               val orig_id: Int, var points: MutableList[Coordinate]) {}
