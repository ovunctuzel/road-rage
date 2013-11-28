// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.common.{StateWriter, StateReader}

/*
 * A vital note about coordinate systems:
 * - (longitude, latitude) corresponds to (x, y) where y increases upwards
 * - Swing/AWT has y increasing downwards
 * - Thus, Pass 1 immediately converts to y increasing downwards. This removes
 *   the "y inversion" handling from everywhere else.
 */

case class Coordinate(x: Double, y: Double) extends Ordered[Coordinate] {
  // Lexicographic
  override def compare(other: Coordinate) = if (x == other.x)
                                              y.compare(other.y)
                                            else
                                              x.compare(other.x)
  // pretty printer
  override def toString = "(%f, %f)".format(x, y)

  def +(other: Coordinate) = Coordinate(x + other.x, y + other.y)
  def dist_to(o: Coordinate) = Coordinate.gps_dist_in_meters(
    Graph.world_to_gps(this.x, this.y), Graph.world_to_gps(o.x, o.y)
  )

  def serialize(w: StateWriter) {
    w.double(x)
    w.double(y)
  }
}

object Coordinate {
  // In meters
  private val earth_radius = 6378100.0

  // use Graph.world_to_gps to get original GPS coordinates first.
  def gps_dist_in_meters(c1: Coordinate, c2: Coordinate) = haversine_dist(c1, c2)

  // Haversine formula (slow, but accurate) from
  // http://www.movable-type.co.uk/scripts/latlong.html
  private def haversine_dist(c1: Coordinate, c2: Coordinate): Double = {
    val lon1 = math.toRadians(c1.x)
    val lon2 = math.toRadians(c2.x)
    val lat1 = math.toRadians(c1.y)
    val lat2 = math.toRadians(c2.y)

    val dLat = lat2 - lat1
    val dLon = lon2 - lon1
    val a = math.pow(math.sin(dLat / 2), 2) +
    	      math.pow(math.sin(dLon / 2), 2) * math.cos(lat1) * math.cos(lat2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    return earth_radius * c
  }

  // Equirectangular approximation (fast, still should be pretty accurate) from
  // http://www.movable-type.co.uk/scripts/latlong.html
  private def equirectangular_dist(c1: Coordinate, c2: Coordinate): Double = {
    val lon1 = math.toRadians(c1.x)
    val lon2 = math.toRadians(c2.x)
    val lat1 = math.toRadians(c1.y)
    val lat2 = math.toRadians(c2.y)
    val x = (lon2 - lon1) * math.cos((lat1 + lat2) / 2)
    val y = lat2 - lat1
    return earth_radius * math.sqrt(math.pow(x, 2) + math.pow(y, 2))
  }

  def unserialize(r: StateReader) = Coordinate(r.double, r.double)
}
