package utexas.map

import java.io.FileWriter

/*
 * A vital note about coordinate systems:
 * - (longitude, latitude) corresponds to (x, y) where y increases upwards
 * - Swing/AWT has y increasing downwards
 * - Thus, Pass 1 immediately converts to y increasing downwards. This removes
 *   the "y inversion" handling from everywhere else.
 */

class Coordinate(val x: Double, val y: Double) extends Ordered[Coordinate] {
  override def hashCode = (x, y).hashCode
  // TODO override this notion of equals and hashCode automatically.
  override def equals(other: Any) = other match {
    case other: Coordinate => { hashCode == other.hashCode }
    case _ => false
  }

  // Lexicographic
  override def compare(other: Coordinate) = if (x == other.x)
                                              y.compare(other.y)
                                            else
                                              x.compare(other.x)
  // pretty printer
  override def toString = "(%f, %f)".format(x, y)

  def to_xml(out: FileWriter) = {
    out.write("    <pt x=\"" + x + "\" y=\"" + y + "\"/>\n")
  }

  def +(other: Coordinate) = new Coordinate(x + other.x, y + other.y)
  def euclid_dist(o: Coordinate) = scala.math.sqrt(scala.math.pow(x - o.x, 2) + scala.math.pow(y - o.y, 2))
}

object Coordinate {
  // use Graph.world_to_gps to get original GPS coordinates.
  def gps_dist_in_meters(c1: Coordinate, c2: Coordinate): Double = {
    // This is Mike's math.
    val lon1 = math.toRadians(c1.x)
    val lon2 = math.toRadians(c2.x)
    // make 0 at the north pole, not equator
    val lat1 = math.toRadians(c1.y) + (math.Pi / 2.0)
    val lat2 = math.toRadians(c2.y) + (math.Pi / 2.0)

    val radius = 6378100.0  // of earth, in meters
//
//    // convert to xyz coord system
//    val x1 = math.sin(lat1) * math.cos(lon1)
//    val y1 = math.sin(lat1) * math.sin(lon1)
//    val z1 = math.cos(lat1)
//
//    val x2 = math.sin(lat2) * math.cos(lon2)
//    val y2 = math.sin(lat2) * math.sin(lon2)
//    val z2 = math.cos(lat2)
//
//    val dot_prod = (x1 * x2) + (y1 * y2) + (z1 * z2)
//
//    val radians = math.acos(dot_prod)
//
//    // When the coordinates are the same, I've observed dot_prod = 1.0 +
//    // epsilon, which makes acos flip out and return NaN. Just catch that here.
//    // We can't simply check 'c1 == c2', because the difference is a weird
//    // floating-point epsilon issue. So ASSUME NaN means same coordinates.
//    return if (radians.isNaN)
//             0.0
//           else
//             radius * radians  // arc len = radius * theta
    
    val dLat = (lat2-lat1)
    val dLon = (lon2-lon1)
    
    //a is the square of half the chord length between the points
    val a = math.pow(math.sin(dLat/2),2) +
    	math.pow(math.sin(dLon/2),2) * Math.cos(lat1) * Math.cos(lat2)
    //c is the angular distance in radians
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    return radius * c
  }
}
