// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

/*
 * A vital note about coordinate systems:
 * - (longitude, latitude) corresponds to (x, y) where y increases upwards
 * - Swing/AWT has y increasing downwards
 * - Thus, Pass 1 immediately converts to y increasing downwards. This removes
 *   the "y inversion" handling from everywhere else.
 */

// TODO case class
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

  def +(other: Coordinate) = new Coordinate(x + other.x, y + other.y)
  def dist_to(o: Coordinate) = Coordinate.gps_dist_in_meters(
    Graph.world_to_gps(this.x, this.y), Graph.world_to_gps(o.x, o.y)
  )
}

object Coordinate {
  // use Graph.world_to_gps to get original GPS coordinates.
  def gps_dist_in_meters(c1: Coordinate, c2: Coordinate): Double = {
    // This is Mike's math.
    val lon1 = math.toRadians(c1.x)
    val lon2 = math.toRadians(c2.x)
    val lat1 = math.toRadians(c1.y)
    val lat2 = math.toRadians(c2.y)

    val radius = 6378100.0  // of earth, in meters
    val dLat = lat2 - lat1
    val dLon = lon2 - lon1
    
    // a is the square of half the chord length between the points
    val a = math.pow(math.sin(dLat / 2), 2) +
    	      math.pow(math.sin(dLon / 2), 2) * math.cos(lat1) * math.cos(lat2)
    // c is the angular distance in radians
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    return radius * c
  }
}
