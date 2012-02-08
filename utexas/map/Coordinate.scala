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

    // convert to xyz coord system
    val x1 = math.sin(lat1) * math.cos(lon1)
    val y1 = math.sin(lat1) * math.sin(lon1)
    val z1 = math.cos(lat1)

    val x2 = math.sin(lat2) * math.cos(lon2)
    val y2 = math.sin(lat2) * math.sin(lon2)
    val z2 = math.cos(lat2)

    val dot_prod = (x1 * x2) + (y1 * y2) + (z1 * z2)

    val radians = math.acos(dot_prod)

    // arc len = radius * theta
    return radius * radians

    // TODO test it
    /*// Avoid NaN issues
    if (c1.equals(c2)) {
      return 0
    }*/
  }
}
