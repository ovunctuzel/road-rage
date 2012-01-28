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

object Coordinate{
  /**
   * This will find the distance in meters between any two GPS coordinates
   */
  def getDistanceInMeters(c1: Coordinate,c2: Coordinate): Double = {
        if (c1.equals(c2)) {
            // This otherwise produces NaN on some (Dustin's) machines
            return 0;
        }

    	//TODO This could probably be more accurate if we paid closer attention to the floating point representations.  The math is sound at least though.
    	val lon1 = scala.math.toRadians(c1.x);//Convert to radians
    	val lon2 = scala.math.toRadians(c2.x);
    	val lat1 = scala.math.toRadians(c1.y) + scala.math.Pi/2.0; //I want 0 at the North Pole, not the equator
    	val lat2 = scala.math.toRadians(c2.y) + scala.math.Pi/2.0;
    	val r = 6378100.; //Radius of earth in meters
    	
    	//Convert to xyz coordinates
    	val x1 = scala.math.sin(lat1)*scala.math.cos(lon1); //Since all we really want is the angle between these, r shouldn't matter
    	val y1 = scala.math.sin(lat1)*scala.math.sin(lon1);
    	val z1 = scala.math.cos(lat1);
    	val x2 = scala.math.sin(lat2)*scala.math.cos(lon2);
    	val y2 = scala.math.sin(lat2)*scala.math.sin(lon2);
    	val z2 = scala.math.cos(lat2);
    	
    	val dotProd = x1*x2+y1*y2+z1*z2;
    	//double len1 = Math.hypot(x1, Math.hypot(y1,z1));//These better both be 1.0 anyway
    	//double len2 = Math.hypot(x2, Math.hypot(y2,z2));
    	
    	val radTheta = scala.math.acos(dotProd/*/(len1*len2)*/); //Don't need to handle +pi cases because if theta > pi then we should go the other direction around Earth
        val result = r * radTheta; // s = r*theta

        return result;
	}
}