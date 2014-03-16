// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.common.{MagicSerializable, MagicReader, MagicWriter}
import utexas.aorta.common.algorithms.Distance

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
  override def toString = s"($x, $y)"

  def +(other: Coordinate) = Coordinate(x + other.x, y + other.y)
  def dist_to(o: Coordinate) = Coordinate.gps_dist_in_meters(
    Graph.world_to_gps(this.x, this.y), Graph.world_to_gps(o.x, o.y)
  )
}

object Coordinate {
  // use Graph.world_to_gps to get original GPS coordinates first.
  def gps_dist_in_meters(c1: Coordinate, c2: Coordinate) =
    Distance.equirectangular_dist(c1.x, c1.y, c2.x, c2.y)

  def do_magic_save(obj: Coordinate, w: MagicWriter) {
    MagicSerializable.materialize[Coordinate].magic_save(obj, w)
  }
  def do_magic_load(r: MagicReader) = MagicSerializable.materialize[Coordinate].magic_load(r)
}
