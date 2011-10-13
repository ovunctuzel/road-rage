package map

class Coordinate(val x: Double, val y: Double) {
  override def hashCode = (x, y).hashCode
  // TODO override this notion of equals and hashCode automatically.
  override def equals(other: Any) = other match {
    case other: Coordinate => { hashCode == other.hashCode }
    case _ => false
  }

  // TODO better cloning constructor? and is it needed?
  def copy = new Coordinate(x, y)

  // pretty printer
  override def toString = "(%f, %f)".format(x, y)

  def to_xml = <pt x={x.toString} y={y.toString}/>

  // TODO sexy overloading
  def subtract(other: Coordinate) = new Coordinate(x - other.x, y - other.y)

  def difference(other: Coordinate) = subtract(other).mag

  def mag = math.sqrt(x*x + y*y)
}
