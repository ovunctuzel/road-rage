package utexas.map

// Something with a sequence of lines forming a path and a way to get to more
// somethings
abstract class Traversable() {
  var lines: List[Line]
  def leads_to: List[Traversable]

  def length: Double = lines.foldLeft(0.0)((a, b) => a + b.length)

  // if dist is > length or < 0, then this query makes no sense
  def location(dist: Double): Coordinate = {
    if (dist < 0) {
      throw new Exception("Negative distance on a location?!")
    }

    // TODO it's late, I am not going to write this functionally...
    var at = dist
    for (l <- lines) {
      if (at > l.length) {
        at -= l.length
      } else {
        // where are we on this line?
        val percent = at / l.length
        return new Coordinate(
          l.x1 + (l.width * percent),
          l.y1 + (l.height * percent)
        )
      }
    }

    throw new Exception("Location is past the end of an edge!")
  }

  def start_pt = lines.head.start
  def end_pt  = lines.last.end
}
