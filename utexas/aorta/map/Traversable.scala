// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

// TODO I don't want this dependency, but at the moment, it leads to a great
// perf boost due to dropping a pricy hash lookup
import utexas.aorta.sim.Queue

// Something with a sequence of lines forming a path and a way to get to more
// somethings
abstract class Traversable() {
  // TODO temporary perf fix
  var queue: Queue = null

  var lines: List[Line] = Nil // till set_lines happens.
  def leads_to: List[Traversable]

  // Store; it's not free to compute it constantly
  var length: Double = 0

  def set_lines(ls: List[Line]) = {
    lines = ls
    length = lines.foldLeft(0.0)((a, b) => a + b.length)
  }

  // if dist is > length or < 0, then this query makes no sense
  def location(dist: Double) = current_pos(dist) match {
    case (l: Line, dist_along: Double) => l.point_on(dist_along)
  }

  // returns line and distance along that line
  def current_pos(dist: Double): (Line, Double) = {
    if (dist < 0) {
      throw new Exception("Negative distance on a location?!")
    }

    // TODO it's late, I am not going to write this functionally...
    var at = dist
    for (l <- lines) {
      if (at > l.length) {
        at -= l.length
      } else {
        return (l, at)
      }
    }

    // Here's the deal. Normally this would be a bug, but actually,
    // map.make.Reader artificially bumps up tiny edges to a min of 0.1 meters
    // to avoid breaking lookahead. So just... cheat and claim they're at the
    // end of the tiny, tiny edge.
    assert(length == 0.1)
    return (lines.last, lines.last.length)
  }

  def start_pt = lines.head.start
  def end_pt  = lines.last.end
}
