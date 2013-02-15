// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.Serializable
import scala.collection.mutable.HashSet

import utexas.aorta.Common

// This constructor eschews geometry, taking a length and two points for
// conflicts.
// TODO from and to ID are var because we fiddle with IDs during Pass 3
// construction.
@SerialVersionUID(1)
class Turn(
  val id: Int, var from_id: Int, var to_id: Int, length: Double, val conflict_line: Line
) extends Traversable with Ordered[Turn] with Serializable
{
  // The serialization dependency cycle has to be broken somewhere, or the order
  // causes stack overflow. Break it here.
  @transient lazy val from = Common.edges(from_id)
  @transient lazy val to = Common.edges(to_id)

  // TODO conflict_line is a fragile approach that just extends segments a bit
  set_length(length)

  override def compare(other: Turn) = id.compare(other.id)

  override def toString = "turn[" + id + "](" + from + ", " + to + ")"
  // Short form is nice.
  //override def toString = "Turn(" + from.id + ", " + to.id + ")"

  def leads_to = List(to)
  def speed_limit = to.speed_limit

  def involves_road(r: Road) = (from.road == r || to.road == r)
  def other_road(r: Road) = if (from.road == r) to.road else from.road

  def vert = from.to

  def conflicts_with(t: Turn) =
    (from != t.from) && (to == t.to || conflict_line.segment_intersection(t.conflict_line).isDefined)

  // TODO more efficiently?
  def conflicts = vert.turns.filter(conflicts_with).toSet
}

object Turn {
  // This variant creates default one-line long turns.
  def apply(id: Int, from: Edge, to: Edge): Turn = {
    val a = from.lines.last.end
    val b = to.lines.head.start
    return Turn(id, from, to, Array(new Line(a, b)))
  }

  // Alternate constructor uses full geometry.
  def apply(id: Int, from: Edge, to: Edge, lines: Array[Line]): Turn = {
    val len = lines.foldLeft(0.0)((a, b) => a + b.length)
    val t = new Turn(id, from.id, to.id, len, conflict(from, to))
    t.set_lines(lines)
    return t
  }

  def apply(id: Int, from: Edge, to: Edge, len: Double) =
    new Turn(id, from.id, to.id, len, conflict(from, to))

  def conflict(from: Edge, to: Edge) = new Line(from.end_pt, to.start_pt)
}
