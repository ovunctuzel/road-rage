// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import scala.collection.mutable.HashSet

object TurnType extends Enumeration {
  type TurnType = Value
  val CROSS       = Value("C")
  val CROSS_MERGE = Value("M")
  val LEFT        = Value("L")
  val RIGHT       = Value("R")
  val UTURN       = Value("U")
}

// This constructor eschews geometry, taking a length and two points for
// conflicts.
class Turn(
  val id: Int, val from: Edge, val turn_type: TurnType.TurnType, val to: Edge,
  length: Double, val conflict_line: Line
) extends Traversable with Ordered[Turn] {
  // TODO conflict_line is a fragile approach that just extends segments a bit
  set_length(length)

  override def compare(other: Turn) = id.compare(other.id)

  // id is just for comparisons, not indexing
  def to_xml(out: FileWriter) = {
    out.write(
      "    <link from=\"" + from.id + "\" to=\"" + to.id
      + "\" type=\"" + turn_type + "\" length=\"" + length
      + "\" id=\"" + id + "\"/>\n"
    )
  }

  def to_plaintext(out: FileWriter) = {
    out.write(
      from.id + "," + to.id + "," + turn_type + "," + length + "," + id +
      "," + conflict_line.x1 + "," + conflict_line.y1 + "," + conflict_line.x2 +
      "," + conflict_line.y2 + ";"
    )
  }

  override def toString = "" + turn_type + " turn[" + id + "](" + from + ", " + to + ")"
  // Short form is nice.
  //override def toString = "Turn(" + from.id + ", " + to.id + ")"

  def leads_to = List(to)
  def speed_limit = to.speed_limit

  def involves_road(r: Road) = (from.road == r || to.road == r)
  def other_road(r: Road) = if (from.road == r) to.road else from.road

  def vert = from.to

  def conflicts_with(t: Turn) =
    (from != t.from) && (to == t.to || conflict_line.segment_intersect(t.conflict_line))

  // TODO more efficiently?
  def conflicts = vert.turns.filter(conflicts_with).toSet
}

object Turn {
  // This variant creates default one-line long turns.
  def apply(id: Int, from: Edge, turn_type: TurnType.TurnType, to: Edge): Turn = {
    val a = from.lines.last.end
    val b = to.lines.head.start
    return Turn(id, from, turn_type, to, Array(new Line(a, b)))
  }

  // Alternate constructor uses full geometry.
  def apply(id: Int, from: Edge, turn_type: TurnType.TurnType, to: Edge, lines: Array[Line]): Turn = {
    val len = lines.foldLeft(0.0)((a, b) => a + b.length)
    val t = new Turn(id, from, turn_type, to, len, conflict(from, to))
    t.set_lines(lines)
    return t
  }

  def apply(id: Int, from: Edge, turn_type: TurnType.TurnType, to: Edge, len: Double) =
    new Turn(id, from, turn_type, to, len, conflict(from, to))

  def conflict(from: Edge, to: Edge) = new Line(from.end_pt, to.start_pt)
}
