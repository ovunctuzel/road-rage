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

  def conflicts: Set[Turn] = {
    val set = new HashSet[Turn]()

    // always: anything else that has the same target
    set ++= (to.prev_turns.toSet - this)

    turn_type match {
      // All crossings from perpendicular roads
      // and lefts, except from us
      // TODO anything in or out of the road to our right?
      case TurnType.CROSS => {
        val perps = vert.perp_roads(from.road)
        set ++= vert.turns.filter(
          t => (t.turn_type == TurnType.CROSS && perps(t.from.road))
             || (t.turn_type == TurnType.LEFT && t.from != from)
        )
      }

      case TurnType.LEFT => {
        // TODO this doesnt handle multiple lanes, its too conservative
        // other sides' crossings
        set ++= vert.turns.filter(t => t.turn_type == TurnType.CROSS && t.from != from)
        // TODO cleaner way of finding this opposite lane... this is a mess again
        for (opposite <- vert.turns_to(to).filter(t => t.turn_type == TurnType.RIGHT)) {
          set ++= opposite.from.other_lanes.flatMap(e => e.crosses)
        }
        // and left turns from perp roads
        val perps = vert.perp_roads(from.road)
        set ++= vert.turns.filter(
          t => t.turn_type == TurnType.LEFT && perps(t.from.road)
        )
      }

      case TurnType.CROSS_MERGE => {
        // TODO
      }

      // just things that have the same target!
      case TurnType.RIGHT => {}

      // Nothing!
      case TurnType.UTURN => {}
    }

    return set.toSet
  }

  def conflicts_with(t: Turn) =
    (from != t.from) && (to == t.to || conflict_line.segment_intersect(t.conflict_line))

  // TODO more efficiently?
  def more_conflicts = vert.turns.filter(conflicts_with)
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

  // TODO brittle approach
  def conflict(from: Edge, to: Edge) =
    new Line(from.shifted_end_pt(0.0), to.shifted_start_pt(0.0))
}
