// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import scala.collection.mutable.HashSet

abstract class TurnLike extends Traversable {
  def from: Edge
  def to: Edge
  def conflicts(other: TurnLike): Boolean
  def other_turns(): Set[TurnLike]
}

object TurnLike {
  def toTurn(t: TurnLike) = t match {
    case turn: Turn => turn
    case _ => throw new ClassCastException("uberturn, not turn")
  }

  def toUberTurn(t: TurnLike) = t match {
    case u: UberTurn => u
    case _ => throw new ClassCastException("turn, not uberturn")
  }
}

object TurnType extends Enumeration {
  type TurnType = Value
  val CROSS       = Value("C")
  val CROSS_MERGE = Value("M")
  val LEFT        = Value("L")
  val RIGHT       = Value("R")
  val UTURN       = Value("U")
}

class Turn(val id: Int, val from: Edge, val turn_type: TurnType.TurnType, val to: Edge)
  extends TurnLike with Ordered[Turn]
{
  override def compare(other: Turn) = id.compare(other.id)

  // id is just for comparisons, not indexing
  def to_xml(out: FileWriter) = {
    out.write(
      "    <link from=\"" + from.id + "\" to=\"" + to.id
      + "\" type=\"" + turn_type + "\" id=\"" + id + "\"/>\n"
    )
  }

  override def toString = "" + turn_type + " turn[" + id + "](" + from + ", " + to + ")"
  // Short form is nice.
  //override def toString = "Turn(" + from.id + ", " + to.id + ")"

  // TODO a little anonymous sub returning the line?
  private val a = from.lines.last.end
  private val b = to.lines.head.start
  set_lines(List[Line](new Line(a.x, a.y, b.x, b.y)))
  def leads_to = List(to)

  def involves_road(r: Road) = (from.road == r || to.road == r)
  def other_road(r: Road) = if (from.road == r) to.road else from.road

  def vert = from.to

  var conflicts_set: Set[TurnLike] = null
  def conflicts(other: TurnLike) = conflicts_set.contains(other)
  def calculate_conflicts = {
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

    conflicts_set = set.toSet
  }

  def other_turns() = vert.turns.toSet
}

// a sequence of turns within an UberVertex
class UberTurn(val turns: List[Turn], val v: UberVertex) extends TurnLike {
  def from: Edge = turns.head.from
  def to: Edge = turns.last.to
  def edges_to: Set[Edge] = turns.map(t => t.to).toSet
  // TODO technically we should do something with lines...

  override def toString = "Uber[" + turns.map(t => t.from.id + "->" + t.to.id).mkString(", ") + "]"

  def conflicts(other: TurnLike): Boolean = {
    val uber_other = TurnLike.toUberTurn(other)

    // if they share any edge, they conflict
    // TODO but what about subpaths of bigger paths?
    if (!(edges_to & uber_other.edges_to).isEmpty) {
      return true
    }

    // for any common vertex, check for conflicting turns.
    // note an uber-turn could repeat a vertex (rarely)
    // TODO this search is quadratic, it could probably be better
    for (t1 <- turns) {
      return uber_other.turns.find(t2 => t2.vert == t1.vert && t1.conflicts(t2)).isDefined
    }

    // no problems
    return false
  }

  def other_turns() = v.turns.toSet // not sure why other .toSet needed... TODO

  def leads_to = List(to)
}
