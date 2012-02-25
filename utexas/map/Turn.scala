package utexas.map

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

class Turn(val id: Int, val from: Edge, val turn_type: TurnType.TurnType, val to: Edge)
  extends Traversable with Ordered[Turn]
{
  override def compare(other: Turn) = id.compare(other.id)

  // id is just for comparisons, not indexing
  def to_xml(out: FileWriter) = {
    out.write(
      "    <link from=\"" + from.id + "\" to=\"" + to.id
      + "\" type=\"" + turn_type + "\" id=\"" + id + "\"/>\n"
    )
  }

  override def toString = "" + turn_type + " turn(" + from + ", " + to + ")"
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
        // TODOthis doesnt handle multiple lanes, its too conservative
        // other sides' crossings
        set ++= vert.turns.filter(t => t.turn_type == TurnType.CROSS && t.from != from)
        // TODO cleaner way of finding this opposite lane... this is a mess again
        for (opposite <- vert.turns_to(to).filter(t => t.turn_type == TurnType.RIGHT)) {
          set ++= opposite.from.other_lanes.flatMap(e => e.crosses)
        }
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
}
