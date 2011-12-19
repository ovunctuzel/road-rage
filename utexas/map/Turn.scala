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

class Turn(val from: Edge, val turn_type: TurnType.TurnType, val to: Edge) {
  def to_xml(out: FileWriter) = {
    out.write(
      "    <link from=\"" + from.id + "\" to=\"" + to.id
      + "\" type=\"" + turn_type + "\"/>\n"
    )
  }

  override def toString = "" + turn_type + " turn from " + from + " to " + to

  def involves_road(r: Road) = (from.road == r || to.road == r)
  def other_road(r: Road) = if (from.road == r) to.road else from.road

  def conflicts: Set[Turn] = {
    val set = new HashSet[Turn]()

    // always: anything else that has the same target
    set ++= to.prev_turns
    set -= this

    turn_type match {
      case TurnType.CROSS => {
      }

      case TurnType.LEFT => {
        // TODO all crossings or lefts from a lane in a perpendicular road
        //set ++= to.opposite_lanes.flatMap(e => e.crosses ++ e.left_turns)
      }

      case TurnType.CROSS_MERGE => {
      }

      // just things that have the same target!
      case TurnType.RIGHT => {}

      // TODO nothing, right?
      case TurnType.UTURN => {}
    }

    return set.toSet
  }
}
