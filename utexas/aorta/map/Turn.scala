// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.map.make.MapStateWriter
import utexas.aorta.common.{StateReader, TurnID, EdgeID, Util}

class Turn(val id: TurnID, from_id: EdgeID, to_id: EdgeID, geometry: Array[Line])
  extends Traversable(geometry) with Ordered[Turn]
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  var from: Edge = null
  var to: Edge = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: MapStateWriter) {
    w.int(id.int)
    w.int(from_id.int)
    w.int(to_id.int)
    lines.head.serialize(w)
  }

  def setup(edges: Array[Edge]) {
    from = edges(from_id.int)
    to = edges(to_id.int)
    Util.assert_eq(from.id, from_id)
    Util.assert_eq(to.id, to_id)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def compare(other: Turn) = id.int.compare(other.id.int)
  override def asTurn = this
  override def asEdge = throw new Exception("This is a turn, not an edge")

  override def toString = "turn[" + id + "](" + from + ", " + to + ")"
  // Short form is nice.
  //override def toString = "Turn(" + from.id + ", " + to.id + ")"

  def leads_to = List(to)
  def speed_limit = to.speed_limit

  def vert = from.to

  def conflict_line = lines.head
  def conflicts_with(t: Turn) =
    (from != t.from) && (to == t.to || conflict_line.segment_intersection(t.conflict_line).isDefined)

  // TODO more efficiently?
  def conflicts = vert.turns.filter(conflicts_with).toSet

  def angle_deg = math.toDegrees(conflict_line.angle)
}

object Turn {
  def unserialize(r: StateReader) = new Turn(
    new TurnID(r.int), new EdgeID(r.int), new EdgeID(r.int), Array(Line.unserialize(r))
  )
}
