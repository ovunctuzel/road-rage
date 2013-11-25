// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.common.{Common, StateWriter, StateReader, TurnID, EdgeID}

// This constructor eschews geometry, taking a length and two points for
// conflicts.
// TODO from and to ID are var because we fiddle with IDs during Pass 3
// construction.
class Turn(val id: TurnID, var from_id: EdgeID, var to_id: EdgeID, val conflict_line: Line)
  extends Traversable(Array(conflict_line)) with Ordered[Turn]
{
  //////////////////////////////////////////////////////////////////////////////
  // Deterministic state

  var from: Edge = null
  var to: Edge = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.int(from_id.int)
    w.int(to_id.int)
    conflict_line.serialize(w)
  }

  def setup(g: GraphLike) {
    from = g.get_e(from_id)
    to = g.get_e(to_id)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def compare(other: Turn) = id.int.compare(other.id.int)

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
  def unserialize(r: StateReader) = new Turn(
    new TurnID(r.int), new EdgeID(r.int), new EdgeID(r.int), Line.unserialize(r)
  )
}
