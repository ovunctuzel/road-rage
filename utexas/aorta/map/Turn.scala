// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import utexas.aorta.common.{MagicSerializable, MagicReader, MagicWriter, TurnID, EdgeID, Util}

// TODO args except id public only due to magic serialization
class Turn(val id: TurnID, val from_id: EdgeID, val to_id: EdgeID, val geometry: Array[Line])
  extends Traversable(geometry) with Ordered[Turn]
{
  //////////////////////////////////////////////////////////////////////////////
  // Transient State

  var from: Edge = null
  var to: Edge = null

  def setup(edges: Array[Edge]) {
    from = edges(from_id.int)
    to = edges(to_id.int)
    Util.assert_eq(from.id, from_id)
    Util.assert_eq(to.id, to_id)
    vert.turns += this
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
  def do_magic_save(obj: Turn, w: MagicWriter) {
    MagicSerializable.materialize[Turn].magic_save(obj, w)
  }
  def do_magic_load(r: MagicReader) = MagicSerializable.materialize[Turn].magic_load(r)
}
