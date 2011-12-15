package map

import scala.collection.mutable.MutableList
import scala.collection.immutable.HashSet

class Vertex(val location: Coordinate, val id: Int) {
  // TODO we could keep a map for faster lookup, sure
  var turns = new MutableList[Turn]

  // TODO construction sucks
  def turns_from(from: Edge): List[Turn] = turns.toList.filter(_.from == from)
  def turns_to(to: Edge): List[Turn] = turns.toList.filter(_.to == to)

  // what verts lead to this one?
  def in_verts = HashSet() ++ turns.map(t => t.from.from)
  // what verts does this one lead to?
  def out_verts = HashSet() ++ turns.map(t => t.to.to)

  // Somewhere due to unordered hashes, the turns list winds up in an
  // inconsistent order. To make diffs of output be the same for the same code,
  // impose an ordering on the turns, sorting first by the 'from' edge and then
  // the 'to' edge.
  def to_xml = <vertex id={id.toString} x={location.x.toString}
                       y={location.y.toString}>
                 {turns.sortBy(t => (t.from.id, t.to.id)).map(t => t.to_xml)}
               </vertex>

  override def equals(other: Any) = other match {
    case other: Vertex => { id == other.id }
    case _ => false
  }
}

// TODO maybe this would work better as a Pair
class Turn(val from: Edge, val turn_type: TurnType.TurnType, val to: Edge) {
  def to_xml = <link from={from.id.toString} to={to.id.toString}
                     type={turn_type.toString}/>
}

object TurnType extends Enumeration {
  type TurnType = Value
  val CROSS       = Value("C")
  val CROSS_MERGE = Value("M")
  val LEFT        = Value("L")
  val RIGHT       = Value("R")
  val UTURN       = Value("U")
}
