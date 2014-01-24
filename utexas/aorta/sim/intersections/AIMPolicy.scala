// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.map.{Turn, Line}
import utexas.aorta.sim.make.IntersectionType

import scala.collection.mutable

class AIMPolicy(intersection: Intersection, ordering: IntersectionOrdering[Ticket])
  extends ReservationPolicy(intersection, ordering)
{
  override def policy_type = IntersectionType.Reservation

  case class Conflict(turn1: Turn, collision_dist1: Double, turn2: Turn, collision_dist2: Double)
  private val conflict_map = find_conflicts()

  private def find_conflicts(): mutable.MultiMap[Turn, Conflict] = {
    val all_conflicts =
      for (t1 <- intersection.v.turns; t2 <- intersection.v.turns if t1 != t2)
        yield make_conflict(t1, t2)
    val map = new mutable.HashMap[Turn, mutable.Set[Conflict]] with mutable.MultiMap[Turn, Conflict]
    for (c <- all_conflicts.flatten) {
      map.addBinding(c.turn1, c)
      map.addBinding(c.turn2, c)
    }
    return map
  }

  // TODO other types of conflict... same destination lane. would that cause intersection? is it
  // redundant? check.
  private def make_conflict(turn1: Turn, turn2: Turn): Option[Conflict] =
    turn1.conflict_line.segment_intersection(turn2.conflict_line) match {
      case Some(pt) => Some(Conflict(
        turn1, new Line(turn1.conflict_line.start, pt).length,
        turn2, new Line(turn2.conflict_line.start, pt).length
      ))
      case None => None
    }
}
