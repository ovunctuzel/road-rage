// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.map.{Turn, Line}
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.common.{Physics, cfg}

import scala.collection.mutable

class AIMPolicy(intersection: Intersection, ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  private val conflict_map = find_conflicts()

  // TODO serialization

  override def react() {
    while (true) {
      ordering.choose(candidates, request_queue, this) match {
        case Some(ticket) => {
          // TODO publish a EV_IntersectionOutcome
          accept(ticket)
        }
        case None => return
      }
    }
  }
  // Agents must pause a moment, be the head of their queue, and be close enough
  // to us (in case they looked ahead over small edges).
  private def candidates =
    request_queue.filter(ticket =>
      (ticket.a.is_stopped &&
       ticket.a.cur_queue.head.get == ticket.a &&
       // TODO * 1.5 is a tmp hack
       ticket.a.how_far_away(intersection) <= 1.5 * cfg.end_threshold &&
       !ticket.turn_blocked)
    )

  // Don't check for any collisions?!
  override def end_step() {
  }

  override def policy_type = IntersectionType.AIM

  case class Conflict(turn1: Turn, collision_dist1: Double, turn2: Turn, collision_dist2: Double) {
    // TODO awkward that all methods are duped for 1,2
    def time1(initial_speed: Double) =
      Physics.simulate_steps(collision_dist1, initial_speed, turn1.speed_limit)
    def time2(initial_speed: Double) =
      Physics.simulate_steps(collision_dist2, initial_speed, turn2.speed_limit)
  }

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
