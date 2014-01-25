// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.map.{Turn, Line}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.common.{Physics, cfg}

import scala.collection.mutable

// TODO refactor with reservation policy.
class AIMPolicy(intersection: Intersection, ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  private val conflict_map: Map[Turn, Map[Turn, Conflict]] = find_conflicts()

  private var interruption: Option[Ticket] = None

  // These're for conflict detection
  // How far along their turn each agent was during last tick
  private val dist_last_tick = new mutable.HashMap[Agent, Double]()
  private val exited_this_tick = new mutable.HashSet[Ticket]()

  // TODO serialization

  override def react() {
    // TODO interruptions or not? ideally, keep accepting people that have no conflict with the
    // interruption.
    interruption match {
      case Some(ticket) => {
        if (can_accept(ticket)) {
          ticket.approve()
          interruption = None
        } else {
          return
        }
      }
      case None =>
    }

    while (!interruption.isDefined) {
      ordering.choose(candidates, request_queue, this) match {
        case Some(ticket) => {
          // TODO publish a EV_IntersectionOutcome
          if (can_accept(ticket)) {
            accept(ticket)
          } else {
            interruption = Some(ticket)
            ticket.is_interruption = true
            unqueue(ticket)
            // Furthermore, grab a spot for them and keep it!
            ticket.turn.to.queue.allocate_slot
            return
          }
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
  private def can_accept(ticket: Ticket): Boolean = {
    // See if there's a predicted conflict with any agent that's been accepted
    for (t <- accepted if t.turn.conflicts_with(ticket.turn)) {
      val our_time = ticket.a.sim.tick + conflict_map(t.turn)(ticket.turn).time(ticket.turn, 0)
      val their_time = t.accept_tick + conflict_map(t.turn)(ticket.turn).time(t.turn, 0)
      println(s"${ticket.a} @ $our_time vs ${t.a} @ $their_time")
      if (our_time == their_time) {
        println(s"... oh shit, ${ticket.a} and ${t.a} gonna clash at $our_time")
      }
    }
    return true
  }

  override def cancel_turn(ticket: Ticket) {
    interruption match {
      case Some(t) if t == ticket => interruption = None
      case _ => super.cancel_turn(ticket)
    }
  }

  // Check for collisions by seeing how close agents are to pre-defined collision point
  override def end_step() {
    val in_intersection = accepted.filter(t => t.a.at.on == t.turn) ++ exited_this_tick
    // O(n^2 / 2), n = agents doing turns
    for (t1 <- in_intersection; t2 <- in_intersection if t1.a.id.int < t2.a.id.int) {
      if (t1.turn.conflicts_with(t2.turn)) {
        val conflict = conflict_map(t1.turn)(t2.turn)

        // Are the agents near the collision point? Negative means before point, positive means
        // after it
        val delta1 =
          if (exited_this_tick.contains(t1)) t1.turn.length else t1.a.at.dist - conflict.dist(t1.turn)
        val delta2 =
          if (exited_this_tick.contains(t2)) t2.turn.length else t2.a.at.dist - conflict.dist(t2.turn)
        //println(s"Possible conflict between ${t1.a} ($delta1 away from danger) and ${t2.a} ($delta2 away)")

        // If the agents cross the point (neg -> pos) the same tick, then they definitely hit!
        // If they're not in dist_last_tick, then this is their first tick in the intersection. If
        // they're already positive (past the point), then they crossed it! Hence default value of
        // 0.
        // TODO weird type inference issues here, hence intermediates
        //val old_delta1 = dist_last_tick.getOrElse(t1.a, 0) - conflict.dist(t1.turn)
        //val old_delta2 = dist_last_tick.getOrElse(t2.a, 0) - conflict.dist(t2.turn)
        val old_dist1: Double = dist_last_tick.getOrElse(t1.a, 0)
        val old_delta1 = old_dist1 - conflict.dist(t1.turn)
        val old_dist2: Double = dist_last_tick.getOrElse(t2.a, 0)
        val old_delta2 = old_dist2 - conflict.dist(t2.turn)
        if ((old_delta1 < 0 && delta1 > 0) && (old_delta2 < 0 && delta2 > 0)) {
          throw new Exception(s"${t1.a} and ${t2.a} crossed at an AIM intersection at ${t1.a.sim.tick}!")
        }
      }

      // Update distances
      for (t <- in_intersection) {
        dist_last_tick(t.a) = t.a.at.dist
      }
    }

    // Cleanup
    dist_last_tick --= exited_this_tick.map(_.a)
    exited_this_tick.clear()
  }

  override def handle_exit(t: Ticket) {
    super.handle_exit(t)
    exited_this_tick += t
    // We want to run end_step the tick agents leave, even if none are left in the intersection,
    // since there's a case where exiting agents collide.
    t.a.sim.active_intersections += intersection
  }

  override def policy_type = IntersectionType.AIM

  case class Conflict(turn1: Turn, collision_dist1: Double, turn2: Turn, collision_dist2: Double) {
    // TODO awkward that all methods are duped for 1,2

    def dist(turn: Turn) = turn match {
      case `turn1` => collision_dist1
      case `turn2` => collision_dist2
      case _ => throw new IllegalArgumentException("$turn doesn't belong to $this")
    }

    def time(turn: Turn, initial_speed: Double) = turn match {
      case `turn1` => Physics.simulate_steps(collision_dist1, initial_speed, turn1.speed_limit)
      case `turn2` => Physics.simulate_steps(collision_dist2, initial_speed, turn2.speed_limit)
      case _ => throw new IllegalArgumentException("$turn doesn't belong to $this")
    }
  }

  private def find_conflicts(): Map[Turn, Map[Turn, Conflict]] = {
    val all_conflicts =
      for (t1 <- intersection.v.turns; t2 <- intersection.v.turns if t1 != t2)
        yield make_conflict(t1, t2)
    val map = intersection.v.turns.map(t => t -> new mutable.HashMap[Turn, Conflict]).toMap
    for (c <- all_conflicts.flatten) {
      map(c.turn1)(c.turn2) = c
      map(c.turn2)(c.turn1) = c
    }
    return intersection.v.turns.map(t => t -> map(t).toMap).toMap
  }

  private def make_conflict(turn1: Turn, turn2: Turn): Option[Conflict] =
    turn1.conflict_line.segment_intersection(turn2.conflict_line) match {
      case Some(pt) => Some(Conflict(
        turn1, new Line(turn1.conflict_line.start, pt).length,
        turn2, new Line(turn2.conflict_line.start, pt).length
      ))
      // if same destination lane and doesnt conflict normally by the line, force collision at end
      case None if turn1.to == turn2.to => Some(Conflict(turn1, turn1.length, turn2, turn2.length))
      case None => None
    }
}
