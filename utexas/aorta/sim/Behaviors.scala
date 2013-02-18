// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.ListBuffer

import utexas.aorta.map.{Edge, Turn, Traversable, DirectedRoad}
import utexas.aorta.analysis.{Stats, Turn_Request_Stat}

import utexas.aorta.{Util, Common, cfg}

abstract class Behavior(a: Agent) {
  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()
  def wants_to_lc(): Boolean = target_lane != null && target_lane.isDefined
  def how_far_away(i: Intersection): Double

  // As an optimization and to keep some stats on how successful lane-changing
  // is, remember the adjacent lane we'd like to switch into.
  // Start null to trigger the initial case of resetting it. Have to do it at
  // "sim time" when agent's actually first moving, otherwise the route might
  // not be ready to answer us.
  var target_lane: Option[Edge] = null
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  override def choose_action(): Action = Act_Set_Accel(0)

  override def choose_turn(e: Edge) = e.next_turns.head

  override def transition(from: Traversable, to: Traversable) = {}

  override def dump_info() = {
    Util.log("Idle behavior")
  }

  override def how_far_away(i: Intersection) = 42.0
}

// Reactively avoids collisions and obeys intersections by doing a conservative
// analysis of the next few steps.
class LookaheadBehavior(a: Agent, route: Route) extends Behavior(a) {
  def reset_target_lane(base: Edge) = {
    target_lane = None
    base match {
      case e: Edge => {
        val target = route.pick_lane(e)
        if (target != base) {
          target_lane = Some(target)
        }
      }
      case _ =>
    }
  }

  override def choose_turn(e: Edge) = route.pick_turn(e)
  
  override def transition(from: Traversable, to: Traversable) = {
    (from, to) match {
      // When we lane-change, hopefully we haven't scheduled reservations,
      // right?
      case (_: Edge, _: Edge) => {
        // TODO actually, make sure there arent any of these...
        a.cancel_intersection_reservations
      }
      case _ =>
    }

    route.transition(from, to)
    // reset state
    to match {
      case e: Edge => reset_target_lane(e)
      case _ => target_lane = None
    }
  }

  override def dump_info() = {
    Util.log("Route-following behavior")
    route.dump_info
  }

  override def choose_action(): Action = {
    // Do we want to lane change?
    // TODO 1) discretionary lane changing to pass people
    // TODO 2) routes can lookahead a bit to tell us to lane-change early
    
    // TODO awkward way to bootstrap this.
    if (target_lane == null) {
      // Should be an edge, since we start on edges.
      reset_target_lane(a.at.on.asInstanceOf[Edge])
    }

    // Try a fast-path!
    val no_lc = (!a.is_lanechanging)
    val not_near_end = a.at.dist_left >= a.max_lookahead_dist + cfg.end_threshold // TODO +buf?
    val lead = a.our_lead
    val not_tailing = lead match {
      case Some(l) =>
        // TODO or just stopping dist at max next speed?
        (l.at.dist - a.at.dist) >= (a.max_lookahead_dist + cfg.follow_dist)
      case None => true
    }
    val at_speed = a.speed == a.at.on.speed_limit
    // TODO more fast paths that dont do full analysis
    // TODO go back and prove these are equivalent to the original semantics
    if (no_lc && not_near_end) {
      if (not_tailing && at_speed) {
        return Act_Set_Accel(0)
      } else if (not_tailing) {
        // so we're not at speed
        return Act_Set_Accel(
          math.min(a.accel_to_achieve(a.at.on.speed_limit), a.max_accel)
        )
      } else if (at_speed) {
        // so we're tailing
        return Act_Set_Accel(math.max(
          accel_to_follow(lead.get, lead.get.at.dist - a.at.dist),
          -a.max_accel
        ))
      }
    }

    // TODO refactor and pull in max_safe_accel here? maybe this function is for
    // fast-paths.
    return max_safe_accel
  }

  // This is a lazy sequence of edges/turns that tracks distances away from the
  // original spot. This assumes no lane-changing: where the agent starts
  // predicting is where they'll end up.
  class LookaheadStep(
    // TODO dist_left_to_analyze, dist_so_far?
    val at: Traversable, val predict_dist: Double, val dist_ahead: Double,
    val this_dist: Double)
  {
    // Steps start at the beginning of 'at', except for the 'first' lookahead
    // step. this_dist encodes that case. But dist_ahead is a way of measuring
    // how far the agent really is right now from something in the future.
    // predict_dist = how far ahead we still have to look
    // TODO consider seeding dist_ahead with not 0 but this_dist, then lots of
    // stuff may get simpler.
    // dist_ahead = how far have we looked ahead so far
    // at = where do we end up
    // this_dist = how much distance from 'at' we'll consider. it would just be
    // length, except for the very first step of a lookahead, since the agent
    // doesnt start at the beginning of the step.
    override def toString = "Lookahead to %s with %.2f m left".format(at, predict_dist)

    // TODO iterator syntax

    // TODO this and next_at, maybe move them out of this class
    // TODO the way this gets used is a bit redundant
    def is_last_step = at match {
      case e: Edge => route.done(e)
      case _ => false
    }

    lazy val next_at = at match {
      case e: Edge => route.pick_turn(e)
      case t: Turn => t.to
    }

    lazy val next_step: Option[LookaheadStep] =
      if (predict_dist - this_dist <= 0.0 || is_last_step)
        None
      else
        Some(new LookaheadStep(
          next_at, predict_dist - this_dist, dist_ahead + this_dist,
          next_at.length
        ))
  }

  // Returns Act_Set_Accel almost always.
  def max_safe_accel(): Action = {
    // Since we can't react instantly, we have to consider the worst-case of the
    // next tick, which happens when we speed up as much as possible this tick.

    // the output.
    var accel_for_stop: Option[Double] = None
    var accel_for_agent: Option[Double] = None
    var accel_for_lc_agent: Option[Double] = None
    var min_speed_limit = Double.MaxValue
    var done_with_route = false

    // Stop caring once we know we have to stop for some intersection AND stay
    // behind some agent. Need both, since the agent we're following may not
    // need to stop, but we do.
    // TODO once we are bound by some intersection and theres no agent in
    // between us and it, cant we stop looking for agents?

    var step = new LookaheadStep(
      a.at.on, a.max_lookahead_dist, 0, a.at.dist_left
    )

    accel_for_lc_agent = constraint_lc_agent

    while (step != null && (!accel_for_agent.isDefined || !accel_for_stop.isDefined))
    {
      if (!accel_for_agent.isDefined) {
        accel_for_agent = constraint_agent(step)
      }

      if (!accel_for_stop.isDefined) {
        constraint_stop(step) match {
          case Left(constraint) => accel_for_stop = constraint
          case Right(done) => done_with_route = true
        }
      }

      min_speed_limit = math.min(min_speed_limit, step.at.speed_limit)

      // Set the next step.
      step = step.next_step match {
        case Some(s) => s
        case None => null
      }
    }

    // We can't get any closer to our actual destination. Terminate.
    // TODO consider moving this first case to choose_action and not doing
    // lookahead when these premises hold true.
    return if (done_with_route) {
      Act_Done_With_Route()
    } else {
      val conservative_accel = List(
        accel_for_stop, accel_for_agent, accel_for_lc_agent,
        Some(a.accel_to_achieve(min_speed_limit)),
        // Don't forget physical limits
        Some(a.max_accel)
      ).flatten.min

      // As the very last step, clamp based on our physical capabilities.
      Act_Set_Accel(math.max(conservative_accel, -a.max_accel))
    }
  }

  // All constraint functions return a limiting acceleration, if relevant
  // Don't plow into people
  def constraint_agent(step: LookaheadStep): Option[Double] = {
    val follow_agent = if (a.at.on == step.at)
                         a.cur_queue.ahead_of(a)
                       else
                         step.at.queue.last
    return follow_agent match {
      // This happens when we grab the last person off the next step's queue
      // for lanechanging. Lookahead for lanechanging will change soon anyway,
      // for now just avoid this case. TODO
      case Some(other) if a == other => None  // TODO next to last?
      case Some(other) => {
        val dist_away = if (other.on(a.at.on))
                          other.at.dist - a.at.dist
                        else
                          step.dist_ahead + other.at.dist
        Some(accel_to_follow(other, dist_away))
      }
      case None => None
    }
  }

  // When we're lane-changing, lookahead takes care of the new path. But we
  // still have to pay attention to exactly one other agent: the one in front of
  // us on our old lane.
  def constraint_lc_agent(): Option[Double] = a.old_lane match {
    case Some(e) => e.queue.ahead_of(a) match {
      case Some(other) => {
        val dist_away = other.at.dist - a.at.dist
        Some(accel_to_follow(other, dist_away))
      }
      case None => None
    }
    case None => None
  }

  // Returns an optional acceleration, or 'true', which indicates the agent
  // is totally done.
  def constraint_stop(step: LookaheadStep): Either[Option[Double], Boolean] = {
    // The goal is to stop in the range [length - end_threshold, length),
    // preferably right at that left border.

    if (step.predict_dist < step.this_dist - cfg.end_threshold) {
      return Left(None)
    }

    // end of this current step's edge, that is
    val dist_from_agent_to_end = step.dist_ahead + step.this_dist

    val stop_at_end: Boolean = step.at match {
      // Don't stop at the end of a turn
      case t: Turn => false
      // Stop if we're arriving at destination
      case e: Edge if route.done(e) => {
        // Are we completely done?
        // TODO epsilon here more fair?
        if (dist_from_agent_to_end <= cfg.end_threshold && a.speed == 0.0) {
          // TODO dont return from deep inside here
          return Right(true)
        }
        true
      }
      // Otherwise, ask the intersection
      case e: Edge => {
        val i = e.to.intersection
        if (a.turns_approved(i)) {
          false
        } else {
          if (!a.turns_requested(i)) {
            // If the lookahead includes a next step, this should be consistent
            // with what the route says, by the route's contract of consistent
            // answers. But sometimes the lookahead terminates early due to not
            // enough distance, so just always ask this.
            val next_turn = route.pick_turn(e)
            i.request_turn(a, next_turn)
            a.turns_requested += i
            Stats.record(Turn_Request_Stat(
              a.id, e.to.id, Common.tick, a.wallet.budget
            ))
          }
          true
        }
      }
    }
    if (!stop_at_end) {
      return Left(None)
    }

    // We want to go the distance that puts us at length - end_threshold. If
    // we're already past that point (due to floating point imprecision, or just
    // because the edge is short), then try to cover enough distance to get us
    // to the start of the edge.
    val want_dist = math.max(step.dist_ahead, dist_from_agent_to_end - cfg.end_threshold)
    return Left(Some(accel_to_end(want_dist)))
  }

  private def accel_to_follow(follow: Agent, dist_from_them_now: Double): Double = {
    val us_worst_stop_dist = a.stopping_distance(a.max_next_speed)
    val most_we_could_go = a.max_next_dist
    val least_they_could_go = follow.min_next_dist

    // TODO this optimizes for next tick, so we're playing it really
    // conservative here... will that make us fluctuate more?
    val projected_dist_from_them = dist_from_them_now - most_we_could_go + least_they_could_go
    val desired_dist_btwn = us_worst_stop_dist + cfg.follow_dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = projected_dist_from_them - desired_dist_btwn

    // Try to cover whatever the distance is
    return a.accel_to_cover(delta_dist)
  }

  // Find an accel to travel want_dist and wind up with speed 0.
  private def accel_to_end(want_dist: Double): Double = {
    if (want_dist > 0.0) {
      if (a.speed > 0.0) {
        // d = (v_1)(t) + (1/2)(a)(t^2)
        // 0 = (v_1) + (a)(t)
        // Eliminating time yields the formula for accel below.

        // If this accel puts us past speed 0, it's fine, we just idle for the
        // remainder of the timestep.
        return (-1 * a.speed * a.speed) / (2 * want_dist)
      } else {
        // We have to accelerate so that we can get going, but not enough so
        // that we can't stop. Want (1/2)(a_1)(dt)^2 + [(a_1)(dt)](dt) -
        // (1/2)(a_max)(dt)^2 = want_dist, algebra gives the answer below.
        val coefficient = (2.0 / 3.0) / (cfg.dt_s * cfg.dt_s)
        return (coefficient * want_dist) + (a.max_accel / 3.0)
      }
    } else {
      // Special case for distance of 0: avoid a NaN, just stop.
      return a.accel_to_stop
    }
  }

  override def how_far_away(i: Intersection): Double = {
    // Because the route should be determinstic, just keep asking it how to get
    // there and sum distances.
    var total = 0.0
    // If we're being asked this, we've requested the turn, meaning no more
    // lane-changing needs to happen.
    var at = a.at.on
    while (true) {
      if (at == a.at.on) {
        total += a.at.dist_left
      } else {
        total += at.length
      }
      at match {
        case e: Edge => {
          if (e.to.intersection == i) {
            return total
          } else {
            at = route.pick_turn(e)
          }
        }
        case t: Turn => {
          at = t.to
        }
      }
    }
    throw new Exception(s"how_far_away broke looking for $i")
  }
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Done_With_Route() extends Action
