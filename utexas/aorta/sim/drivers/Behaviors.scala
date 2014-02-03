// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.map.{Edge, Road, Turn, Traversable}
import utexas.aorta.sim.intersections.Ticket
import scala.collection.mutable

import utexas.aorta.common.{Util, cfg, Physics}

abstract class Behavior(a: Agent) {
  // As an optimization and to keep some stats on how successful lane-changing
  // is, remember the adjacent lane we'd like to switch into.
  // Start null to trigger the initial case of resetting it. Have to do it at
  // "sim time" when agent's actually first moving, otherwise the route might
  // not be ready to answer us.
  var target_lane: Option[Edge] = null

  protected var debug_me = false

  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()
  def wants_to_lc(): Boolean = target_lane != null && target_lane.isDefined

  def set_debug(value: Boolean) {
    debug_me = value
  }
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  def choose_action(): Action = Act_Set_Accel(0)

  def choose_turn(e: Edge) = e.next_turns.head

  def transition(from: Traversable, to: Traversable) = {}

  def dump_info() {
    Util.log("Idle behavior")
  }
}

// Reactively avoids collisions and obeys intersections by doing a conservative
// analysis of the next few steps.
class LookaheadBehavior(a: Agent, route: Route) extends Behavior(a) {
  private def reset_target_lane(base: Edge) = {
    target_lane = None
    // Did lookahead previously schedule a turn for this road? We can't
    // lane-change, then! Already committed.
    if (!a.get_ticket(base).isDefined) {
      val goal = route.pick_final_lane(base)._1
      val target = base.adjacent_lanes.minBy(choice => math.abs(choice.lane_num - goal.lane_num))
      // Tough liveness guarantees... give up early.
      // TODO move this check to give up to react()
      if (target != base && a.can_lc_without_blocking(target)) {
        target_lane = Some(target)
        Util.assert_eq(a.get_ticket(base).isDefined, false)
      }
    }
  }

  // Don't commit to turning from some lane in lookahead unless we're there are
  // LCing could still happen, or if it's a future edge with no other lanes.
  private def committed_to_lane(step: LookaheadStep) = step.at match {
    case e if e == a.at.on => !target_lane.isDefined
    case e: Edge => e.other_lanes.size == 1
    case t: Turn => throw new Exception(s"Requesting a turn from a turn $step?!")
  }
  
  // TODO Or lookup in tickets now?
  def choose_turn(e: Edge) = route.pick_turn(e)
  
  def transition(from: Traversable, to: Traversable) = {
    route.transition(from, to)
    // reset state
    to match {
      case e: Edge => reset_target_lane(e)
      case _ => target_lane = None
    }
  }

  def dump_info() {
    Util.log("Route-following behavior")
    Util.log(s"Target lane: $target_lane")
    route.dump_info
  }

  def choose_action(): Action = {
    // Do we want to lane change?
    // TODO these issues are both controlled by routing, so move this comment there
    // TODO 1) discretionary lane changing to pass people
    // TODO 2) routes can lookahead a bit to tell us to lane-change early
    
    // TODO awkward way to bootstrap this.
    if (target_lane == null) {
      // Should be an edge, since we start on edges.
      reset_target_lane(a.at.on.asInstanceOf[Edge])
    }

    // Commit to not lane-changing if it's too late. That way, we can decide on
    // a turn.
    target_lane match {
      case Some(target) => {
        // TODO move these changes to one place, with reset_target_lane
        // No room? Fundamentally impossible
        // Somebody in the way? If we're stalled and somebody's in the way,
        // we're probably waiting in a queue. Don't waste time hoping, grab a
        // turn now.
        if (!a.room_to_lc(target) || (a.is_stopped && !a.can_lc_without_crashing(target))) {
          target_lane = None
        }
      }
      case _ =>
    }

    return max_safe_accel
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

    var step = new LookaheadStep(
      a.at.on, a.kinematic.max_lookahead_dist, 0, a.at.dist_left, route
    )

    accel_for_lc_agent = constraint_lc_agent

    // Verify lookahead doesn't cycle to the same road twice; if it does, the route should pick the
    // second turn during the first choice!
    val visited = new mutable.HashSet[Road]()

    // If we don't have to stop for an intersection, keep caring about staying
    // far enough behind an agent. Once we have to stop somewhere, don't worry
    // about agents beyond that point.
    while (step != null && !accel_for_stop.isDefined) {
      step.at match {
        case e: Edge => {
          if (visited.contains(e.road)) {
            throw new Exception(s"Lookahead visited ${e.road} twice!")
          }
          visited += e.road
        }
        case _ =>
      }

      if (!accel_for_agent.isDefined) {
        accel_for_agent = constraint_agent(step)
      }

      if (!accel_for_stop.isDefined) {
        constraint_stop(step) match {
          case Left(constraint) => accel_for_stop = constraint
          case Right(done) => done_with_route = true
        }
      }

      // How many LCs do we anticipate here? Slown down to increase chances of doing multi-LCs
      // TODO (This is a bit ad-hoc)
      step.at match {
        case e: Edge => route.pick_final_lane(e) match {
          case (target, true) => {
            val num_lcs = math.abs(e.lane_num - target.lane_num)
            min_speed_limit = math.min(min_speed_limit, step.at.speed_limit / math.max(1, num_lcs))
          }
          case _ => // Don't slow down for unnecessary LCs
        }
        case _ =>
      }

      min_speed_limit = math.min(min_speed_limit, step.at.speed_limit)

      // Set the next step.
      step = step.next_step match {
        case Some(s) => s
        case None => null
      }
    }

    // TODO consider moving this first case to choose_action and not doing
    // lookahead when these premises hold true.
    return if (done_with_route) {
      Act_Done_With_Route()
    } else {
      val conservative_accel = List(
        accel_for_stop, accel_for_agent, accel_for_lc_agent,
        Some(a.kinematic.accel_to_achieve(min_speed_limit)),
        // Don't forget physical limits
        Some(a.max_accel)
      ).flatten.min
      //if (debug_me) {
      //  println(s"@ ${a.sim.tick}, ${a.id}'s at ${a.at.dist} with speed ${a.speed} and next accel $conservative_accel")
      //}

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
        Some(a.kinematic.accel_to_follow(other.kinematic, dist_away))
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
        Some(a.kinematic.accel_to_follow(other.kinematic, dist_away))
      }
      case None => None
    }
    case None => None
  }

  // Returns an optional acceleration, or 'true', which indicates the agent
  // is totally done.
  def constraint_stop(step: LookaheadStep): Either[Option[Double], Boolean] = {
    // Request a turn early?
    step.at match {
      case e: Edge if !route.done(e) => {
        if (!a.get_ticket(e).isDefined && committed_to_lane(step)) {
          val next_turn = route.pick_turn(e)
          val ticket = new Ticket(a, next_turn)
          a.add_ticket(ticket)
          e.to.intersection.request_turn(ticket)
        }
      }
      case _ =>
    }

    // The goal is to stop in the range [length - end_threshold, length),
    // preferably right at that left border.

    if (step.predict_dist < step.this_dist - cfg.end_threshold) {
      return Left(None)
    }

    // end of this current step's edge, that is
    val dist_from_agent_to_end = step.dist_ahead + step.this_dist

    val can_go: Boolean = step.at match {
      // Don't stop at the end of a turn
      case t: Turn => true
      // Stop if we're arriving at destination
      case e: Edge if route.done(e) => false
      // Otherwise, ask the intersection
      case e: Edge => a.get_ticket(route.pick_turn(e)) match {
        case Some(ticket) => {
          if (ticket.is_approved) {
            true
          } else {
            if (ticket.should_cancel) {
              // Try again. The routing should avoid choices that're filled up,
              // hopefully avoiding gridlock.
              ticket.cancel()
              // TODO if we end up doing the same thing... wtf, why? if theyre
              // all congested, do something..
              route.reroute(e)
              val next_turn = route.pick_turn(e)

              val replacement = new Ticket(a, next_turn)
              a.add_ticket(replacement)
              e.to.intersection.request_turn(replacement)
            }
            false
          }
        }
        case None => false
      }
    }
    if (can_go) {
      return Left(None)
    }

    // Are we completely done?
    val maybe_done = dist_from_agent_to_end <= cfg.end_threshold && a.is_stopped
    return a.at.on match {
      case e: Edge if route.done(e) && maybe_done => {
        Right(true)
      }
      case _ => {
        // We want to go the distance that puts us at length - end_threshold. If
        // we're already past that point (due to floating point imprecision, or
        // just because the edge is short), then try to cover enough distance to
        // get us to the start of the edge.
        val want_dist = math.max(
          step.dist_ahead, dist_from_agent_to_end - cfg.end_threshold
        )
        Left(Some(a.kinematic.accel_to_end(want_dist)))
      }
    }
  }
}

// This is a lazy sequence of edges/turns that tracks distances away from the
// original spot. This assumes no lane-changing: where the agent starts
// predicting is where they'll end up.
class LookaheadStep(
  // TODO dist_left_to_analyze, dist_so_far?
  val at: Traversable, val predict_dist: Double, val dist_ahead: Double,
  val this_dist: Double, route: Route
) {
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
  override def toString = f"Lookahead to $at with $predict_dist%.2f m left"

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
        next_at.length, route
      ))
}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Done_With_Route() extends Action
