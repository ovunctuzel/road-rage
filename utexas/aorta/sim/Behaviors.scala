// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.ListBuffer

import utexas.aorta.map.{Edge, Turn, Traversable, DirectedRoad}
import utexas.aorta.sim.analysis.Gridlock
import utexas.aorta.{Util, cfg}

abstract class Behavior(a: Agent) {
  val graph = a.graph

  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  override def choose_action() = Act_Set_Accel(0)

  override def choose_turn(e: Edge) = e.next_turns.head

  override def transition(from: Traversable, to: Traversable) = {}

  override def dump_info() = {
    Util.log("Idle behavior")
  }
}

// Reactively avoids collisions and obeys intersections by doing a conservative
// analysis of the next few steps.
class LookaheadBehavior(a: Agent, route: Route) extends Behavior(a) {
  // TODO this is the only state we keep; it would rock to get rid of it.
  // This is set the first time we choose to begin stopping, and it helps since
  // worst-case analysis says we won't toe the line, but we still want to invoke
  // the same math.
  var keep_stopping = false

  override def choose_turn(e: Edge): Turn = {
    route.next_step match {
      // Try to find a turn that leads to where we should go
      case Some(r) => e.turns_leading_to(r).headOption match {
        case Some(t) => return t
        // TODO re-route.
        case None => throw new Exception("Missed our lane!")
      }
      case None => throw new Exception("Route's done, we shouldn't be turning")
    }
  }
  
  override def transition(from: Traversable, to: Traversable) = {
    // The route only cares about entering new roads
    (from, to) match {
      case (t: Turn, e: Edge) => route.transition(t.from.directed_road, e.directed_road)
      case _ =>
    }
    keep_stopping = false   // reset
  }

  override def dump_info() = {
    Util.log("Route-following behavior")
    // Just show a few steps ahead
    Util.log_push
    for (step <- route.steps take 5) {
      Util.log("Step: " + step)
    }
    Util.log_pop
  }

  // If we're supposed to change to another lane, which adjacent lane gets us
  // closest?
  protected def desired_lane(): Option[Edge] =
    if (a.is_lanechanging)
      None
    else
      (route.next_step, a.at.on) match {
        // This query only makes sense while we're on an edge
        case (Some(group), cur_lane: Edge) => {
          val candidates = cur_lane.other_lanes.filter(
            e => !e.turns_leading_to(group).isEmpty
          )
          // Choose the closest candidate
          // TODO unlikely, but if we're in the middle and the far left and far
          // right lane both somehow lead to group, we could end up oscillating
          return candidates.sortBy(
            e => math.abs(e.lane_num - cur_lane.lane_num)
          ).headOption match {
            case Some(e) if e == cur_lane => None
            case Some(e) => Some(e)
            case None => throw new Exception("No lane to reach " + group + "!")
          }
        }
        case _ => None
      }

  protected def safe_to_lanechange(target: Edge): Boolean = {
    // TODO refactor this and make it something sensible
    val dist_required = cfg.lane_width * 10.0

    // Satisfy the physical model, which requires us to finish lane-changing
    // before reaching the intersection.
    if (dist_required + cfg.end_threshold >= a.at.dist_left) {
      return false
    }

    target.queue.closest_behind(a.at.dist) match {
      // If there's a trailing car on this road, require at least 2 ticks worth
      // of distance at the speed limit for the trailing car. Yes, probably too
      // conservative.
      case Some(avoid) => {
        val min_trailing_dist = 2.0 * cfg.dt_s * target.road.speed_limit
        if (a.at.dist - avoid.at.dist <= min_trailing_dist) {
          return false
        }
      }
      // It's impractical to flood backwards and find all the possible cars that
      // could enter our target lane soon. So use the same idea as safe spawning
      // distance and don't start a lane-change too early in the road. This
      // gives agents time next tick to notice us during their lookahead.
      case None => {
        if (a.at.dist <= a.at.on.safe_spawn_dist) {
          return false
        }
      }
    }

    // TODO make sure somebody can't decide to spawn in the way right as we
    // start to shift. when more things happen concurrently, just be careful.

    // TODO We'll back off speed quickly if we find ourselves tailing the guy
    // ahead of us in the target lane, but we should probably do some kind of
    // check to make sure we won't immediately plow through him. Use lookahead
    // engine.

    return true
  }

  override def choose_action(): Action = {
    // Do we want to lane change?
    desired_lane match {
      case Some(e) => {
        if (safe_to_lanechange(e)) {
          return Act_Lane_Change(e)
        } else {
          // TODO Ever a good idea to stall and wait? (Only do so if we didn't
          // fail the precondition of being too close to the end)
        }
      }
      case None =>
    }

    // TODO refactor and pull in max_safe_accel here.
    return max_safe_accel
  }

  // TODO describe. like an iterator.
  class LookaheadStep(
    val predict_dist: Double, val dist_ahead: Double,
    val at: Traversable, val next_dist: Double, val route_steps: Stream[DirectedRoad])
  {
    // predict_dist = how far ahead will we look
    // dist_ahead = how far have we looked ahead so far
    // at = where do we end up
    // next_dist = how much distance from 'at' we'll consider
    // TODO is next_dist ~ predict_dist, dist_ahead?
    // steps = alignment of the route

    // TODO iterator syntax

    lazy val next_step: Option[LookaheadStep] = {
      if (predict_dist <= 0.0) {
        None  // TODO ooh, ban dist of 0, then this is even simpler?
      } else {
        route_steps.headOption match {
          case Some(place) => Some(new LookaheadStep(
            predict_dist - next_dist, dist_ahead + next_dist,
            place, place.length, route_steps.tail
          ))
          // Done with route, stop looking ahead.
          case None => None
        }
      }
    }

    def steps(): Stream[LookaheadStep] = next_step match {
      case Some(step) => this #:: step.steps
      case None => this #:: Stream.empty
    }
  }

  // Finds the culprit, if they exist
  def check_for_blocked_turn(start_step: LookaheadStep): Option[Agent] = {
    // Look ahead from the start of the turn until follow_dist
    // after the end of it too see if anybody's there.
    val cautious_turn = start_step.route_steps.head match {
      case t: Turn => t
      case _       => throw new Exception("not a turn next?")
    }
    val cautious_edge = cautious_turn.to

    val check_steps = new LookaheadStep(
      // TODO + 0.5 as an epsilon...
      cautious_turn.length + cfg.follow_dist + 0.5,
      0, cautious_turn, cautious_turn.length,
      start_step.route_steps.tail
    )

    // If any of these have an agent, see where they are...
    for (step <- check_steps.steps) {
      step.at.queue.last match {
        // We can't block ourselves, even if we think we can
        case Some(check_me) if check_me != a => {
          // the dist they are "away from start of lookahead" will
          // be from the start of the turn... subtract that turn's
          // length; that gives us how far away they are from the
          // end of the turn. make sure THAT'S a nice comfy value.
          val dist_from_end_of_turn = (step.dist_ahead +
                                       check_me.at.dist - cautious_turn.length)
          // TODO some epsilon?
          // they too close?
          if (dist_from_end_of_turn <= cfg.follow_dist) {
            return Some(check_me)
          }
        }
        case _ =>
      }
    }
    // Nobody potentially dangerous
    return None
  }

  // Returns Act_Set_Accel almost always.
  def max_safe_accel(): Action = {
    // Since we can't react instantly, we have to consider the worst-case of the
    // next tick, which happens when we speed up as much as possible this tick.

    // the output.
    var stop_how_far_away: Option[Double] = None
    var stop_at: Option[Traversable] = None
    var stopping_for_destination = false
    var turn_blocked_by: Option[Agent] = None
    var follow_agent: Option[Agent] = None
    var follow_agent_how_far_away = 0.0   // gets set when follow_agent does.
    var min_speed_limit = Double.MaxValue

    // Stop caring once we know we have to stop for some intersection AND stay
    // behind some agent. Need both, since the agent we're following may not
    // need to stop, but we do.
    // TODO once we are bound by some intersection and theres no agent in
    // between us and it, cant we stop looking for agents?

    val first_step = new LookaheadStep(
      a.max_lookahead_dist, 0, a.at.on, a.at.dist_left, route.steps
    )

    for (step <- first_step.steps
         if (!stop_how_far_away.isDefined || !follow_agent.isDefined))
    {
      // 1) Agent
      // Worry either about the one we're following, or the one at the end of
      // the queue right now. It's the intersection's job to worry about letting
      // another wind up on our lane at the wrong time.
      if (!follow_agent.isDefined) {
        follow_agent = if (a.at.on == step.at)
                         a.cur_queue.ahead_of(a)
                       else
                         step.at.queue.last
        // This happens when we grab the last person off the next step's queue
        // for lanechanging. Lookahead for lanechanging will change soon anyway,
        // for now just avoid this case.
        if (follow_agent.isDefined && follow_agent.get == a) {
          follow_agent = None
        } else {
          follow_agent_how_far_away = follow_agent match {
            // A bit of a special case, that dist_ahead doesn't cover well.
            case Some(f) if f.at.on == a.at.on => f.at.dist - a.at.dist
            case Some(f) => step.dist_ahead + f.at.dist
            case _       => 0.0
          }
        }
      }

      // Do agent first to avoid doing some extra lookahead when looking for
      // turn_blocked_by.

      // 2) Stopping at the end
      if (!stop_how_far_away.isDefined) {
        // physically stop somewhat far back from the intersection.
        val should_stop = keep_stopping || (step.predict_dist >= step.next_dist - cfg.end_threshold)
        val how_far_away = step.dist_ahead + step.next_dist

        val stop_at_end: Boolean = should_stop && (step.at match {
          // Don't stop at the end of a turn
          case t: Turn => false
          // Stop if we're arriving at destination
          case e if !step.route_steps.headOption.isDefined => {
            stopping_for_destination = true
            true
          }
          // Otherwise, ask the intersection
          case e: Edge => {
            // BEFORE we ask the intersection, make sure nobody could prevent us
            // from completing the turn we want to do. This includes if we're
            // following some agent.

            // TODO could this cause starvation of somebody that never talks to
            // the intersection fast enough?
            // TODO could it ever work out so that we're approved (meaning
            // nobody was blocking us) but then another step, somebody new
            // blocks us?
            // TODO a decent invariant to verify: once an intersection approves
            // an agent, they shouldn't stall due to somebody blocking them

            // If we've already found somebody we're following, they must be
            // somewhere on the edge leading up to this intersection, so they
            // haven't started their turn yet. So there's a danger they could
            // take the same turn we're doing and cause us to not finish our
            // turn cleanly, possibly leading to gridlock.
            // In other words, it doesn't matter how far away they are -- they
            // haven't started their turn yet, so wait for them to completely
            // finish first.
            turn_blocked_by = if (follow_agent.isDefined)
                                None
                              else
                                check_for_blocked_turn(step)

            if (turn_blocked_by.isDefined || follow_agent.isDefined) {
              true  // stop
            } else {
              // Only now is it fair to ask the intersection. We aren't blocked
              // by any agent.
              assert(a.cur_queue.head.get == a)
              val i = e.to.intersection
              a.upcoming_intersections += i   // remember we've registered here
              val next_turn = step.route_steps.head match {
                case t: Turn => t
                case _ => throw new Exception("next_turn() called at wrong time")
              }
              !i.can_go(a, next_turn, how_far_away)
            }
          }
        })
        if (stop_at_end) {
          keep_stopping = true
          stop_how_far_away = Some(how_far_away)
          stop_at = Some(step.at)
        }
      }

      // 3) Speed limit
      step.at match {
        case e: Edge => {
          min_speed_limit = math.min(min_speed_limit, e.road.speed_limit)
        }
        case t: Turn => None
      }
    }

    if (stopping_for_destination && stop_how_far_away.get <= cfg.end_threshold && a.speed == 0.0) {
      // We can't get any closer to our actual destination. Terminate.
      return Act_Done_With_Route()
    } else {
      // So, three possible constraints.
      val a1 = stop_how_far_away match {
        case Some(dist) => {
          // Stop 'end_threshold' short of where we should when we can, but when
          // our destination is an edge, compromise and stop anywhere along it
          // we can. This handles a few stalemate cases with sequences of short
          // edges and possibly out-of-sync intersection policies.
          val last_stop = stop_at.get
          val go_this_dist = last_stop match {
            // creep forward to halfway along the shorty edge.
            case e: Edge if dist <= cfg.end_threshold => dist - (last_stop.length / 2.0)
            // stop back appropriately.
            case _                                    => dist - cfg.end_threshold
          }
          accel_to_end(go_this_dist)
        }
        case _          => Double.MaxValue
      }
      val a2 = follow_agent match {
        case Some(f) => accel_to_follow(f, follow_agent_how_far_away)
        case _       => Double.MaxValue
      }
      val a3 = a.accel_to_achieve(min_speed_limit)

      val conservative_accel = math.min(a1, math.min(a2, a3))

      // TODO better way to separate this out?
      Gridlock.handle_agent(a, conservative_accel, follow_agent, turn_blocked_by)

      // As the very last step, clamp based on our physical capabilities.
      return Act_Set_Accel(if (conservative_accel > 0)
                             math.min(conservative_accel, a.max_accel)
                           else
                             math.max(conservative_accel, -a.max_accel))
    }
  }

  private def accel_to_follow(follow: Agent, dist_from_them_now: Double): Double = {
    // Maintain our stopping distance away from this guy, plus don't scrunch
    // together too closely...

    // Again, reason about the worst-case: we speed up as much as possible, they
    // slam on their brakes.
    val us_worst_stop_dist = a.stopping_distance(a.max_next_speed)
    val most_we_could_go = a.max_next_dist
    val least_they_could_go = follow.min_next_dist

    // TODO this optimizes for next tick, so we're playing it really
    // conservative here... will that make us fluctuate more?
    val projected_dist_from_them = dist_from_them_now - most_we_could_go + least_they_could_go

    // don't ride bumper-to-bumper
    val desired_dist_btwn = us_worst_stop_dist + cfg.follow_dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = projected_dist_from_them - desired_dist_btwn

    // Try to cover whatever the distance is, and cap off our values.
    val accel = a.accel_to_cover(delta_dist)

    // TODO its a bit scary that this ever happens? does that mean we're too
    // close..?
    // Make sure we don't deaccelerate past 0 either.
    var accel_to_stop = a.accel_to_achieve(0)

    // TODO dumb epsilon bug. fix this better.
    val stop_speed = a.speed + (accel_to_stop * cfg.dt_s)
    if (stop_speed < 0) {
      accel_to_stop += 0.1
    }

    return math.max(accel_to_stop, accel)
  }

  // This is based on Piyush's proof.
  // how_far_away already includes end_threshold, if appropriate.
  private def accel_to_end(how_far_away: Double): Double = {
    // a, b, c for a normal quadratic
    val q_a = 1 / a.max_accel
    val q_b = cfg.dt_s
    val q_c = (a.speed * cfg.dt_s) - (2 * how_far_away)
    val try_speed = (-q_b + math.sqrt((q_b * q_b) - (4 * q_a * q_c))) / (2 * q_a)

    // TODO why does this or NaN ever happen?
    /*if (desired_speed < 0) {
      Util.log("why neg speed?")
    } else if (desired_speed.isNaN) {
      // try seed 1327894373344 to make it happen, though. synthetic.
      Util.log("NaN speed... a=" + q_a + ", b=" + q_b + ", c=" + q_c)
    }*/

    // in the NaN case, just try to stop?
    val desired_speed = if (try_speed.isNaN)
                          0
                        else
                          math.max(0, try_speed)

    val needed_accel = a.accel_to_achieve(desired_speed)

    // TODO dumb epsilon bug again. fix this better.
    val stop_speed = a.speed + (needed_accel * cfg.dt_s)
    return if (stop_speed < 0)
             needed_accel + 0.1
           else
             needed_accel
  }
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
final case class Act_Done_With_Route() extends Action
