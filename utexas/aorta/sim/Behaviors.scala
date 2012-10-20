// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.ListBuffer

import utexas.aorta.map.{Edge, Turn, Traversable, DirectedRoad}
import utexas.aorta.analysis.{Gridlock, Profiling}
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

  // As an optimization and to keep some stats on how successful lane-changing
  // is, remember the adjacent lane we'd like to switch into.
  // Start null to trigger the initial case of resetting it. Have to do it at
  // "sim time" when agent's actually first moving, otherwise the route might
  // not be ready to answer us.
  var target_lane: Option[Edge] = null

  def reset_target_lane(base: Edge) = {
    target_lane = None
    base match {
      case e: Edge => {
        val target = Profiling.desired_lane.time(
          () => route.pick_lane(e)
        )
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
    keep_stopping = false
    to match {
      case e: Edge => reset_target_lane(e)
      case _ => target_lane = None
    }
  }

  override def dump_info() = {
    Util.log("Route-following behavior")
    // TODO ask the route to debug itself? :P
  }

  protected def safe_to_lanechange(target: Edge): Boolean = {
    // One lane could be shorter than the other. When we want to avoid the end
    // of a lane, worry about the shorter one to be safe.
    val min_len = math.min(a.at.on.length, target.length)

    // Satisfy the physical model, which requires us to finish lane-changing
    // before reaching the intersection.
    if (a.at.dist + cfg.lanechange_dist + cfg.end_threshold >= min_len) {
      return false
    }

    // Furthermore, we probably have to stop for the intersection, so be sure we
    // have enough room to do that.
    if (a.at.dist + a.stopping_distance(a.max_next_speed) >= min_len) {
      return false
    }

    target.queue.closest_behind(a.at.dist) match {
      // If there's a trailing car on this road, require at least 2 ticks worth
      // of distance at the speed limit for the trailing car. Yes, probably too
      // conservative.
      // TODO this depends on agent ordering per tick. 2 agents may try to merge
      // into the same lane at the same spot in the same tick. how to
      // parallelize?
      case Some(avoid) => {
        val min_trailing_dist = 2.0 * cfg.dt_s * target.road.speed_limit
        if (a.at.dist - avoid.at.dist <= min_trailing_dist) {
          return false
        }
        // TODO tmp debug
        //Util.log(s"$a at ${a.at} sees $avoid as biggest threat")
      }
      // It's impractical to flood backwards and find all the possible cars that
      // could enter our target lane soon. So use the same idea as safe spawning
      // distance and don't start a lane-change too early in the road. This
      // gives agents time next tick to notice us during their lookahead.
      case None => {
        if (a.at.dist <= a.at.on.queue.worst_entry_dist + cfg.follow_dist) {
          //Util.log("(wont lc) not past min spawn dist yet")
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
    // TODO 1) discretionary lane changing to pass people
    // TODO 2) routes can lookahead a bit to tell us to lane-change early
    
    // TODO awkward way to bootstrap this.
    if (target_lane == null) {
      // Should be an edge, since we start on edges.
      reset_target_lane(a.at.on.asInstanceOf[Edge])
    }
    if (!a.is_lanechanging) {
      target_lane match {
        case Some(e) => {
          if (Profiling.safe_lane.time(() => safe_to_lanechange(e))) {
            return Act_Lane_Change(e)
          }
        }
        case _ =>
      }
    }

    // TODO refactor and pull in max_safe_accel here.
    return Profiling.react_accel.time(max_safe_accel)
  }

  // This is a lazy sequence of edges/turns that tracks distances away from the
  // original spot. This assumes no lane-changing: where the agent starts
  // predicting is where they'll end up.
  class LookaheadStep(
    // TODO dist_left_to_analyze, dist_so_far?
    val at: Traversable, val predict_dist: Double, val dist_ahead: Double,
    val this_dist: Double)
  {
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

    def next_at = at match {
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
          
    // this is the make-lazy boilerplate
    def steps(): Stream[LookaheadStep] = next_step match {
      case Some(step) => this #:: step.steps
      case None => this #:: Stream.empty
    }
  }

  def lookahead_steps = (new LookaheadStep(
    a.at.on, a.max_lookahead_dist, 0, a.at.dist_left
  )).steps

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

    // TODO pull each constraint out into its own function

    for (step <- lookahead_steps
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

      // 2) Stopping at the end
      if (!stop_how_far_away.isDefined) {
        // physically stop somewhat far back from the intersection.
        val should_stop = keep_stopping || (step.predict_dist >= step.this_dist - cfg.end_threshold)
        val how_far_away = step.dist_ahead + step.this_dist

        val stop_at_end: Boolean = should_stop && (step.at match {
          // Don't stop at the end of a turn
          case t: Turn => false
          // Stop if we're arriving at destination
          case e: Edge if route.done(e) => {
            stopping_for_destination = true
            true
          }
          // Lookahead has scanned far ahead enough
          case e: Edge if !step.next_step.isDefined => true
          // Otherwise, ask the intersection
          case e: Edge => {
            val i = e.to.intersection
            a.upcoming_intersections += i   // remember we've registered here
            val next_turn = step.next_step.get.at match {
              case t: Turn => t
              case _ => throw new Exception("lookahead didnt get a turn")
            }
            // TODO verify we're telling the intersection the same turn between
            // ticks
            !i.can_go(a, next_turn, how_far_away)
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

    // We can't get any closer to our actual destination. Terminate.
    // TODO consider moving this first case to choose_action and not doing
    // lookahead when these premises hold true.
    if (stopping_for_destination && stop_how_far_away.get <= cfg.end_threshold && a.speed == 0.0) {
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

      // TODO hopefully gridlock is better, don't need to do this analyis so
      // much
      /*Profiling.debug.start
      Gridlock.handle_agent(a, conservative_accel, follow_agent, turn_blocked_by)
      Profiling.debug.stop*/

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
