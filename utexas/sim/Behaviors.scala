package utexas.sim

import utexas.map.{Edge, Turn, Traversable}
import utexas.{Util, cfg}

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

  override def transition(from: Traversable, to: Traversable) = {}   // mmkay

  override def dump_info() = {
    Util.log("Idle behavior")
  }
}

// the actual mechanics of the route are left up to something else, though
class RouteFollowingBehavior(a: Agent, route: Route) extends Behavior(a) {
  // This is set the first time we choose to begin stopping, and it helps since
  // worst-case analysis says we won't toe the line, but we still want to invoke
  // the same math.
  var keep_stopping = false

  override def choose_action(): Action = (a.at, route.next_step) match {
    case (Position(e1: Edge, _), Some(e2: Edge)) => {
      // TODO choose the right time
      return Act_Lane_Change(e2)
    }
    case _ => max_safe_accel
  }

  override def choose_turn(e: Edge): Turn = route.next_step match {
    case Some(t: Turn) => t
    case _             => throw new Exception("Asking " + a + " to choose turn now?!")
  }

  override def transition(from: Traversable, to: Traversable) = {
    route.transition(from, to)
    keep_stopping = false   // reset
  }

  override def dump_info() = {
    Util.log("Route-following behavior")
  }

  // Returns Act_Set_Accel almost always.
  def max_safe_accel(): Action = {
    // Since we can't react instantly, we have to consider the worst-case of the
    // next tick, which happens when we speed up as much as possible this tick.
    val worst_speed = a.speed + (a.max_accel * cfg.dt_s)
    val worst_travel = Util.dist_at_constant_accel(a.max_accel, cfg.dt_s, a.speed)
    val worst_stop_dist = a.stopping_distance(worst_speed)

    // the input to this lookahead process.
    var lookahead = worst_travel + worst_stop_dist
    var looked_ahead_so_far = 0.0   // how far from a.at to beginning of step
    var step = a.at.on
    var dist_left = a.at.dist_left
    var steps_ahead = 0
    // Only call when next move really is a turn.
    def next_turn() = route.lookahead_step(steps_ahead) match {
      case Some(t: Turn) => t
      case _ => throw new Exception("next_turn() called at wrong time")
    }

    // and the output.
    var stop_how_far_away: Option[Double] = None
    var stopping_for_destination = false
    var follow_agent: Option[Agent] = None
    var follow_agent_how_far_away = 0.0   // gets set when follow_agent does.
    var min_speed_limit = Double.MaxValue

    // Stop caring once we know we have to stop for some intersection AND stay
    // behind some agent. Need both, since the agent we're following may not
    // need to stop, but we do.
    while (lookahead > 0.0 && (!stop_how_far_away.isDefined || !follow_agent.isDefined)) {
      // 1) Stopping at the end
      if (!stop_how_far_away.isDefined) {
        // physically stop somewhat far back from the intersection.
        val should_stop = keep_stopping || (lookahead >= dist_left - cfg.end_threshold)
        val how_far_away = looked_ahead_so_far + dist_left
        // TODO tell them how long we've been waiting
        val stop_at_end: Boolean = should_stop && (step match {
          // Don't stop at the end of a turn
          case t: Turn => false
          // Stop if we're arriving at destination
          case e if !route.lookahead_step(steps_ahead).isDefined => {
            stopping_for_destination = true
            true
          }
          // Otherwise, ask the intersection
          case e: Edge => {
            val i = Agent.sim.intersections(e.to)
            a.upcoming_intersections += i   // remember we've registered here
            i.should_stop(a, next_turn, how_far_away)
          }
        })
        if (stop_at_end) {
          keep_stopping = true
          stop_how_far_away = Some(how_far_away)
        }
      }

      // 2) Agent
      // Worry either about the one we're following, or the one at the end of
      // the queue right now. It's the intersection's job to worry about letting
      // another wind up on our lane at the wrong time.
      if (!follow_agent.isDefined) {
        follow_agent = if (a.at.on == step)
                         a.cur_queue.ahead_of(a)
                       else
                         Agent.sim.queues(step).last
        follow_agent_how_far_away = follow_agent match {
          // A bit of a special case, that looked_ahead_so_far doesn't cover well.
          case Some(f) if f.at.on == a.at.on => f.at.dist - a.at.dist
          case Some(f) => looked_ahead_so_far + f.at.dist
          case _       => 0.0
        }
      }

      // 3) Speed limit
      step match {
        case e: Edge => { min_speed_limit = math.min(min_speed_limit, e.road.speed_limit) }
        case t: Turn => None
      }

      // Now, prepare for the next step.
      lookahead -= dist_left
      if (!route.lookahead_step(steps_ahead).isDefined) {
        // done with route. stop considering possibilities immediately.
        lookahead = -1
      } else {
        looked_ahead_so_far += dist_left
        // order matters... steps_ahead will be what follows this new 'step'
        step = route.lookahead_step(steps_ahead).get
        steps_ahead += 1
        // For all but the first step, we have all of its distance left.
        dist_left = step.length
      }
    }

    if (stopping_for_destination && stop_how_far_away.get <= cfg.end_threshold && a.speed == 0.0) {
      // We can't get any closer to our actual destination. Terminate.
      return Act_Done_With_Route()
    } else {
      // So, three possible constraints.
      val a1 = stop_how_far_away match {
        case Some(dist) => {
          // in most normal circumstances, we take away end_threshold, since
          // stopping right at the end of the edge makes us technically enter
          // the intersection
          // but when we're in the middle of a turn and that would make the
          // distance be <= 0, we don't want to stop; that causes deadlock. it
          // means the next edge is tiny. so just stop along it where we can.
          val go_this_dist = a.at.on match {
            // just go halfway whatever's available.
            case t: Turn if dist <= cfg.end_threshold => accel_to_end(dist / 2)
            case _ => dist - cfg.end_threshold
          }
          accel_to_end(go_this_dist)
        }
        case _          => Double.MaxValue
      }
      val a2 = follow_agent match {
        case Some(f) => accel_to_follow(f, follow_agent_how_far_away)
        case _       => Double.MaxValue
      }
      val a3 = accel_to_achieve(min_speed_limit)

      val conservative_accel = math.min(a1, math.min(a2, a3))

      // let the other intersections that are letting us go know that we
      // won't be going just yet,
      // IF we are not going to make a move this tick.

      // This means we're holding a valuable resource...
      if (a.speed == 0.0 && conservative_accel == 0.0) {
        /*a.at.on match {
          case t: Turn if a2 == 0.0 => {
            // And the cause is an agent in front of us. If this situation persists,
            // we could enter gridlock.
            Util.log("Gridlock possible near " + a)
          }
          case _ =>
        }*/

        // let them ignore us if we're not owner
        a.upcoming_intersections.foreach(p => p.yield_lock(a))
      }

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
    // TODO share this with max_safe_accel
    val us_worst_speed = a.speed + (a.max_accel * cfg.dt_s)
    val us_worst_stop_dist = a.stopping_distance(us_worst_speed)
    val most_we_could_go = Util.dist_at_constant_accel(a.max_accel, cfg.dt_s, a.speed)

    // TODO clamp v_f at 0, since they cant deaccelerate into negative speed.
    val least_they_could_go = Util.dist_at_constant_accel(-follow.max_accel, cfg.dt_s, follow.speed)

    // TODO this optimizes for next tick, so we're playing it really
    // conservative here... will that make us fluctuate more?
    val projected_dist_from_them = dist_from_them_now - most_we_could_go + least_they_could_go

    // don't ride bumper-to-bumper
    val desired_dist_btwn = us_worst_stop_dist + cfg.follow_dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = projected_dist_from_them - desired_dist_btwn

    // Try to cover whatever the distance is, and cap off our values.
    val accel = accel_to_cover(delta_dist)

    // TODO its a bit scary that this ever happens? does that mean we're too
    // close..?
    // Make sure we don't deaccelerate past 0 either.
    var accel_to_stop = accel_to_achieve(0)

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

    val needed_accel = accel_to_achieve(desired_speed)

    return needed_accel
  }

  // This directly follows from the distance traveled at constant accel
  private def accel_to_cover(dist: Double) =
    2 * (dist - (a.speed * cfg.dt_s)) / (cfg.dt_s * cfg.dt_s)
  def accel_to_achieve(target_speed: Double) = Util.accel_to_achieve(a.speed, target_speed)
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
final case class Act_Done_With_Route() extends Action
