// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashSet => MutableSet}

import utexas.aorta.map.{Edge, Coordinate, Turn, Traversable, Graph, Position}
import utexas.aorta.sim.market._
import utexas.aorta.ui.Renderable
import utexas.aorta.analysis.{Profiling, Stats, Wasted_Time_Stat,
                              Total_Trip_Stat}

import utexas.aorta.{Util, RNG, cfg}

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(val id: Int, val route: Route, val rng: RNG, wallet_spec: MkWallet) extends Ordered[Agent] with Renderable
{
  val wallet = wallet_spec.make(this)

  // null just until they're introduced!
  var at: Position = null

  override def compare(other: Agent) = id.compare(other.id)

  // We can only set a target acceleration, which we travel at for the entire
  // duration of timesteps.
  val max_accel = cfg.max_accel   // TODO based on vehicle type
  // TODO max_deaccel too
  var speed: Double = 0.0   // meters/sec, I believe
  var target_accel: Double = 0  // m/s^2
  // TODO who chooses this?
  val behavior = new LookaheadBehavior(this, route)

  // old_lane is where we're shifting from. we immediately warp into the target
  // lane.
  var old_lane: Option[Edge] = None
  var lanechange_dist_left: Double = 0
  def is_lanechanging = old_lane.isDefined

  // stats stuff
  var idle_since = -1.0   // how long has our speed been 0?
  var entered_last = (-1.0, -1.0, -1.0)  // time, distance, speed
  var started_trip_at = -1.0
  var total_dist = 0.0
  def how_long_idle = if (idle_since == -1.0)
                        0
                      else
                        Agent.sim.tick - idle_since

  // Track intersections where we've asked for and received a lease
  val turns_requested = new MutableSet[Intersection]()
  val turns_approved = new MutableSet[Intersection]()
  
  def involved_with(i: Intersection) =
    turns_requested.contains(i) || turns_approved.contains(i)

  override def toString = "Agent " + id
  override def tooltip = List(toString, wallet.toString)

  // Returns true if we move or do anything at all
  def step(dt_s: Double): Boolean = {
    // Do physics to update current speed and figure out how far we've traveled
    // in this timestep.
    val new_dist = update_kinematics(dt_s)
    total_dist += new_dist

    // If they're not already lane-changing, should they start?
    if (!is_lanechanging && behavior.wants_to_lc) {
      val lane = behavior.target_lane.get
      if (safe_to_lc(lane)) {
        // We have to cover a fixed distance to lane-change. The scaling is kind
        // of arbitrary and just forces lane-changing to not be completely
        // instantaneous at higher speeds.
        lanechange_dist_left = cfg.lanechange_dist
        old_lane = Some(at.on.asInstanceOf[Edge])
        target_accel = 0

        // Immediately enter the target lane
        behavior.transition(at.on, lane)
        at = enter(lane, at.dist)
      }
    }

    // Currently Lane-changing?
    old_lane match {
      case Some(lane) => {
        lanechange_dist_left -= new_dist
        if (lanechange_dist_left <= 0) {
          // Done! Leave the old queue
          exit(lane)

          // Return to normality
          old_lane = None
          lanechange_dist_left = 0
          // We'll only shift lanes once per tick.
        }
      }
      case None =>
    }

    // To confirm determinism, enable and diff the logs. (Grep out
    // timing/profiling stuff)
    /*Util.log(
      f"At ${Agent.sim.tick}%.1f, $this is at $at with speed ${speed}%.1f and accel ${target_accel}%.1f"
    )*/

    if (speed == 0.0 && target_accel == 0.0) {
      if (idle_since == -1.0) {
        idle_since = Agent.sim.tick
      }
      // short-circuit... we're not going anywhere.
      return false
    }

    val start_on = at.on
    val old_dist = at.dist

    idle_since = if (speed == 0.0 && idle_since == -1.0)
                   Agent.sim.tick   // we've started idling
                 else if (speed == 0.0)
                   idle_since   // keep same
                 else
                   -1.0   // we're not idling

    // Check speed limit. Allow a bit of slack.
    Util.assert_le(speed, start_on.speed_limit + cfg.epsilon)

    // Apply this distance. 
    var current_on = start_on
    var current_dist = old_dist + new_dist

    while (current_dist >= current_on.length) {
      if (current_on != start_on && is_lanechanging) {
        throw new Exception(this + " just entered an intersection while lane-changing!")
      }
      current_dist -= current_on.length
      // Are we finishing a turn or starting one?
      val next: Traversable = current_on match {
        case e: Edge => {
          val turn = behavior.choose_turn(e)
          assert(e.next_turns.contains(turn))    // Verify it was a legal choice
          turn
        }
        case t: Turn => t.to
      }

      // tell the intersection
      (current_on, next) match {
        case (e: Edge, t: Turn) => {
          val i = t.vert.intersection
          i.enter(this, t)
        }
        case (t: Turn, e: Edge) => {
          val i = t.vert.intersection
          i.exit(this, t)
          turns_approved -= i
        }
      }

      // this lets behaviors make sure their route is being followed
      behavior.transition(current_on, next)
      current_on = next
    }

    // so we finally end up somewhere...
    if (start_on == current_on) {
      val old_dist = at.dist
      at = move(start_on, current_dist, old_dist)
      // Also stay updated in the other queue
      old_lane match {
        // our distance is changed since we moved above...
        case Some(lane) => move(lane, current_dist, old_dist)
        case None =>
      }
    } else {
      exit(start_on)
      at = enter(current_on, current_dist)
    }

    return new_dist > 0.0
  }

  private def safe_to_lc(target: Edge): Boolean = {
    at.on match {
      case e: Edge => {
        if (e.road != target.road) {
          throw new Exception(this + " wants to lane-change across roads")
        }
        if (math.abs(target.lane_num - e.lane_num) != 1) {
          throw new Exception(this + " wants to skip lanes when lane-changing")
        }                                                               
      }
      case _ => throw new Exception(this + " wants to lane-change from a turn!")
    }

    // One lane could be shorter than the other. When we want to avoid the end
    // of a lane, worry about the shorter one to be safe.
    val min_len = math.min(at.on.length, target.length)

    // Satisfy the physical model, which requires us to finish lane-changing
    // before reaching the intersection.
    if (at.dist + cfg.lanechange_dist + cfg.end_threshold >= min_len) {
      return false
    }

    // Furthermore, we probably have to stop for the intersection, so be sure we
    // have enough room to do that.
    // TODO not also travel dist?
    if (at.dist + stopping_distance(max_next_speed) >= min_len) {
      return false
    }

    // It's impractical to flood backwards and find all the possible cars that
    // could enter our target lane soon. So use the same idea as safe spawning
    // distance and don't start a lane-change too early in the road. This
    // gives agents time next tick to notice us during their lookahead.
    if (!target.queue.closest_behind(at.dist).isDefined &&
        at.dist <= at.on.queue.worst_entry_dist + cfg.follow_dist)
    {
      return false
    }

    // We don't want to merge in too closely to the agent ahead of us, nor do we
    // want to make somebody behind us risk running into us. So just make sure
    // there are no agents in that danger range.
    // TODO expanding the search to twice follow dist is a hack; I'm not sure
    // why agents are winding up about a meter too close sometimes.
    val ahead_dist = (2.0 * cfg.follow_dist) + stopping_distance(max_next_speed)
    // TODO assumes all vehicles the same. not true forever.
    val behind_dist = (2.0 * cfg.follow_dist) + stopping_distance(target.road.speed_limit)
    val nearby = target.queue.all_in_range(at.dist - behind_dist, at.dist + ahead_dist)
    if (!nearby.isEmpty) {
      return false
    }

    return true
  }

  // Returns true if we're done
  def react(): Boolean = {
    val was_lanechanging = is_lanechanging

    return behavior.choose_action match {
      case Act_Set_Accel(new_accel) => {
        // we have physical limits
        Util.assert_le(new_accel.abs, max_accel)
        target_accel = new_accel
        false
      }
      case Act_Done_With_Route() => {
        // TODO untrue when our dest is tiny and we stop right before it!
        Util.assert_eq(at.on.asInstanceOf[Edge].directed_road, route.goal)
        // Trust behavior, don't abuse this.
        Util.assert_eq(speed, 0.0)
        true
      }
    }
  }

  def cancel_intersection_reservations() = {
    turns_requested.foreach(i => i.policy.unregister(this))
    turns_approved.foreach(i => i.policy.unregister(this))
    turns_requested.clear
    turns_approved.clear
  }

  // returns distance traveled, updates speed. note unit of the argument.
  def update_kinematics(dt_sec: Double): Double = {
    // Travel at the target constant acceleration for the duration of the
    // timestep, capping off when speed hits zero.
    val initial_speed = speed
    speed = math.max(0.0, initial_speed + (target_accel * dt_sec))
    val dist = Util.dist_at_constant_accel(target_accel, dt_sec, initial_speed)
    Util.assert_ge(dist, 0.0)
    return dist
  }

  // Delegate to the queues and intersections that simulation manages
  def enter(t: Traversable, dist: Double): Position = {
    // Remember for stats
    entered_last = (Agent.sim.tick, dist, speed)
    t.queue.enter(this, dist)
  }
  def exit(t: Traversable) = {
    // If we were on an edge, how long were we idling about?
    t match {
      case e: Edge => {
        val time_spent = Agent.sim.tick - entered_last._1
        // suppose nobody was in our way because the intersection did a good
        // job, and it also let us go immediately. we should be able to traverse
        // this edge at _about_ its speed limit (accounting for acceleration).
        val optimal_time = two_phase_time(
          speed_i = entered_last._3, speed_f = e.road.speed_limit,
          dist = e.length - entered_last._2, accel = max_accel
        )
        //Util.assert_ge(time_spent, optimal_time)  // TODO we shouldn't have sped
        val wasted_time = time_spent - optimal_time
        // The alternatives for this measurement all suck:
        // - from 1st request till entering (includes legit travel time too)
        // - can_go = no to = yes (could change, still misses following delay)
        // - how_long_idle when can_go becomes = yes (same problem)
        if (wasted_time >= 0) {
          // TODO this number still seems off (it's not 0 even when nobody
          // stops, it's occasionally negative)
          Stats.record(
            Wasted_Time_Stat(id, e.to.id, wasted_time, Agent.sim.tick)
          )
        }
      }
      case _ =>
    }
    t.queue.exit(this, at.dist)
  }
  def move(t: Traversable, new_dist: Double, old_dist: Double) =
    t.queue.move(this, new_dist, old_dist)

  def cur_queue = at.on.queue

  def on(t: Traversable) = (at.on, old_lane) match {
    case (ours, _) if ours == t => true
    case (_, Some(l)) if l == t => true
    case _ => false
  }

  def our_lead = at.on.queue.ahead_of(this)
  def our_tail = at.on.queue.behind(this)

  // math queries for lookahead and such

  // stopping time comes from v_f = v_0 + a*t
  // negative accel because we're slowing down.
  def stopping_distance(s: Double = speed) = Util.dist_at_constant_accel(
    -max_accel, s / max_accel, s
  )
  // We'll be constrained by the current edge's speed limit and maybe other
  // stuff, but at least this speed lim.
  def max_next_accel = math.min(max_accel, (at.on.speed_limit - speed) / cfg.dt_s)

  def max_next_speed = speed + (max_next_accel * cfg.dt_s)
  def max_next_dist = Util.dist_at_constant_accel(max_next_accel, cfg.dt_s, speed)
  def min_next_dist = Util.dist_at_constant_accel(-max_accel, cfg.dt_s, speed)
  def min_next_speed = math.max(0.0, speed + (cfg.dt_s * -max_accel))
  def min_next_dist_plus_stopping =
    min_next_dist + stopping_distance(min_next_speed)
  def max_next_dist_plus_stopping =
    max_next_dist + stopping_distance(max_next_speed)
  def max_lookahead_dist = max_next_dist_plus_stopping

  def accel_to_achieve(target_speed: Double) = (target_speed - speed) / cfg.dt_s
  // d = (v_i)(t) + (1/2)(a)(t^2), solved for a
  def accel_to_cover(dist: Double) = (2 * (dist - (speed * cfg.dt_s)) /
                                      (cfg.dt_s * cfg.dt_s))

  // To stop in one time-step, that is. From v_f = v_i + at
  def accel_to_stop = (-1 * speed) / cfg.dt_s

  def debug = {
    Util.log("" + this)
    Util.log_push
    Util.log("At: " + at)
    Util.log("Speed: " + speed)
    Util.log("How long idle? " + how_long_idle)
    Util.log("Max next speed: " + max_next_speed)
    Util.log("Stopping distance next: " + stopping_distance(max_next_speed))
    Util.log("Lookahead dist: " + max_lookahead_dist)
    Util.log("Dist left here: " + at.dist_left)
    behavior.dump_info
    Util.log_pop
  }

  def approve_turn(i: Intersection) = {
    synchronized {
      Util.assert_eq(turns_requested(i), true)
      turns_requested -= i
      turns_approved += i
    }
  }

  def how_far_away(i: Intersection) = behavior.how_far_away(i)

  // Caller must remove this agent from the simulation list
  def terminate() = {
    exit(at.on)
    // don't forget to tell intersections. this is normally just
    // at.on.vert if at.on is a turn, but it could be more due to lookahead.
    cancel_intersection_reservations
    Stats.record(Total_Trip_Stat(id, Agent.sim.tick - started_trip_at, total_dist))
  }

  // find the time to cover dist by accelerating first, then cruising at
  // constant speed
  def two_phase_time(speed_i: Double = 0, speed_f: Double = 0, dist: Double = 0,
                     accel: Double = 0): Double =
  {
    // v_f = v_i + a*t
    val time_to_cruise = (speed_f - speed_i) / accel
    val dist_during_accel = Util.dist_at_constant_accel(
      accel, time_to_cruise, speed_i
    )

    return if (dist_during_accel < dist) {
      // so then the remainder happens at constant cruisin speed
      return time_to_cruise + ((dist - dist_during_accel) / speed_f)
    } else {
      // We spent the whole time accelerating
      // solve dist = a(t^2) + (v_i)t
      val discrim = math.sqrt((speed_i * speed_i) + (4 * accel * dist))
      val time = (-speed_i + discrim) / (2 * accel) // this is the positive root
      Util.assert_ge(time, 0)   // make sure we have the right solution to this
      time
    }
  }
}

class SpawnAgent(val a: Agent, val e: Edge, val dist: Double) {}

// the singleton just lets us get at the simulation to look up queues
object Agent {
  var sim: Simulation = null
}
