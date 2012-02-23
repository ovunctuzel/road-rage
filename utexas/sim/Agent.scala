package utexas.sim

import utexas.map.{Edge, Coordinate, Turn, Traversable, Graph}
import utexas.{Util, cfg}

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(val id: Int, val graph: Graph, val start: Edge, val start_dist: Double,
            val route: Route) extends Ordered[Agent]
{
  // just until they're introduced!
  var at: Position = null

  override def compare(other: Agent) = id.compare(other.id)

  // We can only set a target acceleration, which we travel at for the entire
  // duration of timesteps.
  val max_accel = cfg.max_accel   // TODO based on vehicle type
  var speed: Double = 0.0   // meters/sec, I believe
  var target_accel: Double = 0  // m/s^2
  // TODO who chooses this?
  val behavior = new RouteFollowingBehavior(this, route)
  var idle_since = -1.0   // how long has our speed been 0?

  var upcoming_intersections: Set[Intersection] = Set()

  override def toString = "Agent " + id

  // Returns true if we move or do anything at all
  def step(dt_s: Double): Boolean = {
    Agent.a_step_timer.start

    assert(dt_s == cfg.dt_s)

    // To confirm determinism, enable one of these (more precision in doubles is
    // more likely to differ) and diff the logs.
    //Util.log(this + " at " + Agent.sim.tick + " is at " + at)
    //Util.log(this + " at " + "%.1f".format(Agent.sim.tick) + " is at " + at.on + " len " + "%.1f".format(at.dist))

    val start_on = at.on
    val old_dist = at.dist

    // Do physics to update current speed and figure out how far we've traveled in
    // this timestep.
    val new_dist = update_kinematics(dt_s)

    idle_since = if (speed == 0.0 && idle_since == -1.0)
                   Agent.sim.tick   // we've started idling
                 else if (speed == 0.0)
                   idle_since   // keep same
                 else
                   -1.0   // we're not idling

    // Check speed limit
    start_on match {
      case e: Edge => assert(speed <= e.road.speed_limit)
      case _ =>
    }

    // Apply this distance. 
    var current_on = start_on
    var current_dist = old_dist + new_dist

    while (current_dist >= current_on.length) {
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
          val i = Agent.sim.intersections(t.vert)
          i.enter(this, t)
          upcoming_intersections += i
        }
        case (t: Turn, e: Edge) => {
          val i = Agent.sim.intersections(t.vert)
          i.exit(this, t)
          upcoming_intersections -= i
        }
      }

      // this lets behaviors make sure their route is being followed
      behavior.transition(current_on, next)
      current_on = next
    }

    // so we finally end up somewhere...
    if (start_on == current_on) {
      at = move(start_on, current_dist)
    } else {
      exit(start_on)
      at = enter(current_on, current_dist)
    }

    // TODO deal with lane-changing
    Agent.a_step_timer.stop
    return new_dist > 0.0
  }

  def how_long_idle = if (idle_since == -1.0)
                        0.0
                      else
                        Agent.sim.tick - idle_since

  // Returns true if we're done
  def react(): Boolean = {
    Agent.a_react_timer.start

    behavior.choose_action match {
      case Act_Set_Accel(new_accel) => {
        // we have physical limits
        assert(new_accel.abs <= max_accel)
        // make sure this won't put us at a negative speed
        assert(speed + (new_accel * cfg.dt_s) >= 0)

        target_accel = new_accel
        Agent.a_react_timer.stop
        return false
      }
      case Act_Lane_Change(lane) => {
        // TODO ensure it's a valid request
        Util.log("TODO lanechange")
        Agent.a_react_timer.stop
        return false
      }
      case Act_Done_With_Route() => {
        // Trust behavior, don't abuse this.
        assert(speed == 0.0)
        exit(at.on)
        // and don't forget to tell intersections. this is normally just
        // at.on.vert if at.on is a turn, but it could be more due to lookahead.
        upcoming_intersections.foreach(i => i.unregister(this))
        upcoming_intersections = Set()
        Agent.a_react_timer.stop
        return true
      }
    }
  }

  // returns distance traveled, updates speed. note unit of the argument.
  def update_kinematics(dt_sec: Double): Double = {
    // Simply travel at the target constant acceleration for the duration of the
    // timestep.
    val initial_speed = speed
    speed = initial_speed + (target_accel * dt_sec)
    val dist = Util.dist_at_constant_accel(target_accel, dt_sec, initial_speed)

    // It's the behavior's burden to set acceleration so that neither of these
    // cases happen
    assert(speed >= 0.0)
    assert(dist >= 0.0)

    return dist
  }

  // Delegate to the queues and intersections that simulation manages
  def enter(t: Traversable, dist: Double) = Agent.sim.queues(t).enter(this, dist)
  def exit(t: Traversable): Unit = Agent.sim.queues(t).exit(this)
  def move(t: Traversable, dist: Double)  = Agent.sim.queues(t).move(this, dist)

  def dump_info() = {
    Util.log("" + this)
    Util.log_push
    Util.log("At: " + at)
    Util.log("Speed: " + speed)
    Util.log("Next step's acceleration: " + target_accel)
    Util.log("At end of edge? " + at_end_of_edge)
    Util.log("How long idle? " + how_long_idle)
    behavior.dump_info
    Util.log_pop
  }

  def cur_queue = Agent.sim.queues(at.on)

  // stopping time comes from v_f = v_0 + a*t
  // negative accel because we're slowing down.
  def stopping_distance(s: Double = speed) = Util.dist_at_constant_accel(-max_accel, s / max_accel, s)

  // If at.dist == at.on.length, then we'd actually end up on the next
  // traversable. So accept something a few meters back.
  def at_end_of_edge = at.dist_left <= cfg.end_threshold
}

// the singleton just lets us get at the simulation to look up queues
object Agent {
  var sim: Simulation = null

  val q_start_timer = Util.stopwatch
  q_start_timer.stop
  val q_stop_timer = Util.stopwatch
  q_stop_timer.stop
  val a_step_timer = Util.stopwatch
  a_step_timer.stop
  val a_react_timer = Util.stopwatch
  a_react_timer.stop
  val i_check_timer = Util.stopwatch
  i_check_timer.stop
}

////////////////////////////////////////////////////////////////////////////////

// VoidPosition used to exist too, but I couldn't work out when we would ever
// want it. If there's an agent waiting to enter the map, they don't need to
// exist yet.

case class Position(val on: Traversable, val dist: Double) {
  assert(dist >= 0)
  assert(dist <= on.length)
  // TODO
  /*if (dist > on.length) {
    Util.log("safe_spawn_dist must be broken... " + dist + " > " + on.length + " on " + on)
  }*/

  def location = on.location(dist)
  def dist_left = on.length - dist
}
