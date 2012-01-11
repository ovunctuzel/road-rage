package utexas.sim

import utexas.map.{Edge, Coordinate, Turn, Traversable, Graph}
import utexas.{Util, cfg}

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(id: Int, val graph: Graph, start: Edge) {
  var at = enter(start, Agent.sim.queues(start).random_spawn)

  // We can only change speed, and always accelerate as fast as possible to a
  // new speed. This is a major simplifying assumption!
  var speed: Double = 0.0   // units?
  var target_speed: Double = 0
  val behavior = new DangerousBehavior(this) // TODO who chooses this?

  override def toString = "Agent " + id

  // just tell the behavior
  def go_to(e: Edge) = {
    behavior.set_goal(e)
  }

  def step(dt_s: Double): Unit = {
    assert(dt_s <= cfg.max_dt)

    val start_on = at.on
    val old_dist = at.dist

    // Do physics to update current speed and figure out how far we've traveled in
    // this timestep.
    val new_dist = update_kinematics(dt_s)

    // Apply this distance. 
    var current_on = start_on
    var current_dist = old_dist + new_dist
    while (current_dist > current_on.length) {
      current_dist -= current_on.length
      // Are we finishing a turn or starting one?
      val next: Option[Traversable] = current_on match {
        case e: Edge => behavior.choose_turn(e)
        case t: Turn => Some(t.to)
      }
      next match {
        case Some(t) => {
          // this lets behaviors make sure their route is being followed
          behavior.transition(current_on, t)
          current_on = t
        }
        case None => {
          // Done. Disappear!
          exit(start_on)
          return
        }
      }
    }
    // so we finally end up somewhere...
    if (start_on == current_on) {
      at = move(start_on, current_dist)
    } else {
      exit(start_on)
      at = enter(current_on, current_dist)
    }
    // TODO deal with lane-changing
    // TODO check for collisions when all of this is happening
  }

  def react() = {
    behavior.choose_action match {
      case Act_Set_Speed(new_speed) => { target_speed = new_speed }
      case Act_Lane_Change(lane)    => {
        // TODO ensure it's a valid request
        Util.log("TODO lanechange")
      }
    }
  }

  // returns distance traveled, updates speed. note unit of the argument.
  def update_kinematics(dt_sec: Double): Double = {
    val initial_speed = speed
    def dist_at_constant_speed(speed: Double, time: Double) = speed * time
    def dist_at_constant_accel(accel: Double, time: Double) = (initial_speed * time) + (0.5 * accel * (time * time))

    if (target_speed == speed) {
      // No change to speed
      return dist_at_constant_speed(speed, dt_sec)
    } else {
      val accel = if (speed < target_speed)
                    2.7   // TODO cfg and based on vehicle type!
                  else
                    -2.7
      // How long does it take to get to target speed? v_f = v_i + a*t
      val time_until_target = (target_speed - speed) / accel

      if (time_until_target <= dt_sec) {
        // we reach our target speed.
        speed = target_speed
        // we travel a distance during acceleration and then another distance
        // during constant speed phase once we reach our target
        return dist_at_constant_accel(accel, time_until_target)
             + dist_at_constant_speed(speed, dt_sec - time_until_target)
      } else {
        // we don't reach our target speed, just v_f = v_i + a*t
        speed = speed + (accel * dt_sec)
        // and we travel a distance during acceleration phase only
        return dist_at_constant_accel(accel, dt_sec)
      }
    }
  }

  // Delegate to the queues that simulation manages
  def enter(t: Traversable, dist: Double): Position = {
    t match {
      case turn: Turn => Agent.sim.intersections(turn.vert).enter(turn)
      case _ =>
    }
    return Agent.sim.queues(t).enter(this, dist)
  }
  def exit(t: Traversable) = {
    t match {
      case turn: Turn => Agent.sim.intersections(turn.vert).exit(turn)
      case _ =>
    }
    Agent.sim.queues(t).exit(this)
  }
  def move(t: Traversable, dist: Double)  = Agent.sim.queues(t).move(this, dist)

  def dump_info() = {
    Util.log("" + this)
    Util.log_push
    Util.log("At: " + at)
    Util.log("Speed: " + speed + " of target " + target_speed)
    behavior.dump_info
    Util.log_pop
  }

  def cur_queue = Agent.sim.queues(at.on)
}

// the singleton just lets us get at the simulation to look up queues
object Agent {
  var sim: Simulation = null
}

////////////////////////////////////////////////////////////////////////////////

// VoidPosition used to exist too, but I couldn't work out when we would ever
// want it. If there's an agent waiting to enter the map, they don't need to
// exist yet.

case class Position(val on: Traversable, val dist: Double) {
  assert(dist >= 0)
  def location = on.location(dist)
}
