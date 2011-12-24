package utexas.sim

import utexas.map.{Edge, Coordinate, Turn}
import utexas.Util.{log, log_push, log_pop}

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(id: Int) {
  var at: Position = new VoidPosition()   // start nowhere
  // We can only change speed, and always accelerate as fast as possible to a
  // new speed. This is a major simplifying assumption!
  var speed: Double = 0.0   // units?
  var target_speed: Double = 0.0
  val behavior = new IdleBehavior(this) // TODO

  def spawn_at(e: Edge) = {
    // a sanity check. TODO make it an assert, once we figure out how to test
    // case class.
    at match {
      case LanePosition(_, _) => throw new Exception("spawning must happen from the void!")
      case VoidPosition()     => {}
    }

    at = enter(e, Agent.sim.queues(e).random_spawn)
  }

  def step(dt_ms: Long, tick: Double) = {
    at match {
      case VoidPosition() => {}   // we don't care. (TODO give them a chance to enter map)
      case LanePosition(start_edge, old_dist) => {
        // Do physics to update current speed and figure out how far we've traveled in
        // this timestep.
        val new_dist = update_kinematics(dt_ms / 1000.0)  // ms -> sec

        // Apply this distance. 
        var current_edge = start_edge
        var current_dist = old_dist + new_dist
        while (current_dist > current_edge.length) {
          // TODO don't warp through the vertex; the turn has a line, too
          current_dist -= current_edge.length
          current_edge = behavior.choose_turn(current_edge).to
        }
        // so we finally end up somewhere...
        if (start_edge == current_edge) {
          at = move(current_edge, current_dist)
        } else {
          exit(start_edge)
          at = enter(current_edge, current_dist)
        }
        // TODO deal with lane-changing
        // TODO check for collisions when all of this is happening

        // then let them react
        behavior.choose_action(dt_ms, tick) match {
          case Act_Change_Speed(new_speed) => { target_speed = new_speed }
          case Act_Disappear()             => {}  // TODO signal SImulation
          case Act_Lane_Change(lane)       => {}  // TODO uhh how?
          case Act_Nothing()               => {}
        }
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
  def enter(e: Edge, dist: Double) = Agent.sim.queues(e).enter(this, dist)
  def exit(e: Edge)                = Agent.sim.queues(e).exit(this)
  def move(e: Edge, dist: Double)  = Agent.sim.queues(e).move(this, dist)
}

// the singleton just lets us get at the simulation to look up queues
object Agent {
  var sim: Simulation = null
}

////////////////////////////////////////////////////////////////////////////////

abstract class Position

// They haven't entered the map yet
// TODO or we could have a way of representing driveways
final case class VoidPosition() extends Position {}

final case class LanePosition(e: Edge, dist: Double) extends Position {
  // TODO make them comparable for sorting!

  def location = e.location(dist)
}
