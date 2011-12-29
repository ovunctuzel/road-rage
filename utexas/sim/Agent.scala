package utexas.sim

import utexas.map.{Edge, Coordinate, Turn, Traversable, Graph}
import utexas.Util

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(id: Int, val graph: Graph) {
  var at: Position = new VoidPosition()   // start nowhere
  // We can only change speed, and always accelerate as fast as possible to a
  // new speed. This is a major simplifying assumption!
  var speed: Double = 0.0   // units?
  var target_speed: Double = 0
  val behavior = new DangerousBehavior(this) // TODO who chooses this?

  def spawn_at(e: Edge) = {
    // a sanity check. TODO make it an assert, once we figure out how to test
    // case class.
    at match {
      case LanePosition(_, _) => throw new Exception("spawning must happen from the void!")
      case VoidPosition()     => {}
    }

    at = enter(e, Agent.sim.queues(e).random_spawn)
  }

  // just tell the behavior
  def go_to(e: Edge) = {
    behavior.set_goal(e)
  }

  def step(dt_ms: Long, tick: Double) = {
    at match {
      case VoidPosition() => {}   // we don't care. (TODO give them a chance to enter map)
      case LanePosition(start_on, old_dist) => {
        // Do physics to update current speed and figure out how far we've traveled in
        // this timestep.
        val new_dist = update_kinematics(dt_ms / 1000.0)  // ms -> sec

        // Apply this distance. 
        var current_on = start_on
        var current_dist = old_dist + new_dist
        while (current_dist > current_on.length) {
          // TODO don't warp through the vertex; the turn has a line, too
          current_dist -= current_on.length
          // Are we finishing a turn or starting one?
          current_on = current_on match {
            case e: Edge => behavior.choose_turn(e)
            case t: Turn => t.to
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

        // then let them react
        behavior.choose_action(dt_ms, tick) match {
          case Act_Set_Speed(new_speed) => { target_speed = new_speed }
          case Act_Disappear()          => { Util.log("TODO disappear") }  // TODO signal SImulation
          case Act_Lane_Change(lane)    => { Util.log("TODO lanechange") }  // TODO uhh how?
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
  def enter(t: Traversable, dist: Double) = t match {
    case e: Edge => Agent.sim.queues(e).enter(this, dist)
    case _: Turn => LanePosition(t, dist)
  }
  def exit(t: Traversable) = t match {
    case e: Edge => Agent.sim.queues(e).exit(this)
    case _       =>
  }
  def move(t: Traversable, dist: Double) = t match {
    case e: Edge => Agent.sim.queues(e).move(this, dist)
    case _: Turn => LanePosition(t, dist)
  }
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

// TODO a misnomer now?
final case class LanePosition(on: Traversable, dist: Double) extends Position {
  // TODO make them comparable for sorting!

  def location = on.location(dist)
}
