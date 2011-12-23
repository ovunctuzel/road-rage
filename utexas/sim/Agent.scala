package utexas.sim

import utexas.map.{Edge, Coordinate}
import utexas.Util.{log, log_push, log_pop}

// TODO come up with a notion of dimension and movement capability. at first,
// just use radius bounded by lane widths?

class Agent(id: Int, behavior: Behavior) {
  var at: Position = new VoidPosition()   // start nowhere
  var speed: Double = 0.0

  def spawn_at(e: Edge with Queue_of_Agents) = {
    // a sanity check. TODO make it an assert, once we figure out how to test
    // case class.
    at match {
      case LanePosition(_, _) => throw new Exception("spawning must happen from the void!")
      case VoidPosition()     => {}
    }

    at = e.enter(this, e.random_spawn)
  }

  def step(dt_ms: Long, tick: Double) = {
    // TODO
  }
}

////////////////////////////////////////////////////////////////////////////////

abstract class Position

// They haven't entered the map yet
// TODO or we could have a way of representing driveways
final case class VoidPosition() extends Position {}

final case class LanePosition(e: Edge, dist: Double) extends Position {
  // TODO make them comparable for sorting!

  def location = e.location(dist)

  // TODO having extra dist left over from a turn?
}

////////////////////////////////////////////////////////////////////////////////

sealed trait Behavior {}

class IdleBehavior() extends Behavior {
}

class OldStaticBehavior() extends Behavior {
}
