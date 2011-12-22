package utexas.sim

import utexas.map.{Edge, Coordinate}
import utexas.Util.{log, log_push, log_pop}

// Nothing but a huge pile of state.
// TODO except maybe also some permanent characteristics like size, movement
// capabilities
class Agent(g: Graph, id: Int, behavior: Behavior) {
  var at: Position = new VoidPosition()
  var speed: Double = 0.0

  def spawn_at(e: Edge) = {
  }
}

////////////////////////////////////////////////////////////////////////////////

sealed trait Position {}

// They haven't entered the map yet
// TODO or we could have a way of representing driveways
final case class VoidPosition() extends Position {}

final case class LanePosition(e: Edge, dist: Double) extends Position {
  // TODO make them comparable for sorting!

  def location: Coordinate = {
    return new Coordinate(0.0, 0.0)
    // TODO
  }

  // TODO having extra dist left over from a turn?
}

////////////////////////////////////////////////////////////////////////////////

sealed trait Behavior {}

class NullBehavior() extends Behavior {
}

class OldStaticBehavior(g: Graph) extends Behavior {
}
