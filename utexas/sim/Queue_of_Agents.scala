package utexas.sim

import scala.collection.mutable.{HashSet => MutableSet} // TODO

import utexas.map.Edge
import utexas.Util.{log, log_push, log_pop, rand_double}

// Although we know the edge we describe, it shouldn't be tough to generalize
// this queue to impose ordering for different agent-containing structures.
class Queue_of_Agents(e: Edge) {
  val agents = new MutableSet[Agent]()  // TODO right data structure...

  def enter(a: Agent, dist: Double): LanePosition = {
    val pos = LanePosition(e, dist)
    // TODO add ourself to the CORRECT spot
    agents += a
    return pos
  }

  def exit(a: Agent) = {
    // TODO hrmm...
    agents -= a
  }

  def move(a: Agent, new_dist: Double): LanePosition = {
    // TODO check for collisions? queue shouldn't change...
    return LanePosition(e, new_dist)
  }

  def random_spawn(): Double = {
    // TODO here's where we have to fix old problems:
    // 1) if we're spawning in front of somebody and starting at 0 km/s...,
    //    could have collisions on THIS edge and on predecessors
    // 2) if we're running right now, what do?

    // but for now... :P
    return rand_double(.20 * e.length, .80 * e.length).toDouble
  }

  // TODO queries: collision checks, nearest to agent/pos
}
