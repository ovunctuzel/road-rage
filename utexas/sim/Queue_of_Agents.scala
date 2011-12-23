package utexas.sim

import scala.collection.mutable.MutableList

import utexas.map.Edge
import utexas.Util.{log, log_push, log_pop, rand_double}

trait Queue_of_Agents extends Edge {
  val agents = new MutableList[Agent]()  // TODO right data structure...

  def enter(a: Agent, dist: Double): LanePosition = {
    val pos = new LanePosition(this, dist)
    // TODO add ourself to the CORRECT spot
    agents += a
    return pos
  }

  // TODO def exit

  def random_spawn(): Double = {
    // TODO here's where we have to fix old problems:
    // 1) if we're spawning in front of somebody and starting at 0 km/s...,
    //    could have collisions on THIS edge and on predecessors
    // 2) if we're running right now, what do?

    // but for now... :P
    return rand_double(.20 * length, .80 * length).toDouble
  }

  // TODO queries: collision checks, nearest to agent/pos
}
