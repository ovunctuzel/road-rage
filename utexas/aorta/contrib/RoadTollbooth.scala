// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.map.{Road, Turn, Edge}
import utexas.aorta.sim.{Simulation, EV_Transition}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.RoadAgent

// Manage reservations to use a road resource during a window of time
class RoadTollbooth(road: RoadAgent) {
  private var x = 0.0

  private def dx(a: Agent) = (a.wallet.priority.toDouble / 500) * 10

  def enter(a: Agent) {
    x += dx(a)
  }

  def exit(a: Agent) {
    x -= dx(a)
  }

  // TODO evaluate cost curve at x / road.capacity (which is sum of lanes) and squeeze price to [0,
  // 1] normally
  def toll = 42.0
}

class LatestDelay(sim: Simulation) {
  // 0 delay means unknown
  private val latest_delays = new mutable.HashMap[Road, Double]()
  sim.graph.roads.foreach(r => latest_delays(r) = 0)

  private val entry_time = new mutable.HashMap[Agent, Double]()

  sim.listen(classOf[EV_Transition], _ match {
    // Entering a road
    case EV_Transition(a, from: Turn, to) => entry_time(a) = a.sim.tick
    // Exiting a road that we entered normally (not one we spawned into)
    case EV_Transition(a, from: Edge, to: Turn) if entry_time.contains(a) =>
      latest_delays(from.road) = sim.tick - entry_time(a)
    case _ =>
  })

  // TODO get fancier and be per turn
  def delay(road: Road) =
    if (road.lanes.exists(e => !e.queue.isEmpty) && latest_delays(road) != 0)
      latest_delays(road)
    else
      road.freeflow_time
}
