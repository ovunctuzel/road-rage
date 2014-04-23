// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import scala.collection.mutable

import utexas.aorta.map.{Road, Turn, Edge}
import utexas.aorta.sim.{Simulation, EV_Transition}
import utexas.aorta.sim.intersections.Intersection
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.RoadAgent

class IntersectionTollbooth(intersection: Intersection) {
  // [0, ???]
  private var x = 0.0

  // A priority 1 driver appears like 100 cars, a priority 0.1 looks like 10, a priority .01 looks
  // like 1
  private def dx(a: Agent) = a.wallet.priority * 100

  // enter the road leading up to...
  def enter(a: Agent) {
    x += dx(a)
  }

  // leave the intersection
  def exit(a: Agent) {
    x -= dx(a)
  }

  // Output is ???
  def toll = math.pow(x, 2)
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
