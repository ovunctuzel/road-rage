// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

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
