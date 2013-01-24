// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation

import utexas.aorta.Util

// Misc, temporary stuff
object Debug {
  def main(args: Array[String]) = {
    val with_geo = false
    val (sim, is_scenario) = Util.process_args(args, with_geo, false)

    degenerate_verts(sim)
  }

  private def degenerate_verts(sim: Simulation) = {
    val silly = sim.vertices.filter(v => v.roads.size == 2)
    silly.foreach(v => println("Degenerate vertex: " + v))
    println("")
    println(silly.size + " total")
  }
}
