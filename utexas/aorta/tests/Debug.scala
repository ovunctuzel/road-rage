// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation
import utexas.aorta.map.Vertex

import utexas.aorta.{Util, Common}

// Misc, temporary stuff
object Debug {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args, false)

    degenerate_verts(sim)
    doomed_stuff(sim)
    stress_test_pathfind(sim)
  }

  private def degenerate_verts(sim: Simulation) = {
    val silly = sim.vertices.filter(v => v.roads.size == 2)
    silly.foreach(v => println("Degenerate vertex: " + v))
    println("")
    println(silly.size + " total")
  }

  private def doomed_stuff(sim: Simulation) = {
    println(sim.roads.filter(_.doomed).size + " doomed roads")
    println(sim.edges.filter(_.doomed).size + " doomed edges")
  }

  private def stress_test_pathfind(sim: Simulation) = {
    val rng = new utexas.aorta.RNG()
    val t = Common.timer("routing")

    for (i <- 1 until 500000) {
      if (i % 1000 == 0) {
        println(s"round $i")
      }
      val from = rng.choose_rand[Vertex](sim.graph.vertices)
      val to = rng.choose_rand[Vertex](sim.graph.vertices)
      sim.graph.router.path(from, to)
    }
    t.stop
  }
}
