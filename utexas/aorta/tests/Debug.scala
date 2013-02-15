// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation

import com.graphhopper.storage.{LevelGraphStorage, RAMDirectory}
import com.graphhopper.routing.ch.PrepareContractionHierarchies

import utexas.aorta.{Util, Common}

// Misc, temporary stuff
object Debug {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args, false)

    //degenerate_verts(sim)
    //doomed_stuff(sim)
    graphhopper(sim)
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

  private def graphhopper(sim: Simulation) = {
    val graph = new LevelGraphStorage(
      new RAMDirectory(s"maps/route_${sim.graph.name}", true)
    )
    Util.assert_eq(true, graph.loadExisting)
    val algo = new PrepareContractionHierarchies().graph(graph).createAlgo

    val n = sim.graph.directed_roads.size
    val rng = new utexas.aorta.RNG()
    val t = Common.timer("routing")
    for (i <- 1 until 500000) {
      if (i % 1000 == 0) {
        println(s"round $i")
      }
      val from = rng.rand_int(0, n)
      val to = rng.rand_int(0, n)
      algo.calcPath(from, to)
      algo.clear
    }
    t.stop
  }
}
