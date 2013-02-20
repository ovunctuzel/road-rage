// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation

import utexas.aorta.{Util, Common}

// Misc, temporary stuff
// TODO slowly it's stealing from tests.MapSuite! :P
object Debug {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args)

    degenerate_verts(sim)
    doomed_stuff(sim)
    disconnected_directed_roads(sim)
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

  private def disconnected_directed_roads(sim: Simulation) = {
    for (r <- sim.graph.directed_roads) {
      if (r.succs.isEmpty || r.preds.isEmpty) {
        println(s"$r has succs ${r.succs} and preds ${r.preds}. Big road oneway? ${r.road.is_oneway}")
      }
    }
  }

  private def stress_test_pathfind(sim: Simulation) = {
    val rng = new utexas.aorta.RNG()
    // CH (if available), or Dijkstra's
    val router = sim.graph.router
    val t = Common.timer("routing")

    for (i <- 1 until 500000) {
      if (i % 1000 == 0) {
        println(s"round $i")
      }
      val from = rng.choose(sim.graph.directed_roads)
      val to = rng.choose(sim.graph.directed_roads)
      try {
        router.path(from, to)
      } catch {
        case e: Throwable => {
          println(s"Problem on round $i going $from -> $to: $e")
        }
      }
      // TODO verify path
    }
    t.stop
  }
}
