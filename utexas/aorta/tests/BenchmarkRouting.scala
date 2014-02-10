// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.map.ZoneRouter
import utexas.aorta.sim.routes.CongestionRouter

import utexas.aorta.common.{Util, RNG, Timer, cfg}

object BenchmarkRouting {
  def main(args: Array[String]) {
    val rounds = 100000

    val sim = Util.process_args(args)
    val rng = new RNG()

    val routers = List(
      (new CongestionRouter(sim.graph), "congestion_a*"),
      (new ZoneRouter(sim.graph), "zone_a*")
    )
    val sum_times = Array.fill[Double](routers.size)(0.0)

    for (i <- 1 until rounds) {
      val from = rng.choose(sim.graph.roads)
      val to = rng.choose(sim.graph.roads)

      if (i % (rounds / 100) == 0) {
        Util.log(f"round $i%,d / $rounds%,d")
      }
      for (((router, name), idx) <- routers.zipWithIndex) {
        val t = Timer(name)
        router.path(from, to, 0.0)
        sum_times(idx) += t.so_far
      }
    }
    Util.log("\n\n")
    for (((_, name), idx) <- routers.zipWithIndex) {
      Util.log(s"$name: ${sum_times(idx)}s total, ${sum_times(idx) / rounds}s per path")
    }
  }
}
