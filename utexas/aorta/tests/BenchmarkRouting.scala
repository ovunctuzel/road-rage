// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.map.analysis._

import utexas.aorta.common.{Util, Common, cfg}

object BenchmarkRouting {
  def main(args: Array[String]) = {
    val rounds = 1000

    val sim = Util.process_args(args)
    val rng = new utexas.aorta.common.RNG()

    val routers = List(
      (new CongestionRouter(sim.graph), "congestion_a*")
    )
    val sum_times = Array.fill[Double](routers.size)(0.0)

    for (i <- 1 until rounds) {
      val from = rng.choose(sim.graph.directed_roads)
      val to = rng.choose(sim.graph.directed_roads)

      if (i % (rounds / 100) == 0) {
        Util.log(s"round $i / $rounds")
      }
      for (((router, name), idx) <- routers.zipWithIndex) {
        val t = Common.timer(name)
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
