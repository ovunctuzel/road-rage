// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.map.analysis._

import utexas.aorta.common.{Util, Common, cfg}

object BenchmarkRouting {
  def main(args: Array[String]) = {
    val rounds = 500

    val sim = Util.process_args(args)
    val rng = new utexas.aorta.common.RNG()

    val ch_router = new CHRouter(sim.graph)
    val congestion_router = new CongestionRouter(sim.graph)
    val strand_router = new CompressedGraph(sim.graph)
    val sum = Array.fill[Double](3)(0.0)

    for (i <- 1 until rounds) {
      val from = rng.choose(sim.graph.directed_roads)
      val to = rng.choose(sim.graph.directed_roads)

      if (i % (rounds / 100) == 0) {
        println(s"round $i / $rounds")
      }
      for (((router, name), idx) <- List((ch_router, "ch"), (congestion_router, "cong"), (strand_router, "strand")).zipWithIndex) {
        val t = Common.timer(name)
        router.path(from, to, 0.0)
        sum(idx) += t.so_far
      }
    }
    println(s"\n\nCH: ${sum(0) / rounds}, cong: ${sum(1) / rounds}, strand: ${sum(2) / rounds}")
    println(s"total for $rounds... CH: ${sum(0)}, cong: ${sum(1)}, strand: ${sum(2)}")
  }
}
