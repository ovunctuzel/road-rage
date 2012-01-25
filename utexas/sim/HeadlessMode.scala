package utexas.sim

import utexas.{Util, cfg}

object Headless {
  def main(args: Array[String]) = {
    Util.init_rng(System.currentTimeMillis)
    val sim = Simulation.load("dat/test.map")
    sim.spawn_army(100)

    val timer = Util.timer("running the sim")
    val dt_ms = (cfg.max_dt * 1000.0 / sim.time_speed).toLong
    Util.log("Starting simulation with time-steps of " + dt_ms + "ms")
    while (sim.agents.size != 0) {
      sim.step(dt_ms)
    }
    Util.log("Simulation took " + sim.tick + " virtual seconds")
    timer.stop
  }
}
