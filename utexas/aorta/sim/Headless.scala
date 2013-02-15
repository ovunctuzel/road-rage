// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.{Util, cfg}
import utexas.aorta.analysis.{Stats, Profiling}
import utexas.aorta.map.Coordinate

object Headless {
  // cut off some simulations by time
  var run_for: Double = -1.0

  def main(args: Array[String]) = {
    val sim = Util.process_args(args, true)

    val timer = Profiling.timer("running the sim")
    Util.log("Starting simulation with time-steps of " + cfg.dt_s + "s")
    var last_real_time = 0.0
    var last_virtual_time = 0.0
    var max_movements = 0
    var total_agent_steps = 0

    // TODO move to sim
    def done() = if (run_for == -1.0)
                   sim.done
                 else
                   sim.tick >= run_for

    while (!done) {
      // Print every 1 literal second
      val now = timer.so_far
      if (now - last_real_time >= 1.0) {
        Util.log("At t=%.1f (%.1fs later): %s [%d agents moved] {%s moves/sec}".format(
          sim.tick, sim.tick - last_virtual_time, sim.describe_agents,
          max_movements, Util.comma_num(total_agent_steps)
        ))
        last_real_time = now
        last_virtual_time = sim.tick
        max_movements = 0
        total_agent_steps = 0
      }
      sim.step(cfg.dt_s) match {
        case (moved, total) => {
          max_movements = math.max(max_movements, moved)
          total_agent_steps += total
        }
      }
    }
    Util.log("Simulation took " + sim.tick + " virtual seconds")
    timer.stop
    Util.log("Average of " + (sim.tick / timer.so_far) + "x speedup with dt=" + cfg.dt_s)
  }
}
