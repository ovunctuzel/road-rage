// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.{Util, cfg}
import utexas.aorta.map.Coordinate

object Headless {
  // cut off some simulations by time
  var run_for: Double = -1.0

  def main(args: Array[String]) = {
    val sim = Util.process_args(args)

    // Print an update every second
    var last_tick = sim.tick
    sim.listen((ev: Sim_Event) => { ev match {
      case EV_Heartbeat(info) => {
        Util.log("At t=%.1f (%.1fs later): %s [%d agents moved] {%s moves/sec}".format(
          info.tick, info.tick - last_tick, sim.describe_agents,
          info.active_agents, Util.comma_num(info.agent_steps)
        ))
        last_tick = info.tick
      }
    } })

    // TODO move to sim
    def done() = if (run_for == -1.0)
                   sim.done
                 else
                   sim.tick >= run_for

    Util.log("Starting simulation with time-steps of " + cfg.dt_s + "s")
    while (!done) {
      sim.step(cfg.dt_s)
    }
  }
}
