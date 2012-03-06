package utexas.sim

import utexas.{Util, cfg, Stats}
import utexas.map.Coordinate

object Headless {
  // cut off some simulations by time
  var run_for: Double = -1.0

  def process_args(args: Array[String]): Simulation = {
    // TODO write with 'partition'
    val keys = args.zipWithIndex.filter(p => p._2 % 2 == 0).map(p => p._1)
    val vals = args.zipWithIndex.filter(p => p._2 % 2 == 1).map(p => p._1)
    var fn = "dat/test.map"
    var rng = System.currentTimeMillis

    if (args.size % 2 != 0) {
      Util.log("Command-line parameters must be pairs of key => value")
    }                                                                     

    for ((key, value) <- keys.zip(vals)) {
      key match {
        case "--input"       => { fn = value }
        case "--rng"         => { rng = value.toLong }
        case "--print_stats" => { Stats.use_print = value == "1" }
        case "--log_stats"   => { Stats.use_log = value == "1" }
        case "--run_for"     => { run_for = value.toDouble }
        case _               => { Util.log("Unknown argument: " + key); sys.exit }
      }
    }
    Util.init_rng(rng)
    return Simulation.load(fn)
  }

  def main(args: Array[String]) = {
    val n = cfg.army_size

    val sim = process_args(args)
    sim.spawn_army(n)
    // This can be optional
    Util.log("Waiting for " + n + " routes to be computed")
    Util.log_push
    val t = Util.timer("Computing routes")
    sim.wait_for_all_generators
    t.stop
    Util.log_pop

    val timer = Util.timer("running the sim")
    Util.log("Starting simulation with time-steps of " + cfg.dt_s + "s")
    var last_real_time = 0.0
    var last_virtual_time = 0.0
    var max_movements = 0
    var total_agent_steps = 0

    def done() = if (run_for == -1.0)
                   sim.done
                 else
                   sim.tick >= run_for

    while (!done) {
      // Print every 1 literal second
      val now = timer.so_far
      //val now = sim.tick    // or every 1 virtual second
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
    sim.shutdown
  }
}
