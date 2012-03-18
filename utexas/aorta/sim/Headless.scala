// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.{Util, cfg, Stats}
import utexas.aorta.map.Coordinate

object Headless {
  // cut off some simulations by time
  var run_for: Double = -1.0

  def process_args(args: Array[String]): Simulation = {
    // TODO write with 'partition'
    val keys = args.zipWithIndex.filter(p => p._2 % 2 == 0).map(p => p._1)
    val vals = args.zipWithIndex.filter(p => p._2 % 2 == 1).map(p => p._1)
    var fn = "dat/test.map"
    var rng = System.currentTimeMillis
    var diff_rng = false
    var load_scenario = ""
    var exp_name = "Experiment " + rng

    if (args.size % 2 != 0) {
      Util.log("Command-line parameters must be pairs of key => value")
    }                                                                     

    for ((key, value) <- keys.zip(vals)) {
      key match {
        case "--input"       => { fn = value }
        case "--rng"         => { rng = value.toLong; diff_rng = true }
        case "--print_stats" => { Stats.use_print = value == "1" }
        case "--log_stats"   => { Stats.use_log = value == "1" }
        case "--run_for"     => { run_for = value.toDouble }
        case "--scenario"    => { load_scenario = value }
        case "--name"        => { exp_name = value }
        case _               => { Util.log("Unknown argument: " + key); sys.exit }
      }
    }

    Stats.experiment_name(exp_name)

    if (load_scenario.isEmpty) {
      Util.init_rng(rng)
      val sim = Simulation.load(fn)
      sim.spawn_army(cfg.army_size)
      return sim
    } else {
      val sim = Simulation.load_scenario(load_scenario)
      // It's useful to retry a scenario with a new seed.
      if (diff_rng) {
        Util.init_rng(rng)
      }
      return sim
    }
  }

  def main(args: Array[String]) = {
    val sim = process_args(args)
    // We don't have to wait, but it's better for determinism if we do.
    // TODO hard to get the # of routes now
    Util.log("Waiting for all routes to be computed")
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
