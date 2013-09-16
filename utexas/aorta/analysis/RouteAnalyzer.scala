// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.File

import utexas.aorta.map.Graph
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario}

import utexas.aorta.common.{RNG, Util, Flags}

object RouteAnalyzer {
  // No arguments
  def main(args: Array[String]): Unit = {
    notify("Route analyzer initiated")
    val rng = new RNG()

    // Pick a random map
    // TODO have a file IO lib for stuff like this
    val map_fn = rng.choose(new File("maps").listFiles.map(_.toString).filter(_.endsWith(".map")))

    // Generate a scenario for it
    notify("Generating random scenario")
    val scenario_fn = map_fn.replace("maps/", "scenarios/").replace(".map", "_routes")
    ScenarioTool.main(Array(
      map_fn, "--out", scenario_fn, "--spawn", "5000", "delay=3600", "generations=3", "lifetime=3600"
    ))
    // TODO do the caching in the graph load layer.
    val graph = Graph.load(map_fn)

    // Simulate fully, savestating at 1 hour
    Flags.set("--savestate", "false")
    val base_times = simulate(0, Scenario.load(scenario_fn).make_sim(graph))
    val base_fn = scenario_fn.replace("scenarios/", "scenarios/savestate_") + "_3600"
  }

  // Simulate and return trip time for every agent ID
  private def simulate(round: Int, sim: Simulation): Map[Integer, Double] = {
    var last_time = System.currentTimeMillis
    while (!sim.done) {
      sim.step()
      if (sim.tick == 3600.0 && round == 0) {
        sim.savestate()
      }
      val now = System.currentTimeMillis
      if (now - last_time > 10000) {
        last_time = now
        notify(s"Round $round at ${Util.time_num(sim.tick)}")
      }
    }
    return null  // TODO hijack statsrecorder to get times
  }

  private def notify(status: String) {
    // TODO asynchronously write to GS or so...
    println(s"*** $status ***")
  }
}
