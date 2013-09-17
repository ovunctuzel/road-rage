// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.File

import utexas.aorta.map.Graph
import utexas.aorta.map.analysis.{AstarRouter, RouteFeatures}
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, AgentDistribution, MkAgent, MkWallet,
                         MkSpecificPathRoute, Sim_Event, EV_Heartbeat}

import utexas.aorta.common.{RNG, Util, Flags, Common}

object RouteAnalyzer {
  // Params
  val scenario_params = Array("--spawn", "5000", "delay=3600", "generations=3", "lifetime=3600")
  val new_id = 15000  // This has to be correct based on scenario_params
  val warmup_time = 3600
  val report_every_ms = 10 * 1000
  val num_routes = 5

  // TODO vary more things, like scenario size.
  // TODO maybe fix the map for now, make the learning easier.

  // No arguments
  def main(args: Array[String]): Unit = {
    val rng = new RNG()

    // Pick a random map
    // TODO have a file IO lib for stuff like this
    val map_fn = rng.choose(new File("maps").listFiles.map(_.toString).filter(_.endsWith(".map")))

    // Generate a scenario for it
    notify("Generating random scenario")
    val scenario_fn = map_fn.replace("maps/", "scenarios/").replace(".map", "_routes")
    ScenarioTool.main(Array(map_fn, "--out", scenario_fn) ++ scenario_params)
    // TODO do the caching in the graph load layer.
    val graph = Graph.load(map_fn)

    // Simulate fully, savestating at 1 hour
    Flags.set("--savestate", "false")
    val base_times = simulate(0, Scenario.load(scenario_fn).make_sim(graph).setup)
    val base_fn = scenario_fn.replace("scenarios/", "scenarios/savestate_") + "_" + warmup_time

    // Pick a random source and destination for the new driver
    val candidate_edges = AgentDistribution.filter_candidates(graph.edges)
    val start = rng.choose(candidate_edges)
    val end = rng.choose(candidate_edges)

    // Try some routes
    for (round <- 1 to num_routes) {
      notify(s"Creating modified world for round $round")
      val router = new AstarRouter(graph, RouteFeatures.random_weight)
      val new_sim = Simulation.unserialize(Util.reader(base_fn))
      // Modify the simulation by adding a new driver
      // (No need to modify the scenario or anything)
      val route = router.path(start.directed_road, end.directed_road, warmup_time.toDouble)
      new_sim.future_spawn += MkAgent(
        new_id, warmup_time + 5.0, rng.new_seed, start.id, start.safe_spawn_dist(rng),
        new MkSpecificPathRoute(route.map(_.id), rng.new_seed),
        MkWallet(AgentDistribution.default_wallet, 42, 42)  // wallet doesn't matter
      )

      val new_times = simulate(round, new_sim)
      val new_drivers_trip_time = new_times(new_id)
      val externality = calc_externality(base_times, new_times)
    }
  }

  // Simulate and return trip time for every agent ID
  private def simulate(round: Int, sim: Simulation): Map[Int, Double] = {
    val times = new mutable.HashMap[Int, Double]()
    Common.stats_log = new StatsListener() {
      override def record(item: Measurement) {
        item match {
          // We don't care about anybody who finishes before the new driver is introduced
          case s: Agent_Lifetime_Stat if sim.tick > warmup_time => {
            times(s.id) = s.trip_time
          }
          case _ =>
        }
      }
    }

    var last_time = 0L
    sim.listen("route-analyzer", (ev: Sim_Event) => { ev match {
      case EV_Heartbeat(info) => {
        val now = System.currentTimeMillis
        if (now - last_time > report_every_ms) {
          last_time = now
          notify(s"Round $round at ${Util.time_num(sim.tick)}: ${info.describe}" +
                 s" / ${sim.finished_count} finished")
        }
      }
      case _ =>
    } })

    while (!sim.done) {
      sim.step()
      if (sim.tick.toInt == warmup_time && round == 0) {
        sim.savestate()
      }
    }
    return times.toMap
  }

  private def calc_externality(base: Map[Int, Double], mod: Map[Int, Double]): Double = {
    // The keys (drivers) should be the same, except for the new agent
    Util.assert_eq(base.size, mod.size - 1)
    return base.keys.map(id => mod(id) - base(id)).sum
  }

  private def notify(status: String) {
    // TODO asynchronously write to GS or so...
    println(s"*** $status ***")
  }
}
