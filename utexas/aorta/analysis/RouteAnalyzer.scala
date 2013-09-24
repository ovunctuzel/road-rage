// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.File

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.map.analysis.{AstarRouter, RouteFeatures, Demand}
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, AgentDistribution, MkAgent, MkWallet,
                         MkRoute, Sim_Event, EV_Heartbeat, RouteType, EV_AgentDone, PathRoute}

import utexas.aorta.common.{RNG, Util, Flags, Common}

object RouteAnalyzer {
  // Params
  val scenario_params = Array("--spawn", "5000", "delay=3600", "generations=3", "lifetime=3600")
  val new_id = 15000  // This has to be correct based on scenario_params
  val warmup_time = 3600
  val report_locally_every_ms = 10 * 1000
  val report_remotely_every_ms = 60 * 1000
  val num_routes = 5
  val csv_out_fn = "route_data"
  val deadline = 3600 * 12  // Give up after 12 hours

  // Bit of config
  var gs_prefix = ""
  var report_every_ms = report_locally_every_ms

  // TODO vary more things, like scenario size, or the warmup time
  // TODO maybe fix the map for now, make the learning easier.

  // No arguments
  def main(args: Array[String]): Unit = {
    // Append to this, so we only write one file
    var results = ""

    if (args.nonEmpty) {
      gs_prefix = args.head
      report_every_ms = report_remotely_every_ms
    }
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
    val scenario = Scenario.load(scenario_fn)

    // Simulate fully, savestating at 1 hour
    Flags.set("--savestate", "false")
    val base_times = simulate(0, scenario.make_sim(graph).setup)._1
    val base_fn = scenario_fn.replace("scenarios/", "scenarios/savestate_") + "_" + warmup_time

    // Pick a random source and destination for the new driver
    val candidate_edges = AgentDistribution.filter_candidates(graph.edges)
    val start = rng.choose(candidate_edges)
    val end = rng.choose(candidate_edges)

    // Determine the demand once on all roads and intersections
    notify("Round 0 done, precomputing demand on roads/intersections")
    val demand = Demand.demand_for(scenario, graph)

    // Try some routes
    val scores_seen = new mutable.HashSet[RouteFeatures]()
    for (round <- 1 to num_routes) {
      notify(s"Discovering new route for round $round")
      var continue = true
      var router: AstarRouter = null
      var result: (List[DirectedRoad], RouteFeatures) = null
      while (continue) {
        router = new AstarRouter(graph, RouteFeatures.random_weight, demand)
        result = router.scored_path(start.directed_road, end.directed_road)
        if (!scores_seen.contains(result._2)) {
          scores_seen += result._2
          continue = false
        }
      }

      // Modify the simulation by adding a new driver
      // (No need to modify the scenario or anything)
      notify(s"Creating modified world for round $round")
      val new_sim = Simulation.unserialize(Util.reader(base_fn))
      val route = start.directed_road :: result._1
      new_sim.future_spawn += MkAgent(
        new_id, warmup_time + 5.0, rng.new_seed, start.id, start.safe_spawn_dist(rng),
        new MkRoute(RouteType.Path, route.map(_.id), end.id, rng.new_seed),
        MkWallet(AgentDistribution.default_wallet, 42, 42)  // wallet doesn't matter
      )

      try {
        val sim_results = simulate(round, new_sim)
        val new_times = sim_results._1
        val new_drivers_trip_time = new_times(new_id)
        val externality = calc_externality(base_times, new_times)

        // How much of the route did they follow? Determine the score for the piece of the route
        // they followed, not for the full thing.
        val score: RouteFeatures = route
          .takeWhile(step => step != sim_results._2.headOption.getOrElse(null))
          .map(step => RouteFeatures.for_step(step, demand))
          .fold(RouteFeatures.BLANK)((a, b) => a + b)
        println(s"Orig score was ${result._2}, but actual is $score")
        println(s"New driver had ${sim_results._2.size} steps left of ${route.size}")

        // TODO scenario size should be num of agents after the new driver spawns

        // Output in weka format
        // input: normalized route features (length, time, congested roads, stop signs, signals,
        // reservations, queued turns, waiting time), scenario size
        // output: driver's time, total externality
        val weka_line = graph.name + ":" +
          (score.toList ++ List(new_id, new_drivers_trip_time, externality)).mkString(",")
        results += weka_line + "\\n"  // Gotta escape this so exec() doesn't eat it

        notify(s"Round $round done! New driver's time is $new_drivers_trip_time, externality is $externality")
        if (gs_prefix.nonEmpty) {
          upload_gs(gs_prefix + "results", results)
        } else {
          println(s"Would normally upload to GS: $results")
        }
      } catch {
        case e: Throwable => {
          println(s"Problem simulating round $round")
          e.printStackTrace
        }
      }
    }
  }

  // Simulate and return trip time for every agent ID
  private def simulate(round: Int, sim: Simulation): (Map[Int, Double], List[DirectedRoad]) = {
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
    // of the new driver
    var remaining_route: List[DirectedRoad] = Nil

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
      case EV_AgentDone(a) if a.id == new_id => {
        // The first step of the remaining route is where they end
        remaining_route = a.route.asInstanceOf[PathRoute].path.tail
      }
      case _ =>
    } })

    while (!sim.done) {
      sim.step()
      if (sim.tick.toInt == warmup_time && round == 0) {
        sim.savestate()
      }
      if (sim.tick >= deadline) {
        throw new Exception(s"Simulation past $deadline seconds. Giving up, discarding result.")
      }
    }
    return (times.toMap, remaining_route)
  }

  private def calc_externality(base: Map[Int, Double], mod: Map[Int, Double]): Double = {
    // The keys (drivers) should be the same, except for the new agent
    Util.assert_eq(base.size, mod.size - 1)
    return base.keys.map(id => mod(id) - base(id)).sum
  }

  private def notify(status: String) {
    if (gs_prefix.nonEmpty) {
      // TODO write the instance name here
      upload_gs(gs_prefix + "status", status)
    } else {
      println(s"*** $status ***")
    }
  }

  private def upload_gs(fn: String, contents: String) {
    Runtime.getRuntime.exec(Array("./tools/cloud/upload_gs.sh", fn, contents))
  }
}
