// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.map.analysis.{AstarRouter, RouteFeatures, Demand}
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, AgentDistribution, MkAgent, MkWallet,
                         MkRoute, Sim_Event, EV_Heartbeat, RouteType, EV_AgentSpawned, PathRoute,
                         RouteRecorder}

import utexas.aorta.common.{RNG, Util, Flags, Common, AgentID}

// TODO refactor an Experiment class.

object RouteAnalyzer {
  // Params
  val scenario_params = Array("--spawn", "5000", "delay=3600", "generations=3", "lifetime=3600")
  val report_locally_every_ms = 10 * 1000
  val report_remotely_every_ms = 60 * 1000
  val deadline = 3600 * 12  // Give up after 12 hours

  // Bit of config
  var gs_prefix = ""
  var report_every_ms = report_locally_every_ms

  // TODO vary more things, like scenario size, or the warmup time
  // TODO maybe fix the map for now, make the learning easier.

  // Optional GS prefix
  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      gs_prefix = args.head
      report_every_ms = report_remotely_every_ms
    }
    val outfn = "route-results"
    val output = new PrintWriter(new FileWriter(new File(outfn)))
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

    // Simulate, capturing every driver's route and trip time
    Flags.set("--savestate", "false")
    val base_sim = scenario.make_sim(graph).setup()
    val times = record_trip_times(base_sim)
    val actual_paths = record_agent_paths(base_sim)
    simulate(0, base_sim)

    // Resimulate, calculating the path score at the moment 
    notify("Round 0 done, precomputing demand on roads/intersections")
    val demand = Demand.demand_for(scenario, graph)

    // Simulate again, scoring the path that the agent is destined to take at the time they spawn
    val sim_again = scenario.make_sim(graph).setup()
    sim_again.listen("route-analyzer", (ev: Sim_Event) => { ev match {
      case EV_AgentSpawned(a) => {
        val score = score_path(actual_paths(a.id).actual_path, demand)
        // Output in weka format
        // input: normalized route features (length, time, congested roads, stop signs, signals,
        // reservations, queued turns, waiting time), scenario size
        // output: driver's time, total externality
        output.println(
          (score.toList ++ List(scenario.agents.size, times(a.id), 0)).mkString(",")
        )
      }
      case _ =>
    } })
    simulate(1, sim_again)
    output.close()

    if (gs_prefix.nonEmpty) {
      Runtime.getRuntime.exec(Array("gsutil", "cp", outfn, gs_prefix + "results"))
    }
  }

  private def score_path(path: List[DirectedRoad], demand: Demand) =
    path
      .map(step => RouteFeatures.for_step(step, demand))
      .fold(RouteFeatures.BLANK)((a, b) => a + b)

  // TODO agent id => trip time
  private def record_trip_times(sim: Simulation): mutable.Map[AgentID, Double] = {
    val times = new mutable.HashMap[AgentID, Double]()
    Common.stats_log = new StatsListener() {
      override def record(item: Measurement) {
        item match {
          // We don't care about anybody who finishes before the new driver is introduced
          case s: Agent_Lifetime_Stat => {
            times(s.id) = s.trip_time
          }
          case _ =>
        }
      }
    }
    return times
  }

  private def record_agent_paths(sim: Simulation): mutable.Map[AgentID, RouteRecorder] = {
    val routes = new mutable.HashMap[AgentID, RouteRecorder]()
    sim.listen("route-analyzer", (ev: Sim_Event) => { ev match {
      case EV_AgentSpawned(a) => {
        routes(a.id) = new RouteRecorder(a.route)
      }
      case _ =>
    } })
    return routes
  }

  private def simulate(round: Int, sim: Simulation) = {
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
      if (sim.tick >= deadline) {
        throw new Exception(s"Simulation past $deadline seconds. Giving up, discarding result.")
      }
    }
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
