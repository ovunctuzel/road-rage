// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, AgentDistribution, MkAgent, MkWallet,
                         MkRoute, Sim_Event, EV_Heartbeat, RouteType, EV_AgentSpawned}

import utexas.aorta.common.{Util, Flags, Common, AgentID}

object RouteAnalyzer {
  def main(args: Array[String]) {
    new RouteAnalyzer(ExpConfig.from_args(args)).run()
  }
}

class RouteAnalyzer(config: ExpConfig) extends Experiment(config) {
  def run() {
    val outfn = "route-results"
    val output = new PrintWriter(new FileWriter(new File(outfn)))

    // Simulate, capturing every driver's route and trip time
    val base_sim = scenario.make_sim(graph).setup()
    val times = record_trip_times()
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

    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array("gsutil", "cp", outfn, prefix + "results"))
      case None =>
    }
  }

  private def score_path(path: List[DirectedRoad], demand: Demand) =
    path
      .map(step => RouteFeatures.for_step(step, demand))
      .fold(RouteFeatures.BLANK)((a, b) => a + b)
}
