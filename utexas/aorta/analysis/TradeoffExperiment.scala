// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.{DirectedRoad, Graph}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.{Sim_Event, EV_AgentSpawned, Agent}

object TradeoffExperiment {
  def main(args: Array[String]) {
    new TradeoffExperiment(ExpConfig.from_args(args)).run()
  }
}

class TradeoffExperiment(config: ExpConfig) extends Experiment(config) {
  protected def outfn = "tradeoff-results"
  protected val output = new PrintWriter(new FileWriter(new File(outfn)))

  def run() {
    // Simulate with normal routes, capturing trip time
    val base_sim = scenario.make_sim(graph).setup()
    val base_times = record_trip_times()
    simulate(0, base_sim)

    // Enable route choices
    Graph.route_choice_experiment = true
    // TODO the hackiness! :O
    Graph.tmp_demand = Demand.demand_for(scenario, graph)
    Graph.tmp_predictor = Predictor(
      time_model = LinearModel(),
      externality_model = LinearModel()
    )

    // TODO multiple rounds and stuff, for one base
    val mod_sim = scenario.make_sim(graph).setup()
    val mod_times = record_trip_times()
    simulate(1, mod_sim)

    // TODO emit lotsa output

    output.close()
    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", outfn, prefix + "results_" + graph.basename
      ))
      case None =>
    }
  }
}
