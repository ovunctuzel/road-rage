// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.{DirectedRoad, Graph}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.{Sim_Event, EV_AgentSpawned, Agent}
import utexas.aorta.sim.meep.{RouteChooser, Predictor, LinearModel}

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
    // TODO the hackiness! :O
    Graph.route_chooser = new RouteChooser(
      graph, Demand.demand_for(scenario, graph), new Predictor(
        // from the SF model
        time_model = LinearModel(
          RouteFeatures(-0.0439, 1.3905, 46.2986, 8.0701, 36.8984, 4.2773, 4.051, -0.6787, -0.009,
            0.0063, 3.0402), scenario_size_weight = -0.0053, constant = 221.7262
        ),
        // made up!
        externality_model = LinearModel(
          RouteFeatures.BLANK.copy(congested_road_count = 1.0, queued_turn_count = 2.0,
            total_avg_waiting_time = 3.0, road_demand = 1.5, intersection_demand = 4.5,
            agents_enroute = 1.5), scenario_size_weight = 5.0, constant = 0
        )
      ))
    Graph.num_routes = 5

    // TODO multiple rounds and stuff, for one base?
    val mod_sim = scenario.make_sim(graph).setup()
    val mod_times = record_trip_times()
    simulate(1, mod_sim)

    // TODO emit lotsa output. 3 metrics in the paper.

    output.close()
    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", outfn, prefix + "results_" + graph.basename
      ))
      case None =>
    }
  }
}
