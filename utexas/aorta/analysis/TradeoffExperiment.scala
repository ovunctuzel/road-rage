// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}
import scala.util.Random

import utexas.aorta.map.{DirectedRoad, Graph}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.{Sim_Event, EV_AgentSpawned, Agent}
import utexas.aorta.sim.meep.{RouteChooser, Predictor, LinearModel, AgentAdaptor}
import utexas.aorta.common.Util

object TradeoffExperiment {
  def main(args: Array[String]) {
    new TradeoffExperiment(ExpConfig.from_args(args)).run()
  }
}

class TradeoffExperiment(config: ExpConfig) extends Experiment(config) {
  protected def outfn = "tradeoff-results"
  protected val output = new PrintWriter(new FileWriter(new File(outfn)))

  val num_special = 3000

  def run() {
    // Simulate with normal routes, capturing trip time
    val base_sim = scenario.make_sim(graph).setup()
    val base_times = record_trip_times()
    simulate(0, base_sim)

    // Enable route choices for a random subset of the population
    // (Doing it for everyone makes running simulations way too slow)
    AgentAdaptor.special_routes =
      Random.shuffle(scenario.agents.map(_.id).toList).take(num_special).toSet
    Util.log(s"Special agents: ${AgentAdaptor.special_routes}")
    //AgentAdaptor.special_routes = scenario.agents.map(_.id).toSet

    // TODO the hackiness! :O
    notify("Precomputing demand...")
    Graph.route_chooser = new RouteChooser(
      graph, Demand.demand_for(scenario, graph), new Predictor(
        // from the SF model
        time_model = LinearModel(
          RouteFeatures(-0.0439, 1.3905, 46.2986, 8.0701, 36.8984, 4.2773, 4.051, -0.6787, -0.009,
            0.0063, 3.0402), scenario_size_weight = -0.0053, constant = 221.7262
        ),
        // made up!
        externality_model = LinearModel(
          RouteFeatures.BLANK.copy(congested_road_count = 0.008, queued_turn_count = 0.005,
            total_avg_waiting_time = 0.009, road_demand = 0.004, intersection_demand = 0.01,
            agents_enroute = 0.007), scenario_size_weight = 0.003, constant = 0
        )
      ))
    
    // Try different numbers of route choices? For now, just one
    for (num_routes <- List(5)) {
      Graph.num_routes = num_routes
      AgentAdaptor.reset()
      val mod_sim = scenario.make_sim(graph).setup()
      val mod_times = record_trip_times()
      simulate(1, mod_sim)

      val saved_time_special = AgentAdaptor.special_routes.map(id => mod_times(id) - base_times(id)).sum
      val saved_time_rest = scenario.agents
        .filter(a => !AgentAdaptor.special_routes.contains(a.id))
        .map(a => mod_times(a.id) - base_times(a.id))
        .sum
      val saved_time_total = saved_time_special + saved_time_rest
      val aggregate_vot_special = saved_time_special / AgentAdaptor.max_paid.values.sum
      val aggregate_vot_total = saved_time_total / AgentAdaptor.max_paid.values.sum
      val gain_vot = AgentAdaptor.paying_drivers.map(
        id => ((mod_times(id) - base_times(id)) / AgentAdaptor.max_paid(id)) - scenario.agents(id.int).wallet.priority
      ).sum
      output.println(List(
        num_routes, scenario.agents.size, num_special, saved_time_special, saved_time_rest,
        saved_time_total, aggregate_vot_special, aggregate_vot_total, gain_vot
      ).mkString(","))

      if (false) {
        // TODO perhaps record, in some form?
        println("Individual experiences...")
        for (a <- scenario.agents) {
          println(s"  ${a.id}: base ${base_times(a.id)}, mod ${mod_times(a.id)}, paid ${AgentAdaptor.max_paid(a.id)}, VOT ${a.wallet.priority}")
        }
      }
    }

    output.close()
    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", outfn, prefix + "results_" + graph.basename
      ))
      case None =>
    }
  }
}
