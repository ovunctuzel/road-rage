// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.Scenario
import utexas.aorta.map.Graph
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.sim.meep._
import utexas.aorta.common.{AgentID, Util}

import scala.collection.mutable
import java.io.File
import java.util.Scanner

object ClownCarExperiment {
  def main(args: Array[String]) {
    new ClownCarExperiment(ExpConfig.from_args(args)).run()
  }
}

class ClownCarExperiment(config: ExpConfig) extends Experiment(config) {
  // TODO itd be cool to refactor run_trial and output_data by just defining what metrics we want.

  def run() {
    // TODO only hacky due to change not being in the scenario.
    val t1 = run_trial(scenario, "baseline")

    // TODO this isnt the clown-car, single-route way...
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
      )
    )
    val t2 = run_trial(scenario, "clown_car")

    output_data(List(t1, t2), scenario)
  }

  private def run_trial(s: Scenario, mode: String): RawResult = {
    val sim = s.make_sim().setup()
    val times = new TripTimeMetric(sim)
    // TODO other metrics, please!
    simulate(sim)
    return RawResult(mode, Map(
      "times" -> times.result
    ), Map())
  }

  protected def output_data(data: List[RawResult], s: Scenario) {
    output_per_agent("times", data, s)
  }
}
