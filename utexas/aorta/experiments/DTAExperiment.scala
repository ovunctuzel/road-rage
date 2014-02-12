// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.map.{Graph, Road, AbstractPairAstarRouter}
import utexas.aorta.sim.routes.SimpleHeuristic
import utexas.aorta.sim.make.{Scenario, RouterType}
import utexas.aorta.common.{Util, RNG}

import scala.collection.mutable

object DTAExperiment {
  def main(args: Array[String]) {
    new DTAExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

// Dynamic traffic assignment
class DTAExperiment(config: ExpConfig) extends SmartExperiment(config) {
  private val iterations = 5  // TODO put in ExpConfig
  private val rng = new RNG()

  // TODO indicate which agents had routes shifted in each round.
  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new LinkDelayMetric(info)
  )

  override def run() {
    val results = new mutable.ListBuffer[List[Metric]]()

    var current_scenario = scenario
    for (round <- Range(0, iterations)) {
      val metrics = run_trial(current_scenario, s"dta_$round")
      results += metrics
      if (round != iterations - 1) {
        val delay = metrics.last.asInstanceOf[LinkDelayMetric]  // TODO bit of a hack.
        // Min round value is 2
        current_scenario = change_paths(current_scenario, delay, 1.0 / (round + 2))
      }
    }

    output_data(results.toList, scenario)
  }

  // Reroute some drivers using actual delays
  private def change_paths(
    base_scenario: Scenario, delay: LinkDelayMetric, percent: Double
  ): Scenario = {
    val graph = base_scenario.graph
    return base_scenario.copy(agents = base_scenario.agents.map(a => {
      // TODO choose lucky part of population based on budget?
      if (rng.percent(percent)) {
        // Replan!
        // TODO spawn vs start time...
        val new_path = new TimeDependentAStar(graph, delay, a.birth_tick)
          .path(graph.get_r(a.start), graph.get_r(a.route.goal), 0 /* this time doesnt matter */)
          .map(_.id)
        a.copy(route = a.route.copy(orig_router = RouterType.Fixed, initial_path = new_path))
        // TODO make these delays available to all/some drivers, for rerouting? could introduce bad
        // biases towards regions that should be clear but arent, though.
      } else {
        a
      }
    }))
  }
}

class TimeDependentAStar(graph: Graph, delays: LinkDelayMetric, start_time: Double)
  extends AbstractPairAstarRouter(graph) with SimpleHeuristic
{
  override def router_type = RouterType.Unusable

  // cost_sofar._2 is the time spent in the route so far
  override def cost_step(
    prev: Road, next: Road, cost_sofar: (Double, Double)
  ) = (Util.bool2binary(next.auditor.congested), delays.delay(next, start_time + cost_sofar._2))
}
