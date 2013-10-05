// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.meep

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.map.analysis.{AstarRouter, Demand, RouteFeatures}
import utexas.aorta.common.Common

import scala.collection.mutable

// TODO hackish to have an extra weight there.
case class LinearModel(weights: RouteFeatures, scenario_size_weight: Double, constant: Double) {
  def predict(score: RouteFeatures) =
    weights.score(score) + (scenario_size_weight * Common.scenario.agents.size) + constant
}

case class Predictor(time_model: LinearModel, externality_model: LinearModel) {
  def trip_time(score: RouteFeatures) = time_model.predict(score)
  def externality(score: RouteFeatures) = externality_model.predict(score)
}

case class RouteChoice(
  path: List[DirectedRoad], score: RouteFeatures, predicted_time: Double,
  predicted_externality: Double
) {
  override def toString = s"Route Choice[time ~ $predicted_time, cost ~ $predicted_externality]"
}

class ValueOfTime(val time_per_cost: Double) extends AnyVal {
  override def toString = time_per_cost.toString
}

class RouteChooser(graph: Graph, demand: Demand, predictor: Predictor) {
  def discover_routes(start: DirectedRoad, end: DirectedRoad, num_routes: Int): List[RouteChoice] = {
    val scores_seen = new mutable.HashSet[RouteFeatures]()
    val result = new mutable.ListBuffer[RouteChoice]()
    for (i <- 1 to num_routes) {
      var continue = true
      while (continue) {
        val router = new AstarRouter(graph, RouteFeatures.random_weight, demand)
        val scored_path = router.scored_path(start, end)
        val path = scored_path._1
        val score = scored_path._2
        if (!scores_seen.contains(score)) {
          scores_seen += score
          result += RouteChoice(
            path, score, predictor.trip_time(score), predictor.externality(score)
          )
          continue = false
        }
      }
    }
    return result.toList
  }

  def choose_route(choices: List[RouteChoice], vot: ValueOfTime): RouteChoice = {
    val debug = true  // TODO

    // The cheapest route is the baseline
    val baseline = choices.minBy(_.predicted_externality)
    def rating(r: RouteChoice): ValueOfTime = new ValueOfTime(
      (baseline.predicted_time - r.predicted_time) /
      (r.predicted_externality - baseline.predicted_externality)
    )
    val best = choices.maxBy(c => rating(c).time_per_cost)

    if (debug) {
      println(s"baseline is $baseline")
      for (c <- choices) {
        println(s"  $c")
        println(s"  time savings: ${baseline.predicted_time - c.predicted_time}, cost ${c.predicted_externality - baseline.predicted_externality}, value ${rating(c)}")
        println(s"  features... ${c.features}")
      }
      println(s"best is $best, with rating ${rating(best)}")
      println(s"agents VOT is $vot")
    }

    if (rating(best).time_per_cost >= vot.time_per_cost) {
      // Pay the difference
      return best.copy(
        predicted_externality = best.predicted_externality - baseline.predicted_externality
      )
    } else {
      // Freebie
      return baseline.copy(predicted_externality = 0)
    }
  }
}
