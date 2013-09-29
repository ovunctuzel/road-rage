// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}
import scala.util.Random

import utexas.aorta.sim.{Agent, Scenario}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.common.Util

object ExternalityAnalyzer {
  def main(args: Array[String]) {
    new ExternalityAnalyzer(args(0), args(1).toInt, ExpConfig.from_args(args.drop(2))).run()
  }
}

class ExternalityAnalyzer(scenario_fn: String, population_size: Int, config: ExpConfig)
  extends RouteAnalyzer(config)
{
  override def get_scenario(): Scenario = {
    val base = Scenario.load(scenario_fn)
    val pair = base.agents.splitAt(population_size)
    val population = pair._1
    val test_drivers = pair._2
    Util.assert_eq(population_size, population.size)
    // Choose a random half of the test population
    val chosen_testers = Random.shuffle(test_drivers.toList).take(population_size / 2)
    return base.copy(agents = population ++ chosen_testers)
  }

  override def output_score(a: Agent, score: RouteFeatures, trip_time: Double) {
    output.println((List(a.id.int, trip_time) ++ score.toList).mkString(","))
  }
}
