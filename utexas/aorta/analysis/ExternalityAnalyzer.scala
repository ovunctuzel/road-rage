// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}
import scala.util.Random

import utexas.aorta.sim.{Agent, Scenario, ScenarioTool}
import utexas.aorta.map.analysis.{RouteFeatures, Demand}
import utexas.aorta.common.Util

object ExternalityAnalyzer {
  def main(args: Array[String]) {
    new ExternalityAnalyzer(args(0).toInt, args(1).toInt, ExpConfig.from_args(args.drop(2))).run()
  }
}

class ExternalityAnalyzer(population_size: Int, test_size: Int, config: ExpConfig)
  extends RouteAnalyzer(config)
{
  val base_scenario_fn = "scenarios/base_externality"

  override def outfn = "externality-results"

  override def get_scenario(): Scenario = {
    if (!(new File(base_scenario_fn).exists)) {
      notify("Generating scenario")
      // TODO this ignores the exp config
      ScenarioTool.main(Array(
        "maps/austin.map", "--out", base_scenario_fn, "--spawn", (population_size / 3).toString,
        "delay=3600", "lifetime=3600", "generations=3", "--spawn", test_size.toString, "delay=10800"
      ))
    }

    val base = Scenario.load(base_scenario_fn)
    val pair = base.agents.splitAt(population_size)
    val population = pair._1
    val test_drivers = pair._2
    // Choose a random half of the test population
    val chosen_testers = Random.shuffle(test_drivers.toList).take(test_size / 2)
    Util.log(s"Randomly chose the following test drivers: ${chosen_testers.map(_.id).sortBy(_.int)}")
    // TODO instrument so base scenario can run, 0 chosen_testers
    return base.copy(agents = population ++ chosen_testers)
  }

  override def output_score(a: Agent, score: RouteFeatures, trip_time: Double) {
    output.println((List(a.id.int, trip_time) ++ score.toList).mkString(","))
  }
}
