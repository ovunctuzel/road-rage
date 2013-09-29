// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}
import scala.io.Source

import utexas.aorta.map.analysis.RouteFeatures

import utexas.aorta.common.AgentID

object ExternalityPostProc {
  def main(args: Array[String]) {
    // args = [test driver lower inclusive, test driver upper inclusive, files with worlds]
    val test_drivers = Range(args(0).toInt, args(1).toInt + 1).map(new AgentID(_))
    val worlds = args.drop(2).map(fn => read_world(fn))
    new ExternalityPostProc(test_drivers).compute_externality(worlds)
  }

  def read_world(fn: String): Map[AgentID, Experience] = {
    val world = new mutable.HashMap[AgentID, Experience]()
    for (line <- Source.fromFile(fn).getLines) {
      val Array(id, trip_time, raw_features) = line.split(",", 3)
      val features = RouteFeatures.fromList(raw_features.split(",").map(_.toDouble))
      world(new AgentID(id.toInt)) = Experience(trip_time.toDouble, features)
    }
    return world.toMap
  }
}

case class Experience(trip_time: Double, route_score: RouteFeatures)

class ExternalityPostProc(test_drivers: Seq[AgentID]) {
  // The fixed population, not the test drivers
  private val population = Range(0, test_drivers.head.int).map(new AgentID(_))

  private val outfn = "externality-results"
  private val output = new PrintWriter(new FileWriter(new File(outfn)))

  def compute_externality(worlds: Seq[Map[AgentID, Experience]]) {
    val externality = new mutable.HashMap[AgentID, Double]().withDefaultValue(0)
    for (driver <- population) {
      val relevant_worlds = worlds.filter(_.contains(driver))
      if (relevant_worlds.nonEmpty) {
        val best_world = relevant_worlds.minBy(w => w(driver).trip_time)
        val best_time = best_world(driver).trip_time

        for (world <- relevant_worlds) {
          val lost_time = world(driver).trip_time - best_time
          val blame_drivers = world.keys.toSet.diff(best_world.keys.toSet).intersect(test_drivers.toSet)
          for (blame <- blame_drivers) {
            externality(blame) += lost_time / blame_drivers.size
          }
        }
      }
    }

    // Copy the ARFF header
    for (line <- Source.fromFile("misc/route_features.arff").getLines) {
      output.println(line)
    }
    output.println("@ATTRIBUTE externality NUMERIC")
    output.println("")
    output.println("@DATA")

    // For each test driver with externality defined, print out a weka line
    val scenario_size = test_drivers.head.int
    for (test_driver <- test_drivers if externality.contains(test_driver)) {
      // TODO Grab features from which world? Average features? Use longest time (least
      // externality), or shortest time (most externality)? HOPEFULLY arbitrary since it should be
      // similar among all worlds, since few drivers are changing.
      val features = (worlds.find(_.contains(test_driver)).get)(test_driver).route_score
      output.println((features.toList ++ List(scenario_size, externality)).mkString(","))
    }
    output.close()
  }
}
