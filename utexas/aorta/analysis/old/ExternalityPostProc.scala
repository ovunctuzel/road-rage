// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}
import scala.io.Source

import utexas.aorta.map.analysis.RouteFeatures

import utexas.aorta.common.{AgentID, Util}

object ExternalityPostProc {
  private var num_worlds = -1

  // TODO Grab features from which world? Average features? Use longest time (least
  // externality), or shortest time (most externality)? HOPEFULLY arbitrary since it should be
  // similar among all worlds, since few drivers are changing.
  private val testers_features = new mutable.HashMap[AgentID, RouteFeatures]()

  def main(args: Array[String]) {
    // args = [test driver lower inclusive, test driver upper exclusive, files with worlds]
    val test_drivers = Range(args(0).toInt, args(1).toInt).map(new AgentID(_))
    Util.log("Parsing results from each world...")
    num_worlds = args.size - 2
    val base_world = read_world(args(2))
    val worlds = args.drop(3).map(fn => read_world(fn))
    Util.log("")
    new ExternalityPostProc(test_drivers, num_worlds).compute_externality(
      base_world, worlds, testers_features.toMap
    )
  }

  private var next_id = -1

  def read_world(fn: String): World = {
    next_id += 1
    print(s"\r  Parsing world ${next_id + 1} / $num_worlds...")
    val results = new mutable.HashMap[AgentID, Double]()
    for (line <- Source.fromFile(fn).getLines) {
      val Array(raw_id, trip_time, raw_features) = line.split(",", 3)
      val id = new AgentID(raw_id.toDouble.toInt)
      if (!testers_features.contains(id)) {
        testers_features(id) = RouteFeatures.fromList(raw_features.split(",").map(_.toDouble))
      }
      results(id) = trip_time.toDouble
    }
    return World(next_id, results.toMap)
  }
}

// double is trip time
case class World(id: Int, results: Map[AgentID, Double])

class ExternalityPostProc(test_drivers: Seq[AgentID], num_worlds: Int) {
  // The fixed population, not the test drivers
  private val population = Range(0, test_drivers.head.int).map(new AgentID(_))

  private val outfn = "externality-data.arff"
  private val output = new PrintWriter(new FileWriter(new File(outfn)))

  def compute_externality(base_world: World, worlds: Seq[World], testers_features: Map[AgentID, RouteFeatures]) {
    // Represents the externality of all test drivers in the first world but not the second
    val externality = Array.fill(num_worlds)(0.0)
    Util.log("Computing externality...")
    var cnt = 0
    for (driver <- population) {
      cnt += 1
      if (cnt % 1000 == 0) {
        print(s"\r  Evaluated impact on $cnt drivers...")
      }
      val relevant_worlds = worlds.filter(_.results.contains(driver))
      if (relevant_worlds.nonEmpty) {
        val base_time = base_world.results(driver)
        for (world <- relevant_worlds) {
          externality(world.id) += world.results(driver) - base_time
        }
      }
    }
    Util.log("\nPrinting results...")

    // Copy the ARFF header
    for (line <- Source.fromFile("misc/route_features.arff").getLines) {
      output.println(line)
    }
    output.println("@ATTRIBUTE externality NUMERIC")
    output.println("")
    output.println("@DATA")

    // For each test driver, print out a weka line
    val scenario_size = test_drivers.head.int
    for (test_driver <- test_drivers) {
      val their_worlds = worlds.filter(_.results.contains(test_driver)).map(_.id)
      if (their_worlds.nonEmpty) {
        // Discount them for the number of worlds they were in
        val their_blame = their_worlds.map(externality(_)).sum / their_worlds.size
        output.println(
          (testers_features(test_driver).toList ++ List(scenario_size, their_blame)).mkString(",")
        )
      }
    }
    output.close()
  }
}
