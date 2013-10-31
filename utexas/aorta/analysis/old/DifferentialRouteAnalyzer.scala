// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

/*package utexas.aorta.analysis

import scala.collection.mutable
import java.io.File

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.map.analysis.{AstarRouter, RouteFeatures, Demand}
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, AgentDistribution, MkAgent, MkWallet,
                         MkRoute, EV_Heartbeat, RouteType, EV_AgentSpawned, EV_Step}

import utexas.aorta.common.{RNG, Util, Flags, Common, AgentID}

object DifferentialRouteAnalyzer {
  def main(args: Array[String]) {
    new DifferentialRouteAnalyzer(ExpConfig.from_args(args)).run()
  }
}

// TODO I haven't kept this code up to date much, since it's been replaced with other forms of
// experiments.
class DifferentialRouteAnalyzer(config: ExpConfig) extends Experiment(config) {
  val new_id = new AgentID(scenario.agents.size)
  val warmup_time = 3600
  val num_routes = 5

  def run() {
    // Append to this, so we only write one file
    var results = ""

    val rng = new RNG()

    // Simulate fully, savestating at 1 hour
    val base_sim = scenario.make_sim(graph).setup()
    val base_times = record_trip_times(base_sim, () => base_sim.tick > warmup_time)
    base_sim.listen("diff-route-analyzer", _ match {
      case EV_Step(tick) if tick == warmup_time => base_sim.savestate()
      case _ =>
    })
    simulate(base_sim)
    val base_fn = scenario.name.replace("scenarios/", "scenarios/savestate_") + "_" + warmup_time

    // Pick a random source and destination for the new driver
    val candidate_edges = AgentDistribution.filter_candidates(graph.edges)
    val start = rng.choose(candidate_edges)
    val end = rng.choose(candidate_edges)

    // Determine the demand once on all roads and intersections
    notify("Round 0 done, precomputing demand on roads/intersections")
    val demand = Demand.demand_for(scenario, graph)

    // Try some routes for the new driver
    val scores_seen = new mutable.HashSet[RouteFeatures]()
    for (round <- 1 to num_routes) {
      notify(s"Discovering new route for round $round")
      var continue = true
      var router: AstarRouter = null
      var result: (List[DirectedRoad], RouteFeatures) = null
      while (continue) {
        router = new AstarRouter(graph, RouteFeatures.random_weight, demand)
        // TODO return a Path that could also compute score, stuff like that?
        result = router.scored_path(start.directed_road, end.directed_road)
        if (!scores_seen.contains(result._2)) {
          scores_seen += result._2
          continue = false
        }
      }

      // Modify the simulation by adding a new driver
      // (No need to modify the scenario or anything)
      notify(s"Creating modified world for round $round")
      val new_sim = Simulation.unserialize(Util.reader(base_fn))
      val route = start.directed_road :: result._1
      new_sim.future_spawn += MkAgent(
        new_id, warmup_time + 5.0, rng.new_seed, start.id, start.safe_spawn_dist(rng),
        new MkRoute(RouteType.Path, route.map(_.id), end.id, rng.new_seed),
        MkWallet(AgentDistribution.default_wallet, 42, 42)  // wallet doesn't matter
      )

      try {
        val new_times = record_trip_times(new_sim, () => new_sim.tick > warmup_time)
        val actual_paths = record_agent_paths(new_sim, (a) => a.id == new_id)
        simulate(new_sim)
        val new_drivers_trip_time = new_times(new_id)
        val externality = calc_externality(base_times, new_times)

        // How much of the route did they follow? Determine the score for the piece of the route
        // they followed, not for the full thing.
        // TODO but this is invalid for the features that need to be evaluated when they first do
        // the routing!
        val score = actual_paths(new_id).actual_path
          .map(step => RouteFeatures.for_step(step, demand))
          .fold(RouteFeatures.BLANK)((a, b) => a + b)
        println(s"Orig score was ${result._2}, but actual is $score")

        // TODO scenario size should be num of agents after the new driver spawns

        // Output in weka format
        // input: normalized route features (length, time, congested roads, stop signs, signals,
        // reservations, queued turns, waiting time), scenario size
        // output: driver's time, total externality
        val weka_line = graph.name + ":" +
          (score.toList ++ List(new_id, new_drivers_trip_time, externality)).mkString(",")
        results += weka_line + "\\n"  // Gotta escape this so exec() doesn't eat it

        notify(s"Round $round done! New driver's time is $new_drivers_trip_time, externality is $externality")
        config.gs_prefix match {
          case Some(prefix) => upload_gs(prefix + "results", results)
          case None => println(s"Would normally upload to GS: $results")
        }
      } catch {
        case e: Throwable => {
          println(s"Problem simulating round $round")
          e.printStackTrace
        }
      }
    }
  }

  private def calc_externality(base: mutable.Map[AgentID, Double], mod: mutable.Map[AgentID, Double]): Double = {
    // The keys (drivers) should be the same, except for the new agent
    Util.assert_eq(base.size, mod.size - 1)
    return base.keys.map(id => mod(id) - base(id)).sum
  }
}*/
