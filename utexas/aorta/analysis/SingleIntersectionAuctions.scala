// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.sim.{Scenario, SystemWalletConfig, AgentDistribution, IntersectionDistribution,
                         IntersectionType, OrderingType}
import utexas.aorta.sim.market.Wallet
import utexas.aorta.map.Graph

object SingleIntersectionAuction {
  def main(args: Array[String]) {
    new SingleIntersectionAuction(ExpConfig.from_args(args)).run()
  }
}

class SingleIntersectionAuction(config: ExpConfig) extends Experiment(config) {
  // Don't use the scenario/etc from Experiment
  val budgets = (100, 200)
  val synthetic = "maps/small_synthetic.map"
  val test_graph = Graph.load(synthetic)

  def run() {
    val outfn = "single-results"
    val output = new PrintWriter(new FileWriter(new File(outfn)))

    // TODO TODO TODO tune demand with size of map, gridlock is no fun.
    for (spawn_per_hour <- List(1000, 5000, 10000, 15000, 25000)) {
      // TODO 1 iter per proc? distribute on GCE here
      for (round <- Range(0, 5)) {
        // Generate scenario
        var s = Scenario(
          name = "single_intersection",
          map_fn = synthetic,
          agents = AgentDistribution.uniform(
            Range(0, spawn_per_hour), test_graph.edges, test_graph.edges, (0, 3600.0),
            Array(AgentDistribution.default_route), Array(AgentDistribution.default_wallet),
            budgets
          ),
          intersections = IntersectionDistribution.uniform_default(test_graph)
            .map(_.copy(policy = IntersectionType.Reservation)),
          system_wallet = SystemWalletConfig()
        )
        for (use_auctions <- List(true, false)) {
          // By default, off
          if (use_auctions) {
            s = s.copy(
              intersections = s.intersections.map(_.copy(ordering = OrderingType.Auction))
            )
          }
          for (use_sysbids <- List(true, false)) {
            // By default, on
            if (!use_sysbids) {
              s = s.copy(system_wallet = SystemWalletConfig.blank)
            }
            for (bid_ahead <- List(true, false)) {
              Wallet.tmp_bid_ahead = bid_ahead
              if (!use_auctions && (bid_ahead || use_sysbids)) {
                // Skip
              } else {
                // Run!
                val sim = s.make_sim(test_graph).setup()
                val times = record_trip_times()
                simulate(round, sim)
                val unweighted_time = times.values.sum
                val weighted_time = s.agents.map(a => a.wallet.budget * times(a.id))

                def bit(bool: Boolean) =
                  if (bool)
                    1
                  else
                    0
                output.println(List(
                  bit(use_auctions), bit(use_sysbids), bit(bid_ahead), spawn_per_hour,
                  unweighted_time, weighted_time
                ).mkString(","))
              }
            }
          }
        }
      }
    }

    output.close()
    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", outfn, prefix + "results"
      ))
      case None =>
    }
  }
}
