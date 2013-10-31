// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Scenario, SystemWalletConfig, OrderingType, WalletType, IntersectionType}
import utexas.aorta.common.{AgentID, Util}

import scala.collection.mutable
import java.io.File
import java.util.Scanner

// TODO get a macro or something for main, or have a more flexible cmdline tool
// likewise for the scripts
object AuctionExperiment {
  def main(args: Array[String]) {
    new AuctionExperiment(ExpConfig.from_args(args)).run()
  }

  // Scenario transformations
  def enable_auctions(s: Scenario) = s.copy(
    intersections = s.intersections.map(_.copy(ordering = OrderingType.Auction))
  )
  def disable_sysbids(s: Scenario) = s.copy(system_wallet = SystemWalletConfig.blank)
  def equal_budgets(s: Scenario) = s.copy(
    agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(budget = 1)))
  )
  def fixed_budgets(s: Scenario) = s.copy(
    agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(policy = WalletType.Static)))
  )
}

class AuctionExperiment(config: ExpConfig) extends Experiment(config) {
  def run() {
    // The generated scenario is the baseline.
    val sysbid_base = AuctionExperiment.enable_auctions(scenario)
    val nosys_base = AuctionExperiment.disable_sysbids(sysbid_base)

    output_data(List(
      run_trial(scenario, "fcfs"),
      run_trial(sysbid_base, "auctions_sysbids"),
      run_trial(nosys_base, "auctions_no_sysbids"),
      run_trial(AuctionExperiment.equal_budgets(sysbid_base), "equal_sysbids"),
      run_trial(AuctionExperiment.equal_budgets(nosys_base), "equal_no_sysbids"),
      run_trial(AuctionExperiment.fixed_budgets(sysbid_base), "fixed_sysbids"),
      run_trial(AuctionExperiment.fixed_budgets(nosys_base), "fixed_no_sysbids")
    ), scenario)
  }

  // TODO rename scenario, graph in base class. its too restrictive.
  // and refactor this.
  private def run_trial(s: Scenario, mode: String): RawResult = {
    val sim = s.make_sim().setup()
    val times = new TripTimeMetric(sim)
    val orig_routes = new OriginalRouteMetric(sim)
    val turn_delays = new TurnDelayMetric(sim)
    val turn_competition = new TurnCompetitionMetric(sim)
    simulate(sim)
    return RawResult(mode, Map(
      "times" -> times.result,
      "orig_routes" -> s.agents.map(a => a.id -> orig_routes(a.id)).toMap
    ), Map(
      "turn_delays" -> turn_delays.delays,
      "turn_competition" -> turn_competition.competition
    ))
  }

  protected def output_data(data: List[RawResult], s: Scenario) {
    output_per_agent("times", data, s)
    output_per_agent("orig_routes", data, s)
    output_per_category("turn_delays", data, "intersection_type")
    output_per_category("turn_competition", data, "intersection_type")
  }
}
