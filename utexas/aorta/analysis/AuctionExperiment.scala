// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Scenario, SystemWalletConfig, OrderingType, WalletType}
import utexas.aorta.common.AgentID

import scala.collection.mutable

// TODO get a macro or something for main, or have a more flexible cmdline tool
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
  private var round = 0 // TODO count this in base class too

  def run() {
    // The generated scenario is the baseline.
    val sysbid_base = AuctionExperiment.enable_auctions(scenario)
    val nosys_base = AuctionExperiment.disable_sysbids(sysbid_base)

    val fcfs = run_trial(scenario)
    val auction_sys = run_trial(sysbid_base)
    val auction_nosys = run_trial(nosys_base)
    val equal_sys = run_trial(AuctionExperiment.equal_budgets(sysbid_base))
    val equal_nosys = run_trial(AuctionExperiment.equal_budgets(nosys_base))
    val fixed_sys = run_trial(AuctionExperiment.fixed_budgets(sysbid_base))
    val fixed_nosys = run_trial(AuctionExperiment.fixed_budgets(nosys_base))

    // TODO have modes... transformation fxn, name.
    // TODO or just move the lines above down here...
    output_times(Map(
      "FCFS" -> fcfs, "Auction (with sysbids)" -> auction_sys,
      "Auction (no sysbids)" -> auction_nosys, "Equal (with sysbids)" -> equal_sys,
      "Equal (no sysbids)" -> equal_nosys, "Fixed (with sysbids)" -> fixed_sys,
      "Fixed (no sysbids)" -> fixed_nosys
    ))
  }

  // TODO rename scenario, graph in base class. its too restrictive.
  // and refactor this.
  private def run_trial(s: Scenario): mutable.Map[AgentID, Double] = {
    round += 1
    val sim = s.make_sim().setup()
    val times = record_trip_times()
    // TODO record other metrics too.
    simulate(round, sim)
    return times
  }
}
