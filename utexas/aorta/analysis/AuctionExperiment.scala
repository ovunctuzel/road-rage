// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Scenario, SystemWalletConfig, OrderingType, WalletType}

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
  def run() {
    // The generated scenario is the baseline.
    //simulate(0, scenario)

    val auction_base = AuctionExperiment.enable_auctions(scenario)
    for (base <- List(auction_base, AuctionExperiment.disable_sysbids(auction_base))) {
      //simulate(1, auction_base)
      //simulate(2, AuctionExperiment.equal_budgets(auction_base))
      //simulate(3, AuctionExperimentfixed_budgets(auction_base))
      // TODO fxn that sets up sim from scenario, metrics, writes output
    }
  }
}
