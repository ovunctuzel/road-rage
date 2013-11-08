// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Scenario, OrderingType, WalletType, RouterType}

object ClownCarExperiment {
  def main(args: Array[String]) {
    new ClownCarExperiment(ExpConfig.from_args(args)).run_experiment()
  }

  // Scenario transformations
  def smart_intersections(s: Scenario) = s.copy(
    // System bids on by default
    intersections = s.intersections.map(_.copy(ordering = OrderingType.Auction)),
    agents = s.agents.map(a => a.copy(wallet = a.wallet.copy(budget = 1, policy = WalletType.Static)))
  )
  def use_router(s: Scenario, r: RouterType.Value) = s.copy(
    agents = s.agents.map(a => a.copy(route = a.route.copy(orig_router = r, rerouter = r)))
  )
}

class ClownCarExperiment(config: ExpConfig) extends SmartExperiment(config) {
  // TODO so far, just need to match scale of possible tolls
  override def scenario_params = Array("budget=0-500")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new RoadCongestionMetric(info)
  )

  override def run() {
    val baseline = ClownCarExperiment.smart_intersections(scenario)

    output_data(List(
      run_trial(baseline, "baseline"),
      run_trial(ClownCarExperiment.use_router(baseline, RouterType.DumbToll), "dumb_tolls"),
      run_trial(ClownCarExperiment.use_router(baseline, RouterType.TollThreshold), "toll_threshold")
    ), scenario)
  }
}
