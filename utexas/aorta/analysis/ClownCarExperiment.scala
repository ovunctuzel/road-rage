// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.make.{Scenario, OrderingType, WalletType, RouterType}

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
  // We never want to send somebody to a road already with more than its freeflow capacity, so the
  // max budget should be 50 (since that's the cost of a 100% freeflow-congested road).
  override def scenario_params = Array("budget=0-50")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new OriginalRouteMetric(info), new RoadCongestionMetric(info)
  )

  override def run() {
    val base = ClownCarExperiment.smart_intersections(scenario)

    output_data(List(
      run_trial(base, "baseline"),
      run_trial(ClownCarExperiment.use_router(base, RouterType.DumbToll), "avoid_max"),
      run_trial(ClownCarExperiment.use_router(base, RouterType.SumToll), "avoid_sum")
      //run_trial(ClownCarExperiment.use_router(base, RouterType.TollThreshold), "toll_threshold")
    ), scenario)
  }
}
