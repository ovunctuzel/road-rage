// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.make.{Scenario, OrderingType, IntersectionType, RouterType,
                              ReroutePolicyType}

object PhantomTollboothExperiment {
  def main(args: Array[String]) {
    new PhantomTollboothExperiment(ExpConfig.from_args(args)).run_experiment()
  }

  // Scenario transformations
  def use_baseline(s: Scenario) = s.copy(intersections = s.intersections.map(_.copy(
    policy = IntersectionType.Reservation, ordering = OrderingType.FIFO
  )))
  def use_tolls(s: Scenario) = s.copy(
    intersections = s.intersections.map(_.copy(
      policy = IntersectionType.Reservation, ordering = OrderingType.Toll
    )),
    agents = s.agents.map(a => a.copy(route = a.route.copy(
      orig_router = RouterType.Tollbooth, rerouter = RouterType.Tollbooth,
      reroute_policy = ReroutePolicyType.PriceChange
    )))
  )
}

class PhantomTollboothExperiment(config: ExpConfig) extends SmartExperiment(config) {
  override def scenario_params = Array("budget=0-500")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TripDistanceMetric(info)
  )

  override def run() {
    output_data(List(
      run_trial(PhantomTollboothExperiment.use_baseline(scenario), "baseline"),
      run_trial(PhantomTollboothExperiment.use_tolls(scenario), "tolls")
    ))
  }
}
