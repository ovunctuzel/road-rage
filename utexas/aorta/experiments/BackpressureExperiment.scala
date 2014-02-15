// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.make.{Scenario, OrderingType, IntersectionType}

object BackpressureExperiment {
  def main(args: Array[String]) {
    new BackpressureExperiment(ExpConfig.from_args(args)).run_experiment()
  }

  // Scenario transformations
  def use_backpressure(s: Scenario) = s.copy(
    intersections = s.intersections.map(_.copy(ordering = OrderingType.Pressure))
  )
  def use_reservation(s: Scenario) = s.copy(
    intersections = s.intersections.map(_.copy(policy = IntersectionType.Reservation))
  )
}

class BackpressureExperiment(config: ExpConfig) extends SmartExperiment(config) {
  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TurnDelayMetric(info)
  )

  override def run() {
    val fcfs = scenario
    val backpressure = BackpressureExperiment.use_backpressure(fcfs)

    output_data(List(
      run_trial(fcfs, "fcfs_mixed"),
      run_trial(backpressure, "backpressure_mixed"),
      run_trial(BackpressureExperiment.use_reservation(fcfs), "fcfs_reservation"),
      run_trial(BackpressureExperiment.use_reservation(backpressure), "backpressure_reservation")
    ))
  }
}
