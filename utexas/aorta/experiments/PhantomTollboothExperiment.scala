// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

object PhantomTollboothExperiment {
  def main(args: Array[String]) {
    new PhantomTollboothExperiment(ExpConfig.from_args(args)).run_experiment()
  }
}

class PhantomTollboothExperiment(config: ExpConfig) extends SmartExperiment(config, "phantom") {
  override def scenario_params = Array("budget=0-500")

  override def get_metrics(info: MetricInfo) = List(
    new TripTimeMetric(info), new TripDistanceMetric(info), new TripPathsMetric(info),
    new RoadUsageMetric(info)
  )

  override def run() {
    output_data(List(
      run_trial(ScenarioPresets.transform(scenario, "phantom_baseline"), "baseline"),
      run_trial(ScenarioPresets.transform(scenario, "phantom_tolls"), "tolls")
    ))
  }
}
