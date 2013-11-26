// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.make.Scenario
import utexas.aorta.common.{Util, IO}

// Dynamic traffic assignment
// TODO use more of the experiment framework, once i figure out what i want
class DTA() {
  def run(base_scenario: Scenario, iterations: Int) {
    var current_scenario = base_scenario
    for (round <- Range(0, iterations)) {
      val sim = current_scenario.make_sim().setup()
      val delay = new LinkDelayMetric(MetricInfo(sim, s"dta_$round", new IO(None), Util.unique_id))
      // TODO use experiment's simulate()
      while (!sim.done) {
        sim.step()
      }
      current_scenario = change_paths(current_scenario, delay)
    }
  }

  // Reroute some drivers using actual delays
  private def change_paths(base_scenario: Scenario, delay: LinkDelayMetric): Scenario = {
    // TODO what percent of the population? choose based on budgets.
    return base_scenario // TODO
  }
}
