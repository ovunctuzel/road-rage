// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

object Results extends PlotUtil {
  def main(args: Array[String]) {
    (args(0), args(1)) match {
      // TODO mark all of these as for single scenarios only
      case ("time", "histogram") => show(histogram(time_distr(read_times(args(2)))))
      case ("time", "boxplot") => show(boxplot(time_distr(read_times(args(2)))))
      case ("turn_delay", "histogram") => show(histogram(turn_delay_distr(read_turn_delay(args(2)))))
      case ("time_vs_priority", fn) => show(scatterplot(time_vs_priority(read_times(fn))))
    }
  }

  private def time_vs_priority(s: ScenarioTimes) = ScatterData(
    s.modes.zipWithIndex.map(_ match {
      case (mode, idx) => mode -> s.agents.map(a => (a.priority, a.times(idx) / a.ideal_time))
    }).toMap, "Time vs priority", "Priority", "Trip time / ideal time"
  )

  private def time_distr(s: ScenarioTimes) = DistributionData(
    s.modes.zipWithIndex.map(_ match {
      case (mode, idx) => mode -> s.agents.map(a => a.times(idx) / a.ideal_time)
    }).toMap, "Trip time distribution", "Trip time / ideal time"
  )

  private def turn_delay_distr(s: ScenarioTurnDelays) = PreBinnedData(
    s.delays.groupBy(_.mode).map(_ match {
      case (mode, ls) => mode -> ls.map(d => (d.bin, d.count))
    }).toMap, "Turn delay distribution", "Turn delay (s)"
  )
}
