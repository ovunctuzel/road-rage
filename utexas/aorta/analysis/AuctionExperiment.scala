// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Scenario, SystemWalletConfig, OrderingType, WalletType}
import utexas.aorta.common.{AgentID, Util}

import scala.collection.mutable
import java.io.File
import java.util.Scanner

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

    output_times(List(
      "fcfs" -> run_trial(scenario),
      "auctions_sysbids" -> run_trial(sysbid_base),
      "auctions_no_sysbids" -> run_trial(nosys_base),
      "equal_sysbids" -> run_trial(AuctionExperiment.equal_budgets(sysbid_base)),
      "equal_no_sysbids" -> run_trial(AuctionExperiment.equal_budgets(nosys_base)),
      "fixed_sysbids" -> run_trial(AuctionExperiment.fixed_budgets(sysbid_base)),
      "fixed_no_sysbids" -> run_trial(AuctionExperiment.fixed_budgets(nosys_base))
    ), scenario)
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

  // TODO move to base class
  protected def output_times(times: List[(String, mutable.Map[AgentID, Double])], s: Scenario) {
    val f = output("times")
    f.println("map scenario agent priority " + times.map(_._1).mkString(" "))
    val uid = Util.unique_id
    // We should have the same agents in all runs
    for (a <- s.agents) {
      f.println((List(graph.basename, uid, a.id, a.wallet.priority) ++ times.map(_._2(a.id))).mkString(" "))
    }
    // TODO do this differently...
    f.close()
    compress("times")
    upload("times.gz")
  }
}

// TODO this is more like, compare trip times
object AuctionResults {
  case class RawResult(
    graph: String, scenario: Long, agent: Int, priority: Int, fcfs: Double,
    auctions_sysbids: Double, auctions_no_sysbids: Double, equal_sysbids: Double,
    equal_no_sysbids: Double, fixed_sysbids: Double, fixed_no_sysbids: Double
  ) {
    def per_mode = ResultPerMode(
      fcfs, auctions_sysbids, auctions_no_sysbids, equal_sysbids, equal_no_sysbids, fixed_sysbids,
      fixed_no_sysbids
    )
  }

  case class ResultPerMode(
    fcfs: Double, auctions_sysbids: Double, auctions_no_sysbids: Double, equal_sysbids: Double,
    equal_no_sysbids: Double, fixed_sysbids: Double, fixed_no_sysbids: Double
  ) {
    // TODO need vector type in scala!
    def plus(o: ResultPerMode) = ResultPerMode(
      fcfs + o.fcfs, auctions_sysbids + o.auctions_sysbids,
      auctions_no_sysbids + o.auctions_no_sysbids, equal_sysbids + o.equal_sysbids,
      equal_no_sysbids + o.equal_no_sysbids, fixed_sysbids + o.fixed_sysbids,
      fixed_no_sysbids + o.fixed_no_sysbids
    )

    def times(w: Double) = ResultPerMode(
      w * fcfs, w * auctions_sysbids, w * auctions_no_sysbids, w * equal_sysbids,
      w * equal_no_sysbids, w * fixed_sysbids, w * fixed_no_sysbids
    )

    def savings = ResultPerMode(
      0, fcfs - auctions_sysbids, fcfs - auctions_no_sysbids, fcfs - equal_sysbids,
      fcfs - equal_no_sysbids, fcfs - fixed_sysbids, fcfs - fixed_no_sysbids
    )

    override def toString = s"$fcfs $auctions_sysbids $auctions_no_sysbids $equal_sysbids $equal_no_sysbids $fixed_sysbids $fixed_no_sysbids"
  }
  val zero = ResultPerMode(0, 0, 0, 0, 0, 0, 0)

  def main(args: Array[String]) {
    val raws = args.toList.flatMap(read_raws)
    // TODO for now, neglect pairing specific results by scenario.
    val unweighted = unweighted_times(raws)
    val weighted = weighted_times(raws)
    val raws_per_city = raws.groupBy(_.graph)
    val cities = unweighted.keys
    val modes = "fcfs auctions_sysbids auctions_no_sysbids equal_sysbids equal_no_sysbids fixed_sysbids fixed_no_sysbids"

    for (city <- cities) {
      val f1 = Util.output(s"unweighted_$city")
      f1.println(modes)
      unweighted(city).foreach(r => f1.println(r.toString))

      val f2 = Util.output(s"weighted_$city")
      f2.println(modes)
      weighted(city).foreach(r => f2.println(r.toString))

      val f3 = Util.output(s"unweighted_savings_$city")
      f3.println(modes)
      unweighted(city).foreach(r => f3.println(r.savings.toString))

      val f4 = Util.output(s"unweighted_savings_per_agent_$city")
      f4.println(modes)
      raws_per_city(city).foreach(r => f4.println(r.per_mode.savings.toString))
    }
  }

  private def read_raws(fn: String): List[RawResult] = {
    val raws = new mutable.ListBuffer[RawResult]()
    val s = new Scanner(new File(fn))
    s.nextLine()  // header
    while (s.hasNext) {
      raws += RawResult(s.next, s.nextLong, s.nextInt, s.nextInt, s.nextDouble, s.nextDouble,
                        s.nextDouble, s.nextDouble, s.nextDouble, s.nextDouble, s.nextDouble)
    }
    return raws.toList
  }

  // per city
  private def unweighted_times(raws: List[RawResult]): Map[String, List[ResultPerMode]] = {
    val times = new mutable.HashMap[String, List[ResultPerMode]]()
    val results_per_city = raws.groupBy(_.graph)
    for (city <- results_per_city.keys) {
      val results_per_scenario = results_per_city(city).groupBy(_.scenario)
      times(city) = results_per_scenario.values.map(
        run => run.map(_.per_mode).foldLeft(zero)((a, b) => a.plus(b))
      ).toList
    }
    return times.toMap
  }

  // per city
  // TODO refactor...
  private def weighted_times(raws: List[RawResult]): Map[String, List[ResultPerMode]] = {
    val times = new mutable.HashMap[String, List[ResultPerMode]]()
    val results_per_city = raws.groupBy(_.graph)
    for (city <- results_per_city.keys) {
      val results_per_scenario = results_per_city(city).groupBy(_.scenario)
      times(city) = results_per_scenario.values.map(
        run => run.map(a => a.per_mode.times(a.priority)).foldLeft(zero)((a, b) => a.plus(b))
      ).toList
    }
    return times.toMap
  }
}
