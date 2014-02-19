// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.{Simulation, EV_AgentSpawned, EV_Reroute, EV_AgentQuit, EV_TurnFinished,
                         EV_IntersectionOutcome, EV_Transition}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.{Scenario, MkAgent}
import utexas.aorta.map.{Edge, Road, Turn}
import utexas.aorta.common.{AgentID, IO, Util, BinnedHistogram}

import scala.collection.mutable

case class MetricInfo(sim: Simulation, mode: String, io: IO, uid: String)

// One per trial
abstract class Metric(info: MetricInfo) {
  def name: String
  // Really should be in the companion object, and the type should indicate they're all the same.
  def output(ls: List[Metric])
  def mode = info.mode

  protected def fn = s"${name}.${info.uid}.${info.sim.graph.basename}"
}

// Record one double per agent
abstract class SinglePerAgentMetric(info: MetricInfo) extends Metric(info) {
  protected val per_agent = new mutable.HashMap[AgentID, Double]()
  def apply(a: AgentID) = per_agent(a)

  // These are invariant of trial and printed before the per-agent metric
  protected def extra_fields: List[String] = Nil
  protected def extra_data(a: MkAgent): List[Double] = Nil

  override def output(ls: List[Metric]) {
    val f = info.io.output_file(fn)
    f.println(("agent" :: extra_fields ++ ls.map(_.mode)).mkString(" "))
    for (a <- info.sim.scenario.agents) {
      f.println((
        a.id :: extra_data(a) ++ ls.map(_.asInstanceOf[SinglePerAgentMetric].per_agent(a.id))
      ).mkString(" "))
    }
    f.close()
    info.io.done(fn)
  }
}

// Too many values? Throw em into bins and count the size of each bin.
abstract class HistogramMetric(info: MetricInfo, width: Double) extends Metric(info) {
  protected val histogram = new BinnedHistogram(width)

  override def output(ls: List[Metric]) {
    val f = info.io.output_file(fn)
    f.println(s"mode ${name}_bin count")
    for (raw_metric <- ls) {
      val metric = raw_metric.asInstanceOf[HistogramMetric]
      for (bin <- metric.histogram.bins) {
        f.println(List(metric.mode, (bin * width).toInt, metric.histogram(bin)).mkString(" "))
      }
    }
    f.close()
    info.io.done(fn)
  }
}

// Measure how long each agent's trip takes
class TripTimeMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "trip_time"

  override def extra_fields = List("priority", "ideal_time")
  override def extra_data(a: MkAgent) = List(a.wallet.priority, a.ideal_time(info.sim.graph))

  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) = e.trip_time
  })
}

// Measure how far each agent travels
class TripDistanceMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "trip_distance"

  override def extra_fields = List("priority", "ideal_distance")
  override def extra_data(a: MkAgent) = List(a.wallet.priority, a.ideal_distance(info.sim.graph))

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from: Turn, to: Edge) => per_agent(a.id) += to.road.length
    // Don't miss the first road
    case EV_Transition(a, from: Edge, to: Turn) if !per_agent.contains(a.id) =>
      per_agent(a.id) = from.road.length
    case _ =>
  })
}

// Measure how long a driver follows their original route.
class OriginalRouteMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "orig_routes"

  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  info.sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(a, _, false, _, _, _) if !first_reroute_time.contains(a.id) =>
      first_reroute_time(a.id) = a.sim.tick
    case _ =>
  })
  // per_agent is [0, 100]
  info.sim.listen(classOf[EV_AgentQuit], _ match { case e: EV_AgentQuit =>
    per_agent(e.agent.id) =
      100.0 * ((first_reroute_time.getOrElse(e.agent.id, e.end_tick) - e.birth_tick) / e.trip_time)
  })
}

// Measure how much money the agent actually spends of their total budget
class MoneySpentMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "money_spent"

  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) = e.total_spent
  })
}

// Measure how long drivers wait at intersections, grouped by intersection type
// TODO multiple HistogramMetric. print intersection_type
class TurnDelayMetric(info: MetricInfo) extends HistogramMetric(info, 5.0) {
  override def name = "turn_delay"

  info.sim.listen(classOf[EV_TurnFinished], _ match {
    // could be accept_delay
    case e: EV_TurnFinished => histogram.add(e.total_delay)
  })
}

// Measure how congested roads are when agents enter them
class RoadCongestionMetric(info: MetricInfo) extends HistogramMetric(info, 10.0) {
  override def name = "road_congestion"

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, _, to: Edge) => histogram.add(to.road.road_agent.freeflow_percent_full)
    case _ =>
  })
}

// Measure how much competition is present at intersections
// TODO multiple HistogramMetric. print intersection_type
class TurnCompetitionMetric(info: MetricInfo) extends HistogramMetric(info, 1.0) {
  override def name = "turn_competition"

  info.sim.listen(classOf[EV_IntersectionOutcome], _ match {
    case EV_IntersectionOutcome(policy, losers) => histogram.add(losers.size)
  })
}
