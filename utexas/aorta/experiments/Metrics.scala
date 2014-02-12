// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import utexas.aorta.sim.{Simulation, EV_AgentSpawned, EV_Reroute, EV_AgentQuit, EV_TurnFinished,
                         EV_IntersectionOutcome, EV_Transition}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.Scenario
import utexas.aorta.map.{Edge, Road, Turn}
import utexas.aorta.common.{AgentID, IO, Util, BinnedHistogram}

import scala.collection.mutable

case class MetricInfo(sim: Simulation, mode: String, io: IO, uid: String)

abstract class Metric(info: MetricInfo) {
  def name: String
  // Really should be in the companion object, and the type should indicate they're all the same.
  def output(ls: List[Metric], scenario: Scenario)
  def mode = info.mode
}

// Record one double per agent
abstract class SinglePerAgentMetric(info: MetricInfo) extends Metric(info) {
  protected val per_agent = new mutable.HashMap[AgentID, Double]()
  def apply(a: AgentID) = per_agent(a)

  override def output(ls: List[Metric], scenario: Scenario) {
    val f = info.io.output_file(name)
    f.println("map scenario agent priority " + ls.map(_.mode).mkString(" "))
    for (a <- scenario.agents) {
      f.println((
        List(info.sim.graph.basename, info.uid, a.id, a.wallet.priority) ++
        ls.map(_.asInstanceOf[SinglePerAgentMetric].per_agent(a.id))
      ).mkString(" "))
    }
    f.close()
    info.io.compress(name)
    info.io.upload(name + ".gz")
  }
}

// Record many values without structuring things at all
abstract class EmitMetric(info: MetricInfo) extends Metric(info) {
  // Subclasses should just write to this file directly
  protected val f = info.io.output_file(name + "_" + info.mode)

  def header: String

  override def output(ls: List[Metric], s: Scenario) {
    // Have to combine all the files now
    for (raw_metric <- ls) {
      raw_metric.asInstanceOf[EmitMetric].f.close()
    }
    val final_file = info.io.output_file(name)
    final_file.println(header)
    final_file.close()
    Util.blockingly_run(Seq("./tools/cat.sh", name))
    info.io.compress(name)
    info.io.upload(name + ".gz")
  }
}

// Too many values? Throw em into bins and count the size of each bin.
abstract class HistogramMetric(info: MetricInfo, width: Double) extends Metric(info) {
  protected val histogram = new BinnedHistogram(width)

  override def output(ls: List[Metric], scenario: Scenario) {
    val f = info.io.output_file(name)
    f.println(s"map scenario mode ${name}_bin count")
    for (raw_metric <- ls) {
      val metric = raw_metric.asInstanceOf[HistogramMetric]
      for (bin <- metric.histogram.bins) {
        f.println(List(
          info.sim.graph.basename, info.uid, metric.mode, (bin * width).toInt,
          metric.histogram(bin)
        ).mkString(" "))
      }
    }
    f.close()
    info.io.compress(name)
    info.io.upload(name + ".gz")
  }
}

// Measure how long each agent's trip takes
class TripTimeMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "trip_time"

  info.sim.listen(classOf[EV_AgentQuit], _ match {
    case e: EV_AgentQuit => per_agent(e.agent.id) = e.trip_time
  })
}

// Measure how long a driver follows their original route.
class OriginalRouteMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "orig_routes"

  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  info.sim.listen(classOf[EV_Reroute], _ match {
    case EV_Reroute(a, _, false, _, _, _) if !first_reroute_time.contains(a.id) =>
      first_reroute_time(a.id) = a.sim.tick
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
    case EV_Transition(a, _, to: Edge) => histogram.add(to.road.auditor.freeflow_percent_full)
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

class RouteRecordingMetric(info: MetricInfo) extends Metric(info) {
  override def name = "route_recording"

  private val routes = new mutable.HashMap[AgentID, mutable.ListBuffer[Road]]()

  info.sim.listen(classOf[EV_Transition], _ match {
    case EV_Transition(a, from, to: Turn) => {
      val path = routes.getOrElseUpdate(a.id, new mutable.ListBuffer[Road]())
      if (path.isEmpty) {
        path += to.from.road
      }
      path += to.to.road
    }
  })

  def apply(a: AgentID) = routes(a).toList
  override def output(ls: List[Metric], scenario: Scenario) {
    throw new UnsupportedOperationException("Why save the actual routes?")
  }
}

class LinkDelayMetric(info: MetricInfo) extends Metric(info) {
  override def name = "link_delay"

  // TODO will this eat too much memory?
  private val delays_per_time = info.sim.graph.roads.map(
    r => r -> new java.util.TreeMap[Double, Double]()
  ).toMap
  private val entry_time = new mutable.HashMap[Agent, Double]()

  info.sim.listen(classOf[EV_AgentSpawned], _ match {
    case EV_AgentSpawned(a) => entry_time(a) = a.sim.tick
  })
  info.sim.listen(classOf[EV_Transition], _ match {
    // Entering a road
    case EV_Transition(a, from: Turn, to) => entry_time(a) = a.sim.tick
    // Exiting a road
    case EV_Transition(a, from: Edge, to: Turn) =>
      add_delay(entry_time(a), a.sim.tick - entry_time(a), from.road)
  })

  private def add_delay(entry_time: Double, delay: Double, at: Road) {
    // Two agents can enter the same Road at the same time (on different lanes)
    // Just arbitrarily overwrite if there's a conflict
    delays_per_time(at).put(entry_time, delay)
  }

  override def output(ls: List[Metric], scenario: Scenario) {
    // Don't actually save anything!
  }

  // Many possible interpolations for this...
  def delay(on: Road, at: Double) = delays_per_time(on).lowerKey(at) match {
    // 'at' is before all entries here? then the road's clear
    case 0.0 => on.freeflow_time  // TODO 0.0 is how failure gets encoded by java treemap...
    case entry_time => delays_per_time(on).get(entry_time) match {
      // 'at' happens after the most recent entry finishes
      case delay if at > entry_time + delay => on.freeflow_time
      // This instance overlaps 'at', so just use the same delay.
      case delay => delay
    }
  }
}

// TODO delay on roads vs at intersections?
