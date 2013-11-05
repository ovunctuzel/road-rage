// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, Sim_Event, Route_Event, EV_AgentSpawned, EV_AgentQuit,
                         EV_Reroute, IntersectionType, EV_Stat, EV_IntersectionOutcome,
                         EV_Transition, Scenario}
import utexas.aorta.map.{Edge, DirectedRoad, Turn}
import utexas.aorta.common.{Common, AgentID, IO, Util}

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

// Measure how long each agent's trip takes
class TripTimeMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "trip_time"

  info.sim.listen(name, _ match {
    case EV_Stat(s: Agent_Lifetime_Stat) => per_agent(s.id) = s.trip_time
    case _ =>
  })
}

// Measure how long a driver follows their original route.
class OriginalRouteMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "orig_routes"

  private val first_reroute_time = new mutable.HashMap[AgentID, Double]()
  info.sim.listen(name, _ match {
    case EV_AgentSpawned(a) => a.route.listen(name, _ match {
      case EV_Reroute(_, false) if !first_reroute_time.contains(a.id) =>
        first_reroute_time(a.id) = Common.tick
      case _ =>
    })
    // [0, 100]
    case EV_Stat(s: Agent_Lifetime_Stat) => per_agent(s.id) =
      100.0 * ((first_reroute_time.getOrElse(s.id, s.end_tick) - s.start_tick) / s.trip_time)
    case _ =>
  })
}

// Measure how much money the agent actually spends of their total budget
class MoneySpentMetric(info: MetricInfo) extends SinglePerAgentMetric(info) {
  override def name = "money_spent"

  info.sim.listen(name, _ match {
    case EV_Stat(s: Agent_Lifetime_Stat) => per_agent(s.id) = s.total_spent
    case _ =>
  })
}

// Measure how long drivers wait at intersections, grouped by intersection type
class TurnDelayMetric(info: MetricInfo) extends EmitMetric(info) {
  override def name = "turn_delays"
  override def header = "map scenario agent mode intersection_type turn_delay"

  info.sim.listen(name, _ match {
    case EV_Stat(s: Turn_Stat) => {
      val policy = info.sim.graph.vertices(s.vert.int).intersection.policy.policy_type.toString
      f.println(List(
        // could be accept_delay
        info.sim.graph.basename, info.uid, s.agent, mode, policy, s.total_delay
      ).mkString(" "))
    }
    case _ =>
  })
}

// Measure how congested roads are when agents enter them
class RoadCongestionMetric(info: MetricInfo) extends EmitMetric(info) {
  override def name = "road_congestion"
  override def header = "map scenario agent mode percent_full"

  info.sim.listen(name, _ match {
    case EV_AgentSpawned(a) => a.route.listen(name, _ match {
      case EV_Transition(_, to: Edge) => f.println(List(
        info.sim.graph.basename, info.uid, a.id, mode, to.directed_road.freeflow_percent_full
      ).mkString(" "))
      case _ =>
    })
    case _ =>
  })
}

// Measure how much competition is present at intersections
class TurnCompetitionMetric(info: MetricInfo) extends EmitMetric(info) {
  override def name = "turn_competition"
  override def header = "map scenario mode intersection_type losers"

  info.sim.listen(name, _ match {
    case EV_IntersectionOutcome(policy, losers) => f.println(List(
      info.sim.graph.basename, info.uid, mode, policy, losers.size
    ).mkString(" "))
    case _ =>
  })
}

class RouteRecordingMetric(info: MetricInfo) extends Metric(info) {
  override def name = "route_recording"

  private val routes = new mutable.HashMap[AgentID, mutable.ListBuffer[DirectedRoad]]()

  info.sim.listen(name, _ match {
    case EV_AgentSpawned(a) => {
      routes(a.id) = new mutable.ListBuffer[DirectedRoad]()
      a.route.listen(name, _ match {
        case EV_Transition(from, to: Turn) => {
          val path = routes(a.id)
          if (path.isEmpty) {
            path += to.from.directed_road
          }
          path += to.to.directed_road
        }
        case _ =>
      })
    }
    case _ =>
  })

  def apply(a: AgentID) = routes(a).toList
  def output(ls: List[Metric], scenario: Scenario) {
    throw new UnsupportedOperationException("Why save the actual routes?")
  }
}

// TODO delay on roads vs at intersections?
