// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, Sim_Event, Route_Event, EV_AgentSpawned, EV_AgentQuit,
                         EV_Reroute, IntersectionType, EV_Stat, EV_IntersectionOutcome,
                         EV_Transition, Scenario}
import utexas.aorta.map.{Edge, DirectedRoad, Turn}
import utexas.aorta.common.{Common, AgentID, IO}

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

// Record many doubles per agent, further grouped by some string category
abstract class MultiplePerAgentMetric(info: MetricInfo) extends Metric(info) {
  def category: String
  private type Key = (AgentID, String)
  protected val per_agent_category = new mutable.HashMap[Key, mutable.Set[Double]]
    with mutable.MultiMap[Key, Double]

  // TODO why even store? just output as we collect, since we don't group by mode.
  override def output(ls: List[Metric], s: Scenario) {
    val f = info.io.output_file(name)
    f.println(s"map scenario agent mode $category value")
    for (raw_metric <- ls) {
      val metric = raw_metric.asInstanceOf[MultiplePerAgentMetric]
      for (key <- metric.per_agent_category.keys) {
        for (value <- metric.per_agent_category(key)) {
          f.println(List(
            info.sim.graph.basename, info.uid, key._1, metric.mode, key._2, value
          ).mkString(" "))
        }
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

// Measure how long drivers wait at intersections, grouped by intersection type
class TurnDelayMetric(info: MetricInfo) extends MultiplePerAgentMetric(info) {
  override def name = "turn_delays"
  override def category = "intersection_type"

  info.sim.listen(name, _ match {
    case EV_Stat(s: Turn_Stat) => {
      val policy = info.sim.graph.vertices(s.vert.int).intersection.policy.policy_type.toString
      per_agent_category.addBinding((s.agent, policy), s.total_delay)  // could be accept_delay
    }
    case _ =>
  })
}

// Measure how congested roads are when agents enter them, grouped by nothing (category "all")
class RoadCongestionMetric(info: MetricInfo) extends MultiplePerAgentMetric(info) {
  override def name = "road_congestion"
  override def category = "type"  // TODO this doesnt fit the pattern!

  info.sim.listen(name, _ match {
    case EV_AgentSpawned(a) => a.route.listen(name, _ match {
      case EV_Transition(_, to: Edge) =>
        per_agent_category((a.id, "all")) += to.directed_road.freeflow_percent_full
      case _ =>
    })
    case _ =>
  })
}

// Measure how much competition is present at intersections
class TurnCompetitionMetric(info: MetricInfo) extends Metric(info) {
  override def name = "turn_competition"

  private val losers_per_policy = IntersectionType.values.toList.map(
    t => t -> new mutable.ListBuffer[Double]()
  ).toMap

  info.sim.listen(name, _ match {
    case EV_IntersectionOutcome(policy, losers) => {
      losers_per_policy(policy) += losers.size
    }
    case _ =>
  })

  override def output(ls: List[Metric], scenario: Scenario) {
    val f = info.io.output_file(name)
    f.println("map scenario mode intersection_type losers")
    for (raw_metric <- ls) {
      val metric = raw_metric.asInstanceOf[TurnCompetitionMetric]
      for (key <- IntersectionType.values) {
        for (value <- metric.losers_per_policy(key)) {
          f.println(List(
            info.sim.graph.basename, info.uid, metric.mode, key, value
          ).mkString(" "))
        }
      }
    }
    f.close()
    info.io.compress(name)
    info.io.upload(name + ".gz")
  }
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
