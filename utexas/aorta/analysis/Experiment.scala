// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable
import java.io.{File, PrintWriter, FileWriter}

import utexas.aorta.map.Graph
import utexas.aorta.sim.{ScenarioTool, Simulation, Scenario, Sim_Event, EV_Heartbeat, EV_AgentSpawned,
                         RouteRecorder, Agent, EV_Stat}

import utexas.aorta.common.{RNG, Util, Flags, Common, AgentID}

// TODO divorce scenario generation from the rest
case class ExpConfig(
  spawn_per_hour: Int,
  generations: Int,
  report_every_ms: Int,
  deadline: Int,
  map_fn: String,
  gs_prefix: Option[String]
)

object ExpConfig {
  private val rng = new RNG()
  private def random_map =
    rng.choose(new File("maps").listFiles.map(_.toString).filter(_.endsWith(".map")))

  private val report_locally_every_ms = 10 * 1000
  private val report_remotely_every_ms = 60 * 1000

  def template = ExpConfig(0, 0, 0, 12 * 3600, random_map, None)

  def tiny_test = template.copy(spawn_per_hour = 1000, generations = 1)
  def small_local_test = template.copy(spawn_per_hour = 5000, generations = 3)
  def atx_cloud_test = template.copy(
    spawn_per_hour = rng.int(10000, 15000), generations = 3,
    map_fn = rng.choose(Array(
      "maps/austin.map", "maps/baton_rouge.map", "maps/seattle.map", "maps/sf.map"
    )))
  //def safe_atx_cloud_test =
    //template.copy(spawn_per_hour = 10000, generations = 3, map_fn = "maps/austin.map")

  def from_args(args: Array[String]): ExpConfig = {
    // Empty, just mode, or mode and GS prefix
    if (args.isEmpty) {
      return small_local_test
    } else {
      val base = args.head match {
        case "tiny" => tiny_test
        case "local" => small_local_test
        case "cloud" => atx_cloud_test
        case _ => throw new IllegalArgumentException(s"Dunno mode ${args.head}")
      }
      if (args.tail.isEmpty) {
        return base.copy(report_every_ms = report_locally_every_ms)
      } else {
        return base.copy(
          report_every_ms = report_remotely_every_ms, gs_prefix = args.tail.headOption
        )
      }
    }
  }
}

// per mode
// TODO or change schema? col is map, scenario, agent, trip time, percent. row per mode.
case class Experience(
  mode: String, per_agent: Map[String, Map[AgentID, Double]],
  per_category: Map[String, Map[String, List[Double]]]
)

class Experiment(config: ExpConfig) {
  // TODO do the caching in the graph load layer.
  protected lazy val scenario = get_scenario()
  protected lazy val graph = Graph.load(scenario.map_fn)
  protected val uid = Util.unique_id
  Flags.set("--savestate", "false")

  protected def get_scenario(): Scenario = {
    val scenario_fn = config.map_fn.replace("maps/", "scenarios/").replace(".map", "_routes")
    notify("Generating scenario")
    ScenarioTool.main(Array(
      config.map_fn, "--out", scenario_fn, "--spawn", config.spawn_per_hour.toString,
      "delay=3600", "lifetime=3600", "generations=" + config.generations
    ))
    return Scenario.load(scenario_fn)
  }

  // TODO => trip time
  // TODO move to Metrics
  protected def record_trip_times(
    sim: Simulation, include: () => Boolean = () => true
  ): mutable.Map[AgentID, Double] = {
    val times = new mutable.HashMap[AgentID, Double]()
    sim.listen("trip-time-recorder", (ev: Sim_Event) => { ev match {
        case EV_Stat(s: Agent_Lifetime_Stat) if include() => {
          times(s.id) = s.trip_time
        }
        case _ =>
      }
    })
    return times
  }

  protected def record_agent_paths(
    sim: Simulation, include: (Agent) => Boolean = Function.const(true)
  ): mutable.Map[AgentID, RouteRecorder] = {
    // TODO could save memory by computing score of route incrementally
    val routes = new mutable.HashMap[AgentID, RouteRecorder]()
    sim.listen("route-analyzer", (ev: Sim_Event) => { ev match {
      case EV_AgentSpawned(a) => {
        if (include(a)) {
          routes(a.id) = new RouteRecorder(a.route)
        }
      }
      case _ =>
    } })
    return routes
  }

  protected var round = 0
  protected def simulate(sim: Simulation) = {
    var last_time = 0L
    sim.listen("experiment-framework", (ev: Sim_Event) => { ev match {
      case EV_Heartbeat(info) => {
        val now = System.currentTimeMillis
        if (now - last_time > config.report_every_ms) {
          last_time = now
          notify(s"Round $round at ${Util.time_num(sim.tick)}: ${info.describe}" +
                 s" / ${sim.finished_count} finished")
        }
      }
      case _ =>
    } })

    while (!sim.done) {
      sim.step()
      if (sim.tick >= config.deadline) {
        round += 1
        throw new Exception(s"Simulation past ${config.deadline} seconds. Giving up")
      }
    }
    round += 1
  }

  protected def notify(status: String) {
    config.gs_prefix match {
      case Some(prefix) => upload_gs(prefix + "status", status)
      case None => println(s"*** $status ***")
    }
  }

  // TODO mark a bunch of files with this class, auto upload
  protected def upload_gs(fn: String, contents: String) {
    Runtime.getRuntime.exec(Array("./tools/cloud/upload_gs.sh", fn, contents))
  }

  protected def upload(fn: String) {
    config.gs_prefix match {
      case Some(prefix) => Runtime.getRuntime.exec(Array(
        "gsutil", "cp", fn, prefix + fn
      ))
      case None =>
    }
  }

  // TODO auto close this, and auto upload to GS.
  // TODO and keep it open if needed
  def output(fn: String) = new PrintWriter(new FileWriter(new File(fn)), true /* autoFlush */)
  def compress(fn: String) {
    Runtime.getRuntime.exec(Array("gzip", fn))
  }

  // One double per agent per mode
  protected def output_per_agent(metric: String, data: List[Experience], s: Scenario) {
    val f = output(metric)
    f.println("map scenario agent priority " + data.map(_.mode).mkString(" "))
    // We should have the same agents in all runs
    for (a <- s.agents) {
      f.println((
        List(graph.basename, uid, a.id, a.wallet.priority) ++
        data.map(per_mode => per_mode.per_agent(metric)(a.id))
      ).mkString(" "))
    }
    // TODO do this differently...
    f.close()
    compress(metric)
    upload(metric + ".gz")
  }

  // TODO i kind of want sql.
  protected def output_per_category(
    metric: String, data: List[Experience], category: String
  ) {
    val f = output(metric)
    f.println(s"mode $category value")
    for (per_mode <- data) {
      for (instance <- per_mode.per_category(metric).keys) {
        for (value <- per_mode.per_category(metric)(instance)) {
          f.println(List(per_mode.mode, instance, value).mkString(" "))
        }
      }
    }
    f.close()
    compress(metric)
    upload(metric + ".gz")
  }
}
