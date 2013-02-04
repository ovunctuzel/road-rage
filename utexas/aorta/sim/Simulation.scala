// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{MutableList, PriorityQueue, ListBuffer}
import scala.collection.mutable.{HashSet => MutableSet}
import java.io.FileWriter
import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

import utexas.aorta.map.{Graph, Road, Edge, Vertex, Turn, DirectedRoad}
import utexas.aorta.map.make.PlaintextReader
import utexas.aorta.sim.policies._

import utexas.aorta.{Util, cfg}
import utexas.aorta.analysis.{Stats, Active_Agents_Stat, Simulator_Speedup_Stat}

// This just adds a notion of agents
class Simulation(roads: Array[Road], edges: Array[Edge], vertices: Array[Vertex])
  extends Graph(roads, edges, vertices)
{
  ////////////// Misc
  var listeners: List[Sim_Event => Any] = Nil
  def tell_listeners(ev: Sim_Event) = listeners.foreach(l => l(ev))

  // functions that take nothing and return nothing, and the tick time
  class Callback(val at: Double, val cb: () => Unit) extends Ordered[Callback] {
    // small weights first
    def compare(other: Callback) = other.at.compare(at)
  }
  val events = new PriorityQueue[Callback]()

  /////////// Agent management
  Agent.sim = this  // let them get to us
  Util.log("Creating queues and intersections for collision handling...")
  traversables.foreach(t => t.queue = new Queue(t))
  vertices.foreach(v => v.intersection = new Intersection(v))
  var agents: Vector[Agent] = Vector.empty
  // We pass ready_to_spawn to generators directly
  private var ready_to_spawn = new ListBuffer[SpawnAgent]()
  private var generators: SortedSet[Generator] = new TreeSet[Generator]
  private var generator_count = 0   // just for informational/UI purposes
  private var id_cnt = -1

  // Just for debug.
  var debug_agent: Option[Agent] = None
  def debug(a: Agent) = debug_agent match {
    case Some(special) if a == special => true
    case _ => false
  }

  def next_id(): Int = {
    id_cnt += 1
    return id_cnt
  }

  def get_agent(id: Int): Option[Agent] = {
    // Binary search for them.
    def search(low: Int, high: Int): Option[Agent] = (low + high) / 2 match {
      case _ if high < low => None
      case mid if agents(mid).id > id => search(low, mid - 1)
      case mid if agents(mid).id < id => search(mid + 1, high)
      case mid => Some(agents(mid))
    }
    return search(0, agents.size - 1)
  }

  def has_agent(a: Agent) = get_agent(a.id).isDefined

  def add_gen(g: Generator) = {
    generators += g
    Simulation.generators += g
  }

  // Be sure to call pre_step at least once to poke all the generators
  def done = agents.isEmpty && ready_to_spawn.isEmpty && generator_count == 0

  def shutdown = Generator.shutdown

  def describe_agents = "%d / %d / %d (%d generators)".format(
    agents.size, ready_to_spawn.size, generator_count, generators.size
  )

  // Added by a queue that does an in-place check and thinks there could be an
  // issue.
  val active_queues = new MutableSet[Queue]   // TODO list?
  // All intersections with agents in them.
  val active_intersections = new MutableSet[Intersection]

  def wait_for_all_generators() = {
    // A pre-step to schedule requests, then blockingly wait for routes to be done
    pre_step
    generators.foreach(g => g.wait_for_all)
  }

  ////////////// Timing

  // this represents total "real seconds", so all velocity formulas work out
  // normally. so far this is also just for external UIness, nothing internal
  // cares.
  var tick: Double = 0
  def number_of_ticks = (tick / cfg.dt_s).toInt // TODO if dt_s changes, breaks
  // we only ever step with dt = cfg.dt_s, so we may have leftover.
  private var dt_accumulated: Double = 0
  // WE CAN GO FASTER
  var desired_sim_speed = 1.0
  var actual_sim_speed = 0.0

  // TODO cfg
  def slow_down(amount: Double = 0.5) = {
    desired_sim_speed = math.max(0.5, desired_sim_speed - amount)
  }
  def speed_up(amount: Double = 0.5) = {
    desired_sim_speed += amount
  }

  // Returns true if at least one generator is active
  def pre_step(): Boolean = {
    // First, fire any scheduled callbacks (usually intersections or
    // overseers)
    // (It's good to do this first because they might create generators as part
    // of resimulation)
    while (events.nonEmpty && tick >= events.head.at) {
      val ev = events.dequeue
      ev.cb()
    }

    // Then, give generators a chance to introduce more agents into the system
    val changed = generators.nonEmpty
    generator_count = 0
    val reap = generators.filter(g => {
      val done = g.run(ready_to_spawn)
      generator_count += g.count_pending
      done
    })
    generators --= reap

    // Finally, introduce any new agents that are ready to spawn into the system
    ready_to_spawn = ready_to_spawn.filter(a => !try_spawn(a))

    return changed
  }

  def schedule(at: Double, callback: () => Unit) {
    events.enqueue(new Callback(at, callback))
  }

  private var last_real_time = 0.0
  private var last_sim_time = 0.0

  // Returns (the number of agents that moved, total number of agents processed)
  def step(dt_s: Double): (Int, Int) = {
    // This value is dt in simulation time, not real time
    dt_accumulated += dt_s * desired_sim_speed

    var moved_count = 0
    var total_count = 0

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step into exactly consistent/equal pieces, if needed.
    while (dt_accumulated >= cfg.dt_s) {
      dt_accumulated -= cfg.dt_s
      tick += cfg.dt_s

      // Do this consistently every tick so we have deterministic resimulation,
      // which certainly depends on when agents are introduced to the system.
      // (If the routes take different amounts of time to compute and aren't
      // ASAP, this ISN'T deterministic yet.)
      pre_step

      // If you wanted crazy speedup, disable all but agent stepping and
      // reacting. But that involves trusting that no simulation violations
      // could occur. ;)

      // Queues will lazily start_step, remembering their current state, when
      // they need to.

      // Move agents.
      var active_cnt = 0
      agents.foreach(a => {
        if (a.step(cfg.dt_s)) {
          moved_count += 1
        }
        active_cnt += 1
      })
      total_count += active_cnt
      if (number_of_ticks % 5 == 0) {
        Stats.record(Active_Agents_Stat(tick.toInt, active_cnt))
      }

      // Just check the ones we need to.
      active_queues.foreach(q => q.end_step)

      active_intersections.foreach(i => i.end_step)

      // Let intersections react to the new world.
      vertices.foreach(v => {
        v.intersection.react
      })

      // Let agents react to the new world.

      // Sequential or parallel?
      val reap = agents.filter(a => a.react).toSet
      /*val sz = 100  // TODO how to tune this?
      val max_idx = (agents.size.toDouble / sz).ceil.toInt
      val reap = Range(0, max_idx).par.flatMap(
        // TODO off by one?
        idx => agents.view(sz * idx, sz * (idx + 1)).filter(a => a.react)
      ).toSet*/

      if (reap.nonEmpty) {
        // TODO batch GC.
        reap.foreach(a => a.terminate)
        agents = agents.filter(a => !reap(a))
      }
      
      // reset queues that need to be checked
      active_queues.clear
    }

    val now = System.currentTimeMillis
    if (now - last_real_time >= 1000.0) {
      actual_sim_speed = tick - last_sim_time
      last_sim_time = tick
      last_real_time = now
      Stats.record(Simulator_Speedup_Stat(actual_sim_speed, tick))
    }

    return (moved_count, total_count)
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  def try_spawn(spawn: SpawnAgent): Boolean =
    if (spawn.e.queue.can_spawn_now(spawn.dist)) {
      spawn.a.at = spawn.a.enter(spawn.e, spawn.dist)
      spawn.a.started_trip_at = tick
      agents :+= spawn.a
      true
    } else {
      false
    }
}

sealed trait Sim_Event {}
// TODO maybe have this not here, since a client could make these up?
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event {}

object Simulation {
  // temporary stats for fast-path tests.
  var did_fp = 0
  var didnt_fp = 0

  // maintain a log of the simulation here
  var map_fn: String = ""
  // every generator that was ever in existence
  val generators = new ListBuffer[Generator]()

  // start a new simulation
  def load(fn: String, with_geometry: Boolean): Simulation = {
    map_fn = fn
    return (new PlaintextReader(fn, with_geometry)).load_simulation
  }

  // TODO major limit: we don't yet encode spawning an army and waiting for
  // threads to compute routes.
  // TODO also encode time limit run_for?

  def save_log(fn: String) = {
    val out = new FileWriter(fn)
    out.write("<scenario map=\"%s\" seed=\"%d\">\n".format(map_fn, Util.seed))
    generators.foreach(g => out.write("  " + g.serialize + "\n"))
    out.write("</scenario>\n")
    out.close
  }

  def load_scenario(fn: String, with_geometry: Boolean): Simulation = {
    // TODO probably a better way to unpack than casting to string
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text

    var sim: Simulation = null

    new XMLEventReader(Source.fromFile(fn)).foreach(ev => ev match {
      case EvElemStart(_, "scenario", attribs, _) => {
        sim = load(get_attrib(attribs, "map"), with_geometry)
        Util.init_rng(get_attrib(attribs, "seed").toLong)
      }
      case EvElemStart(_, "generator", attribs, _) => {
        // huzzah for closures!
        def retriever(key: String) = get_attrib(attribs, key)
        Generator.unserialize(sim, retriever)
      }
      case _ =>
    })

    return sim
  }

  def policy_builder(enum: IntersectionPolicy.Value): (Intersection) => Policy = enum match {
    case IntersectionPolicy.NeverGo => (i: Intersection) => new NeverGoPolicy(i)
    case IntersectionPolicy.StopSign => (i: Intersection) => new StopSignPolicy(i)
    case IntersectionPolicy.Signal => (i: Intersection) => new SignalPolicy(i)
    case IntersectionPolicy.Reservation => (i: Intersection) => new ReservationPolicy(i)
  }

  def make_route(enum: RouteStrategy.Value, goal: DirectedRoad) = enum match {
    case RouteStrategy.StaticAstar => new StaticRoute(goal)
    case RouteStrategy.Drunken => new DrunkenRoute(goal)
    case RouteStrategy.DirectionalDrunk => new DirectionalDrunkRoute(goal)
    case RouteStrategy.DrunkenExplorer => new DrunkenExplorerRoute(goal)
  }

  private lazy val default_policy = policy_builder(IntersectionPolicy.withName(cfg.policy))
  def make_policy(i: Intersection) = default_policy(i)
}

object IntersectionPolicy extends Enumeration {
  type IntersectionPolicy = Value
  val NeverGo, StopSign, Signal, Reservation = Value
}

object RouteStrategy extends Enumeration {
  type RouteStrategy = Value
  val StaticAstar, Drunken, DirectionalDrunk, DrunkenExplorer = Value
}
