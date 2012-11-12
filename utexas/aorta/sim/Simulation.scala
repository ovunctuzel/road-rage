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

import utexas.aorta.map.{Graph, Road, Edge, Vertex, Ward, Turn, DirectedRoad}
import utexas.aorta.map.make.Reader
import utexas.aorta.sim.policies._

import utexas.aorta.{Util, cfg}
import utexas.aorta.analysis.{Stats, Total_Trip_Stat, Active_Agents_Stat,
                              Simulator_Speedup_Stat}

// This just adds a notion of agents
class Simulation(roads: Array[Road], edges: Array[Edge], vertices: Array[Vertex],
                 wards: List[Ward], special_ward: Ward)
  extends Graph(roads, edges, vertices, wards, special_ward)
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
  var agents: SortedSet[Agent] = new TreeSet[Agent]
  var ready_to_spawn: List[Agent] = Nil
  private var generators: SortedSet[Generator] = new TreeSet[Generator]
  private var generator_count = 0   // just for informational/UI purposes
  private var id_cnt = -1
  def num_generators = generators.size

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
    var changed = false
    var reap = new MutableSet[Generator]()
    generator_count = 0
    generators.foreach(g => {
      g.run match {
        case Left(newbies) => { ready_to_spawn ++= newbies }
        case Right(_)      => { reap += g }
      }
      generator_count += g.count_pending
      changed = true
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
    // TODO so we're introducing agents at possibly different times?!
    pre_step

    // This value is dt in simulation time, not real time
    dt_accumulated += dt_s * desired_sim_speed

    var moved_count = 0
    var total_count = 0

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step into exactly consistent/equal pieces, if needed.
    while (dt_accumulated >= cfg.dt_s) {
      dt_accumulated -= cfg.dt_s
      tick += cfg.dt_s

      // If you wanted crazy speedup, disable all but agent stepping and
      // reacting. But that involves trusting that no simulation violations
      // could occur. ;)

      // Queues will lazily start_step, remembering their current state, when
      // they need to.

      var active_cnt = 0
      agents.foreach(a => {
        if (a.step(cfg.dt_s)) {
          moved_count += 1
        }
        active_cnt += 1
      })
      total_count += active_cnt
      if (tick.toInt % 5 == 0) {
        Stats.record(Active_Agents_Stat(tick.toInt, active_cnt))
      }

      // Just check the ones we need to.
      active_queues.foreach(q => q.end_step)

      active_intersections.foreach(i => i.end_step)

      agents.foreach(a => {
        // reap the done agents
        if (a.react) {
          agents -= a
          Stats.record(Total_Trip_Stat(a.id, tick - a.started_trip_at, a.total_dist))
        }
      })
      
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
  def try_spawn(a: Agent): Boolean =
    if (a.start.queue.can_spawn_now(a.start_dist)) {
      a.at = a.enter(a.start, a.start_dist)
      a.started_trip_at = tick
      agents += a
      true
    } else {
      false
    }
}

sealed trait Sim_Event {}
// TODO maybe have this not here, since a client could make these up?
final case class EV_Signal_Change(reds: Set[Turn], greens: Set[Turn]) extends Sim_Event {}

object Simulation {
  // maintain a log of the simulation here
  var map_fn: String = ""
  // every generator that was ever in existence
  val generators = new ListBuffer[Generator]()

  // start a new simulation
  def load(fn: String): Simulation = {
    map_fn = fn
    return (new Reader(fn)).load_simulation
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

  def load_scenario(fn: String): Simulation = {
    // TODO probably a better way to unpack than casting to string
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text

    var sim: Simulation = null

    new XMLEventReader(Source.fromFile(fn)).foreach(ev => ev match {
      case EvElemStart(_, "scenario", attribs, _) => {
        sim = load(get_attrib(attribs, "map"))
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

  // TODO make it an enum
  def policy_builder(name: String): (Intersection) => Policy = name match {
    case "Never Go" => (i: Intersection) => new NeverGoPolicy(i)
    case "Stop Sign" => (i: Intersection) => new StopSignPolicy(i)
    case "Signal Cycle" => (i: Intersection) => new SignalCyclePolicy(i)
    case "Reservation" => (i: Intersection) => new ReservationPolicy(i)
    // TODO case _ => ???
  }

  def make_route(name: String, goal: DirectedRoad) = name match {
    case "Static A*" => new StaticRoute(goal)
    case "Drunken" => new DrunkenRoute(goal)
    case "Directional Drunk" => new DirectionalDrunkRoute(goal)
    case "Drunken Explorer" => new DrunkenExplorerRoute(goal)
    // TODO case _ => ???
  }
}
