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

import utexas.aorta.map.{Graph, Road, Edge, Vertex, Turn, DirectedRoad}

import utexas.aorta.{Util, Common, cfg}
import utexas.aorta.analysis.{Stats, Heartbeat_Stat, Scenario_Stat}

// TODO take just a scenario, or graph and scenario?
class Simulation(val graph: Graph, scenario: Scenario)
  extends ListenerPattern[Sim_Event] with EventQueue with AgentManager
  with VirtualTiming
{
  Common.sim = this

  // TODO always do this, and forget this map/sim separation?
  graph.traversables.foreach(t => t.queue = new Queue(t))
  graph.vertices.foreach(v => v.intersection = scenario.make_intersection(v))
  scenario.make_agents
  Stats.record(Scenario_Stat(scenario.map_fn, scenario.intersections))

  // Provide convenient shortcut to graph stuff
  def roads = graph.roads
  def vertices = graph.vertices
  def edges = graph.edges

  // TODO move time limit to scenarios?
  def done =
    (agents.isEmpty && ready_to_spawn.isEmpty && events_done) ||
    (Common.time_limit != -1.0 && tick > Common.time_limit)

  // Added by a queue that does an in-place check and thinks there could be an
  // issue.
  val active_queues = new MutableSet[Queue]   // TODO list?
  // All intersections with agents in them.
  val active_intersections = new MutableSet[Intersection]

  def pre_step() = {
    fire_events(tick)

    // Finally, introduce any new agents that are ready to spawn into the system
    ready_to_spawn = ready_to_spawn.filter(a => !try_spawn(a))
  }

  // Track this for stats.
  private var last_real_time = 0.0
  private var steps_since_last_time = 0

  def step(dt_s: Double) = {
    // This value is dt in simulation time, not real time
    dt_accumulated += dt_s * desired_sim_speed

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step into exactly consistent/equal pieces, if needed.
    while (dt_accumulated >= cfg.dt_s) {
      dt_accumulated -= cfg.dt_s
      tick += cfg.dt_s

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
          active_cnt += 1
        }
        steps_since_last_time += 1
      })

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
        reap.foreach(a => a.terminate())
        agents = agents.filter(a => !reap(a))
      }
      
      // reset queues that need to be checked
      active_queues.clear

      // Record a heartbeat every 1.0s
      val now = System.currentTimeMillis
      if (now - last_real_time >= 1000.0) {
        val measurement = Heartbeat_Stat(
          active_cnt, agents.size, ready_to_spawn.size, tick,
          steps_since_last_time
        )
        Stats.record(measurement)
        tell_listeners(EV_Heartbeat(measurement))
        last_real_time = now
        steps_since_last_time = 0
      }
    }
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  def try_spawn(spawn: SpawnAgent): Boolean =
    if (spawn.e.queue.can_spawn_now(spawn.dist)) {
      // Will we block anybody that's ready?
      val intersection = spawn.e.to.intersection
      val will_block =
        if (!spawn.a.route.done(spawn.e))
          spawn.e.queue.all_agents.find(
            agent => agent.at.dist < spawn.dist && agent.wont_block(intersection)
          ).isDefined || spawn.e.dont_block
        else
          false
      if (will_block) {
        false
      } else {
        val a = spawn.a
        a.at = a.enter(spawn.e, spawn.dist)
        insert_agent(a)
        spawn.e.queue.allocate_slot
        a.stat_memory = (tick, spawn.e.id, a.wallet.budget)
        true
      }
    } else {
      false
    }

  // Do some temporary debugging type thing from the UI
  def ui_debug() = {
    // Detect gridlock super hackishly.
    def demand(e: Edge): Int = try {
      if (e.queue.is_full)
        e.queue.capacity + e.preds.map(demand).sum
      else
        e.queue.capacity - e.queue.avail_slots
    } catch {
      // Mark appropriately :P
      case err: java.lang.StackOverflowError => Int.MaxValue
    }

    // Find queues with the most demand
    for (e <- graph.edges.sortBy(e => -demand(e)).take(10)) {
      Util.log(s"$e has total demand ${demand(e)}")
    }
  }
}

trait ListenerPattern[T] {
  private var listeners: List[(String, T => Any)] = Nil
  def tell_listeners(ev: T) = listeners.foreach(l => l._2(ev))
  def listen(tag: String, subscriber: T => Any) = {
    listeners :+= (tag, subscriber)
  }
  def unlisten(tag: String) = {
    listeners = listeners.filter(l => l._1 != tag)
  }
}
abstract class Sim_Event
// TODO maybe have this not here, since a client could make these up?
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event
final case class EV_Heartbeat(heartbeat: Heartbeat_Stat) extends Sim_Event

trait EventQueue {
  class Callback(val at: Double, val cb: () => Unit) extends Ordered[Callback] {
    // small weights first
    def compare(other: Callback) = other.at.compare(at)
    // TODO ties? determinism matters
  }

  private val events = new PriorityQueue[Callback]()

  def fire_events(tick: Double) = {
    while (events.nonEmpty && tick >= events.head.at) {
      val ev = events.dequeue
      ev.cb()
    }
  }

  def schedule(at: Double, callback: () => Unit) {
    events.enqueue(new Callback(at, callback))
  }

  def events_done = events.isEmpty
}

trait AgentManager {
  var agents: SortedSet[Agent] = TreeSet.empty[Agent]
  var ready_to_spawn = new ListBuffer[SpawnAgent]()
  private var max_agent_id = -1

  def insert_agent(a: Agent) = {
    // TODO assume this isn't a duplicate
    agents += a
    max_agent_id = math.max(max_agent_id, a.id)
  }
  // This reserves the ID returned to the caller, so it'll never be reused.
  def next_agent_id(): Int = {
    max_agent_id += 1
    return max_agent_id
  }

  // Only used by the UI, so is O(n) rather than a binary search.
  def get_agent(id: Int) = agents.find(a => a.id == id)
  def has_agent(a: Agent) = agents.contains(a)
}

trait VirtualTiming {
  // this represents total "real seconds"
  var tick: Double = 0
  // we only ever step with dt = cfg.dt_s, so we may have leftover.
  var dt_accumulated: Double = 0
  // The UI can tell us to make every one step count as multiple.
  var desired_sim_speed = 1.0
  var actual_sim_speed = 0.0

  // TODO cfg
  def slow_down(amount: Double = 0.5) = {
    desired_sim_speed = math.max(0.5, desired_sim_speed - amount)
  }
  def speed_up(amount: Double = 0.5) = {
    desired_sim_speed += amount
  }
}
