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

import utexas.aorta.{Util, cfg}
import utexas.aorta.analysis.{Stats, Active_Agents_Stat, Simulator_Speedup_Stat}

class Simulation(roads: Array[Road], edges: Array[Edge],
                 vertices: Array[Vertex], scenario: Scenario)
  extends Graph(roads, edges, vertices, scenario.map_fn)
  with ListenerPattern[Sim_Event] with EventQueue with AgentManager
  with VirtualTiming
{
  Agent.sim = this  // let them get to us
  // TODO always do this, and forget this map/sim separation?
  traversables.foreach(t => t.queue = new Queue(t))
  vertices.foreach(v => v.intersection = scenario.make_intersection(v))
  scenario.make_agents

  // TODO if we have a viral event that's making more and more agents, then we
  // need a time_limit
  def done = agents.isEmpty && ready_to_spawn.isEmpty && events_done

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

  // TODO just for stats
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
      insert_agent(spawn.a)
      true
    } else {
      false
    }
}

trait ListenerPattern[T] {
  private var listeners: List[T => Any] = Nil
  def tell_listeners(ev: T) = listeners.foreach(l => l(ev))
  def listen(subscriber: T => Any) = {
    listeners :+= subscriber
  }
}
sealed trait Sim_Event {}
// TODO maybe have this not here, since a client could make these up?
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event {}

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

  def describe_agents = s"${agents.size} / ${ready_to_spawn.size}"
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
  def number_of_ticks = (tick / cfg.dt_s).toInt // TODO if dt_s changes, breaks
  // we only ever step with dt = cfg.dt_s, so we may have leftover.
  var dt_accumulated: Double = 0
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
}
