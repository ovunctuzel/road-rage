// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{MutableList, PriorityQueue, ListBuffer}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.{HashMap => MutableMap}
import java.io.FileWriter
import scala.io.Source

import utexas.aorta.map.{Graph, Road, Edge, Vertex, Turn, DirectedRoad}
import utexas.aorta.sim.policies.Phase

import utexas.aorta.{Util, Common, cfg, StateWriter, StateReader}
import utexas.aorta.analysis.{Heartbeat_Stat, Scenario_Stat, ReplayChecker}

// TODO take just a scenario, or graph and scenario?
class Simulation(val graph: Graph, val scenario: Scenario)
  extends ListenerPattern[Sim_Event] with AgentManager
{
  //////////////////////////////////////////////////////////////////////////////
  // State
  // (All transient, important state is in our constituent traits)

  // Added by a queue that does an in-place check and thinks there could be an
  // issue.
  val active_queues = new MutableSet[Queue]   // TODO list?
  // All intersections with agents in them.
  val active_intersections = new MutableSet[Intersection]
  // this represents total "real seconds"
  var tick: Double = 0

  // Track this for stats.
  private var last_real_time = 0.0
  private var steps_since_last_time = 0
  // Not private because these get incremented elsewhere.
  var ch_since_last_time = 0
  var astar_since_last_time = 0

  private val replay = new ReplayChecker(this, List(Common.focus).flatten.toSet)
  {
    override def difference(id: Int, expect: Double, actual: Double) {
      Util.log(s"At $tick, $id chose $actual rather than $expect")
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def setup() {
    Common.sim = this
    Phase.id = 0

    // TODO always do this, and forget this map/sim separation?
    graph.traversables.foreach(t => t.queue = new Queue(t))
    graph.vertices.foreach(v => v.intersection = scenario.make_intersection(v))

    // Are we starting for the first time?
    if (tick == 0.0) {
      Common.record(Scenario_Stat(scenario.map_fn, scenario.intersections))
      future_spawn ++= scenario.agents
    } else {
      future_spawn ++= scenario.agents.filter(_.birth_tick > tick)
    }
  }

  def serialize(w: StateWriter) {
    w.string(scenario.name)
    w.int(max_agent_id)
    w.double(tick)
    w.int(agents.size)
    agents.foreach(a => a.serialize(w))
    w.int(ready_to_spawn.size)
    ready_to_spawn.foreach(a => w.int(a.id))
    graph.traversables.foreach(t => t.queue.serialize(w))
    graph.vertices.foreach(v => v.intersection.policy.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def step() {
    tick += cfg.dt_s

    spawn_agents(tick)
    ready_to_spawn = ready_to_spawn.filter(a => !try_spawn(a))

    // If you wanted crazy speedup, disable all but agent stepping and
    // reacting. But that involves trusting that no simulation violations
    // could occur. ;)

    // Queues will lazily start_step, remembering their current state, when
    // they need to.

    // Move agents.
    var active_cnt = 0
    agents.foreach(a => {
      if (a.step) {
        active_cnt += 1
      }
      steps_since_last_time += 1
      //println(s"$tick: $a @ ${a.at} doing ${a.speed} due to ${a.target_accel}. ${a.old_lane} and ${a.lanechange_dist_left}, ${a.behavior.target_lane} LC")
      //println(s"  tickets: ${a.tickets}")
    })

    // Just check the ones we need to.
    active_queues.foreach(q => q.end_step)

    active_intersections.foreach(i => i.end_step)

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

    // Let intersections react to the new world. By doing this after agent
    // steps, we ensure the intersections' temporary state becomes firm.
    vertices.foreach(v => {
      v.intersection.policy.react_tick
    })
    
    // reset queues that need to be checked
    active_queues.clear

    // Record a heartbeat every 1.0s
    val now = System.currentTimeMillis
    if (now - last_real_time >= 1000.0) {
      val measurement = Heartbeat_Stat(
        active_cnt, agents.size, ready_to_spawn.size, tick,
        steps_since_last_time, ch_since_last_time, astar_since_last_time
      )
      Common.record(measurement)
      tell_listeners(EV_Heartbeat(measurement))
      last_real_time = now
      steps_since_last_time = 0
      ch_since_last_time = 0
      astar_since_last_time = 0
    }

    if ((tick / cfg.autosave_every).isValidInt && tick > 0.0) {
      savestate()
    }
    if ((tick / cfg.replay_freq).isValidInt && tick > 0.0) {
      replay.handle_tick()
    }
  }

  def multi_step(total_dt: Double) {
    val ticks = total_dt / cfg.dt_s
    Util.assert_eq(ticks.isValidInt, true)
    for (i <- Range(0, ticks.toInt)) {
      step()
    }
  }

  def terminate() {
    agents.foreach(a => a.terminate(interrupted = true))
    replay.done()
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  private def try_spawn(spawn: MkAgent): Boolean = {
    val e = graph.edges(spawn.start_edge)
    if (e.queue.can_spawn_now(spawn.start_dist)) {
      // Will we block anybody that's ready?
      val intersection = e.to.intersection
      val will_block =
        if (spawn.route.goal != e.directed_road.id)
          e.queue.all_agents.find(
            a => a.at.dist < spawn.start_dist && a.wont_block(intersection)
          ).isDefined || e.dont_block
        else
          false
      if (will_block) {
        return false
      } else {
        val a = spawn.make(this)
        if (Common.omit.getOrElse(-1) == a.id) {
          Util.log(s"$a would have been created, but omitting")
        } else {
          a.setup(graph.edges(spawn.start_edge), spawn.start_dist)
        }
        return true
      }
    } else {
      return false
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // Provide convenient shortcut to graph stuff
  def roads = graph.roads
  def vertices = graph.vertices
  def edges = graph.edges

  // TODO move time limit to scenarios?
  def done =
    (agents.isEmpty && ready_to_spawn.isEmpty && future_spawn.isEmpty) ||
    (Common.time_limit != -1.0 && tick > Common.time_limit)

  // Do some temporary debugging type thing from the UI
  def ui_debug() = {
    // Memoize!
    val demands = new MutableMap[Edge, Int]()

    def demand(e: Edge, seen: Set[Edge]): Int =
      if (demands.contains(e)) {
        demands(e)
      } else if (e.queue.is_full) {
        val total = e.queue.capacity + e.preds.map(
          pred => if (pred.queue.depends_on.getOrElse(null) != e) {
                    0
                  } else if (seen.contains(pred)) {
                    99999999  // <-- marker for gridlock :P
                  } else {
                    demand(pred, seen + pred)
                  }
        ).sum
        demands(e) = total
        total
      } else {
        val total = e.queue.slots_filled
        demands(e) = total
        total
      }

    for (e <- graph.edges) {
      demand(e, Set(e))
    }

    // Find queues with the most demand
    for (e <- graph.edges.map(e => (-demands(e), e)).sorted.take(10)) {
      Util.log(s"${e._2} has total demand ${-e._1}")
    }
  }

  def savestate(fn: String) {
    val t = Common.timer("savestating")
    val w = Util.writer(fn)
    serialize(w)
    w.done()
    Util.log(s"Savestated to $fn. It took ${t.so_far}s.")
  }

  def savestate() {
    savestate(
      scenario.name.replace("scenarios/", "scenarios/savestate_") + "_" +
      tick.toInt
    )
  }
}

object Simulation {
  // Calls sim.setup()!
  def unserialize(r: StateReader): Simulation = {
    val sim = Scenario.load(r.string).make_sim()
    // Do this before setup so we don't add the wrong spawners
    sim.max_agent_id = r.int
    sim.tick = r.double
    // Need so queues/intersections are set up.
    sim.setup()
    val num_agents = r.int
    for (i <- Range(0, num_agents)) {
      sim.insert_agent(Agent.unserialize(r, sim.graph))
    }
    val num_ready = r.int
    val ready_ids = Range(0, num_ready).map(_ => r.int).toSet
    sim.ready_to_spawn ++=
      sim.scenario.agents.filter(a => ready_ids.contains(a.id)).sortBy(_.birth_tick)
    for (t <- sim.graph.traversables) {
      Queue.unserialize(t.queue, r, sim)
    }
    for (v <- sim.graph.vertices) {
      Policy.unserialize(v.intersection.policy, r, sim)
    }
    return sim
  }
}

trait ListenerPattern[T] {
  //////////////////////////////////////////////////////////////////////////////
  // State

  private var listeners: List[(String, T => Any)] = Nil

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def tell_listeners(ev: T) = listeners.foreach(l => l._2(ev))
  def listen(tag: String, subscriber: T => Any) = {
    listeners :+= (tag, subscriber)
  }
  def unlisten(tag: String) = {
    listeners = listeners.filter(l => l._1 != tag)
  }
}

abstract class Sim_Event
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event
final case class EV_Heartbeat(heartbeat: Heartbeat_Stat) extends Sim_Event
final case class EV_Agent_Created(a: Agent) extends Sim_Event
final case class EV_Agent_Destroyed(a: Agent) extends Sim_Event

trait AgentManager {
  //////////////////////////////////////////////////////////////////////////////
  // State

  var agents: SortedSet[Agent] = TreeSet.empty[Agent]
  var ready_to_spawn = new ListBuffer[MkAgent]()
  protected var max_agent_id = -1
  val future_spawn = new PriorityQueue[MkAgent]()

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def spawn_agents(tick: Double) = {
    while (future_spawn.nonEmpty && tick >= future_spawn.head.birth_tick) {
      ready_to_spawn += future_spawn.dequeue
    }
  }

  def insert_agent(a: Agent) = {
    Util.assert_eq(agents.contains(a), false)
    agents += a
    max_agent_id = math.max(max_agent_id, a.id)
  }
  // This reserves the ID returned to the caller, so it'll never be reused.
  def next_agent_id(): Int = {
    max_agent_id += 1
    return max_agent_id
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // Only used by the UI, so is O(n) rather than a binary search.
  def get_agent(id: Int) = agents.find(a => a.id == id)
  def has_agent(a: Agent) = agents.contains(a)
}
