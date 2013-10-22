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

import utexas.aorta.map.{Graph, Road, Edge, Vertex, Turn}
import utexas.aorta.sim.policies.Phase

import utexas.aorta.ui.ReplayDiffScheme
import utexas.aorta.common.{Util, Common, cfg, StateWriter, StateReader, Flags, AgentID}
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

  private lazy val replay =
    if (Flags.boolean("--replay", false))
      new ReplayChecker(this, List(Flags.int("--focus")).flatten.map(new AgentID(_)).toSet) {
        override def difference(id: AgentID, expected: Double, actual: Double) {
          // TODO this is a bit messy, reaching over to the UI... bcast an event.
          ReplayDiffScheme.add_delta(id, actual - expected)
        }
      }
    else
      null

  private lazy val time_limit = Flags.double("--time_limit", Double.MaxValue)
  private lazy val omit = new AgentID(Flags.int("--omit", -1))
  private lazy val should_savestate = Flags.boolean("--savestate", true)

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def setup(): Simulation = {
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
    return this
  }

  def serialize(w: StateWriter) {
    w.string(scenario.name)
    w.double(tick)
    w.int(finished_count)
    w.int(agents.size)
    agents.foreach(a => a.serialize(w))
    w.int(ready_to_spawn.size)
    ready_to_spawn.foreach(a => w.int(a.id.int))
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
      finished_count += reap.size
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

    if (should_savestate && (tick / cfg.autosave_every).isValidInt && tick > 0.0) {
      savestate()
    }
    if (replay != null && (tick / cfg.replay_freq).isValidInt && tick > 0.0) {
      replay.handle_tick()
    }

    tell_listeners(EV_Step(tick))
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
    if (replay != null) {
      replay.done()
    }
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  private def try_spawn(spawn: MkAgent): Boolean = {
    val e = graph.edges(spawn.start_edge.int)
    if (e.queue.can_spawn_now(spawn.start_dist)) {
      // Will we block anybody that's ready?
      val intersection = e.to.intersection
      val will_block =
        if (graph.edges(spawn.route.goal.int).directed_road != e.directed_road)
          e.queue.all_agents.find(
            a => a.at.dist < spawn.start_dist && a.wont_block(intersection)
          ).isDefined || e.dont_block
        else
          false
      if (will_block) {
        return false
      } else {
        val a = spawn.make(this)
        if (omit == a.id) {
          Util.log(s"$a would have been created, but omitting")
        } else {
          a.setup(graph.edges(spawn.start_edge.int), spawn.start_dist)
        }
        tell_listeners(EV_AgentSpawned(a))
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
    (tick > time_limit)

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
    // TODO figure out lexicographic tuple ordering
    /*for (e <- graph.edges.map(e => (-demands(e), e)).sorted.take(10)) {
      Util.log(s"${e._2} has total demand ${-e._1}")
    }*/
  }

  def savestate(fn: String): String = {
    val t = Common.timer("savestating")
    val w = Util.writer(fn)
    serialize(w)
    w.done()
    Util.log(s"Savestated to $fn. It took ${t.so_far}s.")
    return fn
  }

  def savestate(): String = {
    return savestate(
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
    sim.tick = r.double
    sim.finished_count = r.int
    // Need so queues/intersections are set up.
    sim.setup()
    val num_agents = r.int
    for (i <- Range(0, num_agents)) {
      sim.insert_agent(Agent.unserialize(r, sim.graph))
    }
    val num_ready = r.int
    val ready_ids = Range(0, num_ready).map(_ => new AgentID(r.int)).toSet
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

  private val listeners = new ListBuffer[(String, T => Any)]()

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def tell_listeners(ev: T) = listeners.foreach(l => l._2(ev))
  def listen(tag: String, subscriber: T => Any) = {
    listeners += ((tag, subscriber))
  }
  def unlisten(tag: String) = {
    listeners --= listeners.filter(l => l._1 != tag)
  }
}

abstract class Sim_Event
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event
final case class EV_Heartbeat(heartbeat: Heartbeat_Stat) extends Sim_Event
final case class EV_AgentSpawned(a: Agent) extends Sim_Event
final case class EV_Step(tick: Double) extends Sim_Event

trait AgentManager {
  //////////////////////////////////////////////////////////////////////////////
  // State

  var agents: SortedSet[Agent] = TreeSet.empty[Agent]
  var ready_to_spawn = new ListBuffer[MkAgent]()
  val future_spawn = new PriorityQueue[MkAgent]()
  var finished_count = 0

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
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  // Only used by the UI, so is O(n) rather than a binary search.
  def get_agent(id: Int) = agents.find(a => a.id.int == id)
  def has_agent(a: Agent) = agents.contains(a)
}

// A map from agent to something else, which supports defaults and knows when
// agents are created or destroyed.
class AgentMap[T](default: T) {
  private val mapping = new MutableMap[AgentID, T]()
  AgentMap.maps += this

  def get(id: AgentID): T = mapping.getOrElse(id, default)
  def get(a: Agent): T = get(a.id)
  def put(id: AgentID, value: T) {
    mapping(id) = value
  }
  def values = mapping.values

  def when_created(a: Agent) {}
  def destroy(a: Agent) {
    mapping.remove(a.id)
  }
}

object AgentMap {
  val maps = new MutableList[AgentMap[_ <: Any]]()
}
