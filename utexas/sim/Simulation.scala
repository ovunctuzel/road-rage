package utexas.sim

import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.MutableList
import scala.collection.mutable.{HashSet => MutableSet}

import utexas.map.{Graph, Road, Edge, Vertex, Ward, Turn}
import utexas.map.make.Reader

import utexas.{Util, cfg}

// This just adds a notion of agents
class Simulation(roads: List[Road], edges: List[Edge], vertices: List[Vertex],
                 wards: List[Ward], special_ward: Ward)
  extends Graph(roads, edges, vertices, wards, special_ward)
{
  /////////// Agent management
  Agent.sim = this  // let them get to us
  Util.log("Creating queues and intersections for collision handling...")
  val queues = traversables.map(t => t -> new Queue(t)).toMap
  val intersections = vertices.map(v => v -> new Intersection(v)).toMap
  var agents: SortedSet[Agent] = new TreeSet[Agent]
  var ready_to_spawn: List[Agent] = Nil
  val generators = new MutableSet[Generator]
  private var generator_count = 0   // just for informational/UI purposes
  private var id_cnt = -1

  // Just for debug.
  var debug_agent: Option[Agent] = None

  def next_id(): Int = {
    id_cnt += 1
    return id_cnt
  }

  // Be sure to call pre_step at least once to poke all the generators
  def done = agents.isEmpty && ready_to_spawn.isEmpty && generator_count == 0

  def shutdown = Generator.shutdown

  def describe_agents = "%d / %d / %d (%d generators)".format(
    agents.size, ready_to_spawn.size, generator_count, generators.size
  )

  // It's expensive to go through every one of these without reason every
  // time. This banks on the fact that the number of agents is smaller, and
  // they're not evenly distributed through a huge map.
  def active_queues(): Set[Queue] = agents.map(a => queues(a.at.on)).toSet
  def active_intersections(): Set[Intersection] = agents.flatMap(
    a => a.at.on match {
      case t: Turn => Some(intersections(t.vert))
      case _       => None
    }).toSet

  // Just a convenience method.
  def spawn_army(total: Int) = {
    generators += new FixedSizeGenerator(this, edges, edges, total)
  }

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
  var time_speed = 1.0

  // TODO cfg
  def slow_down() = {
    time_speed = math.max(0.5, time_speed - 0.5)
  }
  def speed_up() = {
    time_speed += 0.5
  }

  def pre_step() = {
    // First, give generators a chance to introduce more agents into the system
    var reap = new MutableSet[Generator]()
    generator_count = 0
    generators.foreach(g => {
      g.run match {
        case Left(newbies) => { ready_to_spawn ++= newbies }
        case Right(_)      => { reap += g }
      }
      generator_count += g.count_pending
    })
    generators --= reap

    // Then, introduce any new agents that are ready to spawn into the system
    ready_to_spawn = ready_to_spawn.filter(a => !try_spawn(a))
  }

  def step(dt_s: Double) = {
    pre_step

    // This value is dt in simulation time, not real time
    val this_time = dt_s * time_speed
    dt_accumulated += this_time
    tick += this_time

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step into exactly consistent/equal pieces, if needed.
    while (dt_accumulated >= cfg.dt_s) {
      //val t0 = Util.timer("whole step")
      //Util.log_push
      dt_accumulated -= cfg.dt_s

      // If you wanted crazy speedup, disable all but agent stepping and
      // reacting. But that involves trusting that no simulation violations
      // could occur. ;)

      //val t2 = Util.timer("queue start")
      active_queues.foreach(q => q.start_step)
      //t2.stop

      //val t3 = Util.timer("agent step")
      agents.foreach(a => a.step(cfg.dt_s))
      //t3.stop

      //val t4 = Util.timer("queue stop")
      active_queues.foreach(q => q.end_step)
      //t4.stop

      //val t5 = Util.timer("vert check")
      active_intersections.foreach(i => i.end_step)
      //t5.stop

      //val t6 = Util.timer("react")
      agents.foreach(a => {
        // reap the done agents
        if (a.react) {
          agents -= a
        }
      })
      //t6.stop
      
      //Util.log_pop
      //t0.stop
    }
  }

  // True if we've correctly promoted into real agents. Does the work of
  // spawning as well.
  def try_spawn(a: Agent): Boolean = {
    if (queues(a.start).can_spawn_now(a.start_dist)) {
      a.at = a.enter(a.start, a.start_dist)
      agents += a
      return true
    } else {
      return false
    }
  }
}

object Simulation {
  def load(fn: String) = (new Reader(fn)).load_simulation
}
