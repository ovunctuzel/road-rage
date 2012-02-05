package utexas.sim

import scala.annotation.tailrec
import scala.collection.mutable.MutableList
import scala.collection.mutable.{HashSet => MutableSet}

import utexas.map.{Graph, Road, Edge, Vertex, Ward, Turn}
import utexas.map.make.Reader

import utexas.{Util, cfg}

// This just adds a notion of agents
class Simulation(roads: List[Road], edges: List[Edge], vertices: List[Vertex],
                 wards: List[Ward], special_ward: Ward, width: Double, height: Double,
                 xOff: Double, yOff: Double, scale: Double)
  extends Graph(roads, edges, vertices, wards, special_ward, width, height, xOff, yOff, scale)
{
  /////////// Agent management
  Agent.sim = this  // let them get to us
  Util.log("Creating queues and intersections for collision handling...")
  val queues = traversables.map(t => t -> new Queue(t)).toMap
  val intersections = vertices.map(v => v -> new Intersection(v)).toMap

  // It's expensive to go through every one of these without reason every
  // time. This banks on the fact that the number of agents is smaller, and
  // they're not evenly distributed through a huge map.
  def active_queues(): Set[Queue] = agents.map(a => queues(a.at.on)).toSet
  def active_intersections(): Set[Intersection] = agents.flatMap(
    a => a.at.on match {
      case t: Turn => Some(intersections(t.vert))
      case _       => None
    }).toSet

  val agents = new MutableSet[Agent]
  // Below was a stable order and less maintenance, but it's SLOW
  //def agents() = traversables.map(t => queues(t)).flatMap(q => q.agents)

  def add_agent(start: Edge): Agent = {
    val a = new Agent(agents.size, this, start)
    agents += a
    // Throw an exception if we couldn't find anything...
    a.go_to(random_edge(min_len = 0).get)
    return a
  }

  // There might be nothing satisfying the constraints.
  final def random_edge(except: Set[Edge] = Set(), spawning: Boolean = false,
                        min_len: Double = 1.0, dynamicSpawning: Boolean = false): Option[Edge] =
  {
    def ok(e: Edge) = (!except.contains(e)
                   && e.road.road_type == "residential" //TODO Really?  There's like 100+ road types...
                   && e.length > min_len
                   && (!spawning || queues(e).ok_to_spawn(dynamicSpawning)))

    val candidates = edges.filter(ok)
    if (candidates.size > 0) {
      return Some(Util.choose_rand(candidates))
    } else {
      return None
    }
  }

  @tailrec final def spawn_army(i: Int, total: Int): Unit = {
    print("\r" + Util.indent + "Spawning agent " + i + "/" + total)
    random_edge(spawning = true) match {
      case Some(e) => {
        add_agent(e)
        if (i != total) {
          spawn_army(i + 1, total)
        } else {
          Util.log("")
        }
      }
      case None => {
        Util.log("\n... No room left!")
      }
    }
  }

  ////////////// Timing

  // this represents total "real seconds", so all velocity formulas work out
  // normally. so far this is also just for external UIness, nothing internal
  // cares.
  var tick: Double = 0
  // we only ever step with dt = cfg.dt_s, so we may have leftover.
  private var dt_accumulated: Double = 0
  // we can pause
  var running = false
  val listeners = new MutableList[Simulation_Listener]()
  // WE CAN GO FASTER
  var time_speed = 1.0

  // TODO cfg
  def slow_down() = {
    time_speed = math.max(0.1, time_speed - 0.1)
  }
  def speed_up() = {
    time_speed += 0.1
  }

  // Fire steps every once in a while
  // TODO this ignores us exiting the swing app, then
  def start_timer = {
    new Thread {
      override def run(): Unit = {
        while (true) {
          val start = System.currentTimeMillis
          // we should fire about 10x/second. optimal/useful rate is going to be
          // related to time_speed and cfg.dt_s   TODO
          Thread.sleep(100)
          if (running) {
            step((System.currentTimeMillis - start).toDouble / 1000.0)
          }
        }
      }
    }.start
  }

  def step(dt_s: Double) = {
    // This value is dt in simulation time, not real time
    val this_time = dt_s * time_speed
    dt_accumulated += this_time
    tick += this_time

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step into exactly consistent/equal pieces, if needed.
    while (dt_accumulated >= cfg.dt_s) {
      dt_accumulated -= cfg.dt_s

      // If you wanted crazy speedup, disable all but agent stepping and
      // reacting. But that involves trusting that no simulation violations
      // could occur. ;)

      // we only sortBy id to get determinism so we can repeat runs.
      // TODO maintain a sorted set or something instead
      val agents_by_id = agents.toList.sortBy(a => a.id)

      active_queues.foreach(q => q.start_step)

      agents_by_id.foreach(a => {
        // reap the done agents
        if (a.step(cfg.dt_s)) {
          agents -= a
        }
      })

      active_queues.foreach(q => q.end_step)

      active_intersections.foreach(i => i.end_step)

      agents_by_id.foreach(a => a.react)
    }

    // listener (usually UI) callbacks
    listeners.foreach(l => l.ev_step)
  }

  def pause()  = { running = false }
  def resume() = { running = true }
}

object Simulation {
  def load(fn: String) = (new Reader(fn)).load_simulation
}

abstract class Simulation_Listener {
  def ev_step()
}
