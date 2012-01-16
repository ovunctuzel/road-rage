package utexas.sim

import scala.collection.mutable.MutableList

import utexas.map.{Graph, Road, Edge, Vertex, Ward}
import utexas.map.make.Reader

import utexas.{Util, cfg}

// This just adds a notion of agents
class Simulation(roads: List[Road], edges: List[Edge], vertices: List[Vertex],
                 wards: List[Ward], special_ward: Ward, width: Double, height: Double)
  extends Graph(roads, edges, vertices, wards, special_ward, width, height)
{
  /////////// Agent management
  Agent.sim = this  // let them get to us
  Util.log("Creating queues and intersections for collision handling...")
  val queues = traversables.map(t => t -> new Queue(t)).toMap
  val intersections = vertices.map(v => v -> new Intersection(v)).toMap

  // Should be a stable order.
  def agents() = traversables.map(t => queues(t)).flatMap(q => q.agents)

  def add_agent(start: Edge): Agent = {
    val a = new Agent(agents.size, this, start)
    // Throw an exception if we couldn't find anything...
    a.go_to(random_edge(min_len = 0).get)
    return a
  }

  // There might be nothing satisfying the constraints.
  final def random_edge(except: Set[Edge] = Set(), spawning: Boolean = false,
                        min_len: Double = 1.0): Option[Edge] =
  {
    def ok(e: Edge) = (!except.contains(e)
                   && e.road.road_type == "residential"
                   && e.length > min_len
                   && (!spawning || queues(e).ok_to_spawn))

    val candidates = edges.filter(ok)
    if (candidates.size > 0) {
      return Some(Util.choose_rand(candidates))
    } else {
      return None
    }
  }

  ////////////// Timing

  // this represents "real seconds", so all velocity formulas work out normally
  var tick: Double = 0
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
  Util.log("making")
  new Thread {
    override def run(): Unit = {
      while (true) {
        val start = System.currentTimeMillis
        // we should fire about 10x/second
        Thread.sleep(100)
        if (running) {
          step(System.currentTimeMillis - start)
        }
      }
    }
  }.start
  Util.log("done")

  def step(dt_ms: Long) = {
    // This value is dt in simulation time, not real time
    var dt_s = dt_ms / 1000.0 * time_speed

    // Agents can't react properly in the presence of huge time-steps. So chop
    // up this time-step, if needed.
    while (dt_s > 0.0) {
      val this_step = math.min(dt_s, cfg.max_dt)
      dt_s -= this_step
      tick += this_step

      queues.values.foreach(q => q.start_step)
      agents.foreach(a => a.step(this_step))
      queues.values.foreach(q => q.end_step)
      intersections.values.foreach(i => i.end_step)

      // Let agents react.
      agents.foreach(a => a.react)
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
