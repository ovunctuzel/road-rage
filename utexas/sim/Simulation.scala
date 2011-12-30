package utexas.sim

import scala.collection.mutable.MutableList
// don't use swing timers; we're not tied to GUI
import java.util.{Timer,TimerTask}
// TODO do we want a dependency on continuations? we'll see...
import scala.util.continuations.{shift, reset}

import utexas.map.{Graph, Road, Edge, Vertex, Ward}
import utexas.map.make.Reader

import utexas.Util

// This just adds a notion of agents
class Simulation(roads: List[Road], edges: List[Edge], vertices: List[Vertex],
                 wards: List[Ward], special_ward: Ward, width: Double, height: Double)
  extends Graph(roads, edges, vertices, wards, special_ward, width, height)
{
  /////////// Agent management
  Agent.sim = this  // let them get to us
  val queues = edges.map(e => e -> new Queue_of_Agents(e)).toMap

  var agents = Set[Agent]()

  def add_agent(start: Edge): Agent = {
    // TODO give it more life soon
    val a = new Agent(agents.size, this)
    agents += a
    a.spawn_at(start)
    a.go_to(random_edge_except(Set()))
    return a
  }

  ////////////// Timing

  // this represents "real seconds", so all velocity formulas work out normally
  var tick: Double = 0
  // we can pause
  var running = false
  val listeners = new MutableList[Simulation_Listener]()

  // an awesome example of continuations from
  // http://www.scala-lang.org/node/2096
  private val timer = new Timer()

  def sleep(delay: Int) = shift { k: (Unit => Unit) =>
    // TODO gotta read the docs more, but I think all timer events are fired
    // from one thread
    timer.schedule(new TimerTask {
      def run() = k() // in a real program, we'd execute k on a thread pool
    }, delay)
  }

  reset {
    while (true) {
      val start = System.currentTimeMillis
      // we should fire about 10x/second
      sleep(100)
      if (running) {
        step(System.currentTimeMillis - start)
      }
    }
  }

  def step(dt_ms: Long) = {
    val time_speed = 1.0  // TODO cfg :P
    tick += dt_ms / 1000.0 * time_speed

    // reap the agents that are done. true indicates alive.
    agents = agents.filter(a => a.step(dt_ms, tick))

    // inform listeners (the UI, usually)
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
