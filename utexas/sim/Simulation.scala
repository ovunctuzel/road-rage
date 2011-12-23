package utexas.sim

import scala.collection.mutable.MutableList
// don't use swing timers; we're not tied to GUI
import java.util.{Timer,TimerTask}
// TODO do we want a dependency on continuations? we'll see...
import scala.util.continuations.{shift, reset}

import utexas.map.{Graph, Road, Edge, Vertex}
import utexas.map.make.Reader

import utexas.Util.{log, log_push, log_pop, choose_rand}

// TODO better name, i promise

// This just adds a notion of agents
class Simulation(roads: List[Road], override val edges: List[Edge with Queue_of_Agents],
                 vertices: List[Vertex], width: Double, height: Double)
  extends Graph(roads, edges, vertices, width, height)
{
  /////////// Agent management

  val agents = new MutableList[Agent]()

  def add_agent(start: Edge with Queue_of_Agents): Agent = {
    // TODO give it more life soon
    val a = new Agent(agents.size, new IdleBehavior())
    agents += a
    a.spawn_at(start)
    return a
  }

  // TODO some sort of thread that's advancing a timer
  // TODO a step(dt) that gives everybody a chance, and reaps?
  // TODO pause toggling

  ////////////// Timing

  // this represents "real seconds", so all velocity formulas work out normally
  var tick: Double = 0
  // we can pause
  var running = false

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
      // we should fire about 2x/second
      sleep(500)
      if (running) {
        step(System.currentTimeMillis - start)
      }
    }
  }

  def step(dt_ms: Long) = {
    val time_speed = 1.0  // TODO cfg :P
    tick += dt_ms / 1000.0 * time_speed

    // TODO reaping dead ones and stuff
    agents.foreach(a => a.step(dt_ms, tick))

    // inform listeners (the UI, usually)
    listeners.foreach(l => l.ev_step)
  }

  def pause()  = { running = false }
  def resume() = { running = true }

  /////////////// Callbacks
  val listeners = new MutableList[Simulation_Listener]()

  ////////////// Type erasure fixes

  // TODO this is HIDEOUS, but i dunno how to preserve the edge with the trait!
  override def random_edge_except(except: Set[Edge]): Edge with Queue_of_Agents = {
    val min_len = 1.0 // TODO cfg. what unit is this in?
    val e = choose_rand(edges)
    if (!except(e) && e.road.road_type == "residential" && e.length > min_len) {
      return e
    } else {
      return random_edge_except(except)
    }
  }
}

object Simulation {
  def load(fn: String) = (new Reader(fn)).load_simulation
}

abstract class Simulation_Listener {
  def ev_step()
}
