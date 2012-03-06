package utexas.sim

import java.util.concurrent.{Executors, FutureTask, Callable}

import utexas.map.{Edge, Traversable}

import utexas.{Util, cfg}

object Generator {
  // Maybe one less, to dedicate one core for simulation?
  val worker_pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  def shutdown = worker_pool.shutdown
}

abstract class Generator(sim: Simulation, desired_starts: List[Edge], val end_candidates: List[Edge])
{
  // Prune the desired_starts for road type and length
  val start_candidates = desired_starts.filter(e => sim.queues(e).ok_to_spawn)

  // there may be no task scheduled
  protected var pending: List[(Agent, Option[FutureTask[List[Traversable]]])] = Nil

  // Returns new agents to try to spawn, or boolean means reap this genertor
  def run(): Either[List[Agent], Boolean]

  def count_pending = pending.size

  def add_specific_agent(start: Edge, end: Edge) = {
    // TODO how to decide?!
    val route = new StaticRoute()
    //val route = new DrunkenRoute()
    //val route = new DirectionalDrunkRoute()
    //var route = new DrunkenExplorerRoute()
    val a = new Agent(sim.next_id, sim, start, sim.queues(start).safe_spawn_dist, route)

    // schedule whatever work the route needs done.
    route.request_route(start, end) match {
      case Some(task) => {
        val delayed = new FutureTask[List[Traversable]](task)
        Generator.worker_pool.execute(delayed)
        pending :+= (a, Some(delayed))
      }
      case _ => {
        // don't schedule anything
        pending :+= (a, None)
      }
    }
  }

  // This is a nonblocking poll, of course
  def poll(): List[Agent] = {
    val (done, still_pending) = pending.partition(
      a => a._2 match {
        case Some(task) => {
          if (task.isDone) {
            a._1.route.got_route(task.get)
            true
          } else {
            false
          }
        }
        case None       => true
      }
    )
    pending = still_pending
    return done.map(a => a._1)
  }

  // And the blocking poll
  def wait_for_all() = {
    var cnt = 0
    pending.foreach(a => {
      print("\r" + Util.indent + "Done with " + cnt + " routes")
      a._2 match {
        case Some(task) => task.get
        case None       =>
      }
      cnt += 1
    })
    Util.log("")
  }

  def create_and_poll(n: Int): List[Agent] = {
    for (i <- (0 until n)) {
      val start = Util.choose_rand[Edge](start_candidates)
      val end = Util.choose_rand[Edge](end_candidates)
      add_specific_agent(start, end)
    }
    return poll
  }
}

// TODO there's further refactorings that should happen between these two...
// Fixed is really a degenerate case of continuous.

class FixedSizeGenerator(sim: Simulation, starts: List[Edge], ends: List[Edge], total: Int)
  extends Generator(sim, starts, ends)
{
  var num_to_spawn = total

  override def run(): Either[List[Agent], Boolean] = {
    if (num_to_spawn == 0 && pending.isEmpty) {
      // this generator's done.
      return Right(true)
    } else if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      return Right(true)
    } else {
      val n = num_to_spawn
      num_to_spawn = 0
      return Left(create_and_poll(n))
    }
  }
}

class ContinuousGenerator(sim: Simulation, starts: List[Edge], ends: List[Edge],
                          spawn_every: Double) extends Generator(sim, starts, ends)
{
  var last_tick = Agent.sim.tick
  var accumulated_time = 0.0

  override def run(): Either[List[Agent], Boolean] = {
    accumulated_time += Agent.sim.tick - last_tick
    last_tick = Agent.sim.tick

    if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      return Right(true)
    } else {
      var new_agents: List[Agent] = Nil
      while (accumulated_time >= spawn_every) {
        accumulated_time -= spawn_every
        new_agents ++= create_and_poll(1)
      }
      return Left(new_agents)
    }
  }
}
