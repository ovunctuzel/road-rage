package utexas.sim

import java.util.concurrent.{Executors, FutureTask}

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

  protected var pending: List[(Agent, FutureTask[List[Traversable]])] = Nil

  // Returns new agents to try to spawn, or boolean means reap this genertor
  def run(): Either[List[Agent], Boolean]

  def count_pending = pending.size

  def add_specific_agent(start: Edge, end: Edge) = {
    // TODO how to decide?!
    //val route = new StaticRoute()
    //val route = new DrunkenRoute()
    val route = new DirectionalRoute()
    val a = new Agent(sim.next_id, sim, start, sim.queues(start).safe_spawn_dist, route)

    // schedule whatever work the route needs done.
    val delayed = new FutureTask[List[Traversable]](route.request_route(start, end))
    Generator.worker_pool.execute(delayed)
    pending :+= (a, delayed)
  }

  // This is a nonblocking poll, of course
  def poll(): List[Agent] = {
    val (done, still_pending) = pending.partition(a => a._2.isDone)
    pending = still_pending
    done.foreach(a => a._1.route.got_route(a._2.get()))
    return done.map(a => a._1)
  }

  // And the blocking poll
  def wait_for_all() = pending.foreach(a => a._2.get())

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
                          rate: Double, num: Int) extends Generator(sim, starts, ends)
{
  val num_per_tick = ((rate / cfg.dt_s) * num).toInt

  override def run(): Either[List[Agent], Boolean] = {
    if (num_per_tick == 0) {
      // TODO figure out how to spawn per 2 ticks, or whatever
      Util.log("Generator won't spawn anything! (Choose more agents per time)")
      return Right(true)
    } else if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      return Right(true)
    } else {
      // TODO fine tune this policy a bit. basically, dont schedule TOO many
      // routes to be found if we can't keep up computationally.
      if (pending.size >= num_per_tick * 2) {
        return Left(poll)
      } else {
        return Left(create_and_poll(num_per_tick))
      }
    }
  }
}
