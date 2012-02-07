package utexas.sim

import scala.actors.Futures.future
import scala.actors.Future

import utexas.map.{Edge, Traversable}

import utexas.Util

// TODO abstract class generator. implementations could use blocking routing,
// futures, ...  or generators dispatch to another mechanism for routefinding,
// altho that might not be clean.

// TODO this will really be 'blocking ASAP' generator
abstract class Generator(sim: Simulation, desired_starts: List[Edge], val end_candidates: List[Edge])
{
  // Prune the desired_starts for road type and length
  val start_candidates = desired_starts.filter(e => sim.queues(e).ok_to_spawn)

  protected var pending: List[(Agent, Future[List[Traversable]])] = Nil

  // Returns new agents to try to spawn, or boolean means reap this genertor
  def run(): Either[List[Agent], Boolean]

  def add_specific_agent(start: Edge, end: Edge) = {
    val a = new Agent(sim.next_id, sim, start, sim.queues(start).safe_spawn_dist)
    // TODO the work we do here, if any, actually depends on the behavior.

    // schedule some work.
    pending :+= (a, future { Util.log("oh god more"); sim.pathfind_astar(start, end) })
  }

  // This is a nonblocking poll, of course
  def poll(): List[Agent] = {
    val (done, still_pending) = pending.partition(a => a._2.isSet)
    pending = still_pending
    done.foreach(a => a._1.behavior.give_route(a._2()))
    Util.log(done.size + " were done, " + pending.size + " left")
    return done.map(a => a._1)
  }

  // TODO makes sense for delayed computation ones.
  def wait_for_all() = {
  }
}

class FixedSizeGenerator(sim: Simulation, starts: List[Edge], ends: List[Edge], total: Int)
  extends Generator(sim, starts, ends)
{
  var num_to_spawn = total

  override def run(): Either[List[Agent], Boolean] = {
    if (num_to_spawn == 0 && pending.isEmpty) {
      Util.log("genertor is done")
      // this generator's done.
      return Right(true)
    } else if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      return Right(true)
    } else {
      // First create new agents
      for (i <- (0 until num_to_spawn)) {
        val start = Util.choose_rand[Edge](start_candidates)
        val end = Util.choose_rand[Edge](end_candidates)
        add_specific_agent(start, end)
        num_to_spawn -= 1
      }

      // Then return anybody who's ready.
      return Left(poll)
    }
  }
}

//class ConstantGenerator() extends Generator() {
//}
