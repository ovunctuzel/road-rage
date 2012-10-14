// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import java.util.concurrent.{Executors, FutureTask, Callable}
import scala.collection.mutable.ListBuffer

import utexas.aorta.map.{Edge, DirectedRoad}

import utexas.aorta.{Util, cfg}

object Generator {
  // Maybe one less, to dedicate one core for simulation?
  val worker_pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  def shutdown = worker_pool.shutdown

  var next_id = 0

  def unserialize(sim: Simulation, params: (String => String)) = {
    val starts = params("starts").split(",").map(e => sim.edges(e.toInt))
    val ends = params("ends").split(",").map(e => sim.edges(e.toInt))

    // TODO a bit fixed based on the two types of generators we have, but adding
    // a new class doesn't entail too much new work
    // TODO serialize this info from the lambda?
    val route_builder = Simulation.route_builder("Static A*")
    val g = params("type") match {
      case "fixed"      => new FixedSizeGenerator(
        sim, starts, ends, params("total").toInt, route_builder
      )
      case "continuous" => new ContinuousGenerator(
        sim, starts, ends, params("spawn_every").toDouble, route_builder
      )
      case _            => throw new Exception(
        "Weird serialized generator with type " + params("type")
      )
    }

    def make_gen(): Unit = sim.add_gen(g)
    sim.schedule(params("time").toDouble, make_gen)
  }
}

abstract class Generator(sim: Simulation, desired_starts: Iterable[Edge],
                         ends: Iterable[Edge], route_builder: () => Route)
extends Ordered[Generator]
{
  val id = Generator.next_id
  Generator.next_id += 1
  val created_at = Agent.sim.tick   // needed for resimulation

  def compare(other: Generator) = other.id.compare(id)

  // Prune the desired_starts for road type and length. Arrays have random
  // access.
  val start_candidates = desired_starts.filter(e => e.queue.ok_to_spawn).toArray
  val end_candidates = ends.toArray

  def serialize_ls(ls: Array[Edge]) = ls.map(e => e.id).mkString(",")

  // there may be no task scheduled
  protected var pending = new ListBuffer[(Agent, Option[FutureTask[Stream[DirectedRoad]]])]

  // Returns new agents to try to spawn, or boolean means reap this genertor
  def run(): Either[List[Agent], Boolean]
  // For resimulation. Spit out something in XML.
  def serialize(): String

  def count_pending = pending.size

  def add_specific_agent(start: Edge, end: Edge) = {
    val route = route_builder()
    val a = new Agent(sim.next_id, sim, start, start.queue.safe_spawn_dist, route)

    // schedule whatever work the route needs done.
    // TODO the route won't get them exactly to end, just end's group of lanes.
    // close enough?
    route.request_route(start.directed_road, end.directed_road) match {
      case Some(task) => {
        val delayed = new FutureTask[Stream[DirectedRoad]](task)
        Generator.worker_pool.execute(delayed)
        pending += ((a, Some(delayed)))
      }
      case _ => {
        // don't schedule anything
        pending += ((a, None))
      }
    }
  }

  // This is a nonblocking poll, of course
  def poll(): List[Agent] = {
    // Even though worker threads will complete tasks out of order, can force
    // determinism by scanning through in order and stopping at the first that
    // isn't done
    val done = new ListBuffer[Agent]()
    while (pending.nonEmpty) {
      val a = pending.head
      val ready = a._2 match {
        case Some(task) => {
          if (task.isDone) {
            a._1.route.got_route(task.get)
            true
          } else {
            false
          }
        }
        case None => true
      }
      if (ready) {
        done += a._1
        pending = pending.tail
      } else {
        // stop immediately!
        return done.toList
      }
    }
    return done.toList
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

class FixedSizeGenerator(sim: Simulation, starts: Iterable[Edge],
                         ends: Iterable[Edge], total: Int,
                         route_builder: () => Route)
  extends Generator(sim, starts, ends, route_builder)
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

  override def serialize() = "<generator type=\"fixed\" time=\"%f\" starts=\"%s\" ends=\"%s\" total=\"%d\"/>".format(
    created_at, serialize_ls(start_candidates), serialize_ls(end_candidates), total
  )
}

class ContinuousGenerator(sim: Simulation, starts: Iterable[Edge],
                          ends: Iterable[Edge], spawn_every: Double,
                          route_builder: () => Route)
  extends Generator(sim, starts, ends, route_builder)
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

  override def serialize() = "<generator type=\"continuous\" time=\"%f\" starts=\"%s\" ends=\"%s\" spawn_every=\"%f\"/>".format(
    created_at, serialize_ls(start_candidates), serialize_ls(end_candidates),
    spawn_every
  )
}
