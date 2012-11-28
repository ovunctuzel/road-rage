// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import java.util.concurrent.{Executors, FutureTask, Callable}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Queue => QueueADT}

import utexas.aorta.map.Edge

import utexas.aorta.{Util, cfg}

object Generator {
  // Maybe one less, to dedicate one core for simulation?
  val worker_pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  def shutdown = worker_pool.shutdown

  var next_id = 0

  def unserialize(sim: Simulation, params: (String => String)) = {
    val route = params("route_type")
    lazy val starts = params("starts").split(",").map(e => sim.edges(e.toInt))
    lazy val ends = params("ends").split(",").map(e => sim.edges(e.toInt))

    // TODO a bit fixed based on the three types of generators we have, but
    // adding a new class doesn't entail too much new work
    val g = params("type") match {
      case "fixed"      => new FixedSizeGenerator(
        sim, starts, ends, params("total").toInt, route
      )
      case "continuous" => new ContinuousGenerator(
        sim, starts, ends, params("spawn_every").toDouble, route
      )
      case "specific" => new SpecificGenerator(
        sim, route, params("positions").split(",").map(
          p => SpecificGenerator.parse(p, sim)
        )
      )
      case _            => throw new Exception(
        "Weird serialized generator with type " + params("type")
      )
    }

    def make_gen(): Unit = sim.add_gen(g)
    sim.schedule(params("time").toDouble, make_gen)
  }
}

abstract class Generator(sim: Simulation, route_type: String)
extends Ordered[Generator]
{
  val id = Generator.next_id
  Generator.next_id += 1
  val created_at = Agent.sim.tick   // needed for resimulation

  def compare(other: Generator) = other.id.compare(id)

  def serialize_ls(ls: Array[Edge]) = ls.map(e => e.id).mkString(",")

  // there may be no task scheduled
  protected val pending = new QueueADT[(SpawnAgent, Option[FutureTask[Unit]])]

  // Returns new agents to try to spawn, or boolean means reap this genertor
  def run(): Either[List[SpawnAgent], Boolean]
  // For resimulation. Spit out something in XML.
  def serialize(): String

  def count_pending = pending.size

  def add_specific_agent(start: Edge, end: Edge, start_dist: Double) = {
    val route = Simulation.make_route(route_type, end.directed_road)
    val a = new SpawnAgent(new Agent(sim.next_id, route), start, start_dist)

    // Agents get to the directed road of the requested ending. Good enough?
    route.compute_route match {
      case Some(task) => {
        // Schedule whatever work the route needs done.
        val delayed = new FutureTask[Unit](task)
        Generator.worker_pool.execute(delayed)
        pending.enqueue((a, Some(delayed)))
      }
      case _ => {
        // don't schedule anything
        pending.enqueue((a, None))
      }
    }
  }

  // This is a nonblocking poll, of course
  def poll(): List[SpawnAgent] = {
    // Even though worker threads will complete tasks out of order, can force
    // determinism by scanning through in order and stopping at the first that
    // isn't done
    val done = new ListBuffer[SpawnAgent]()
    var continue = true
    while (continue && pending.nonEmpty) {
      val a = pending.front
      val ready = a._2 match {
        case Some(task) => {
          if (task.isDone) {
            // Do we even need to call .get? These just have side effects
            // that poke the route internally
            task.get
            true
          } else {
            false
          }
        }
        case None => true
      }
      if (ready) {
        done += a._1
        pending.dequeue
      } else {
        // stop immediately!
        continue = false
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
}

abstract class SpawnAnywhere(sim: Simulation, route_type: String,
                             starts: Iterable[Edge], ends: Iterable[Edge])
extends Generator(sim, route_type)
{
  // Prune starts for road type and length. Arrays have random access.
  val start_candidates = starts.filter(e => e.queue.ok_to_spawn).toArray
  val end_candidates = ends.toArray

  def create_and_poll(n: Int): List[SpawnAgent] = {
    for (i <- (0 until n)) {
      val start = Util.choose_rand[Edge](start_candidates)
      val end = Util.choose_rand[Edge](end_candidates)
      add_specific_agent(start, end, start.queue.safe_spawn_dist)
    }
    return poll
  }
}

// TODO there's further refactorings that should happen between these two...
// Fixed is really a degenerate case of continuous.

class FixedSizeGenerator(sim: Simulation, starts: Iterable[Edge],
                         ends: Iterable[Edge], total: Int,
                         route_type: String)
  extends SpawnAnywhere(sim, route_type, starts, ends)
{
  var num_to_spawn = total

  override def run(): Either[List[SpawnAgent], Boolean] =
    if (num_to_spawn == 0 && pending.isEmpty) {
      // this generator's done.
      Right(true)
    } else if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      Right(true)
    } else {
      val n = num_to_spawn
      num_to_spawn = 0
      Left(create_and_poll(n))
    }

  override def serialize() = "<generator type=\"fixed\" time=\"%f\" starts=\"%s\" ends=\"%s\" total=\"%d\" route_type=\"%s\"/>".format(
    created_at, serialize_ls(start_candidates), serialize_ls(end_candidates),
    total, route_type
  )
}

class ContinuousGenerator(sim: Simulation, starts: Iterable[Edge],
                          ends: Iterable[Edge], spawn_every: Double,
                          route_type: String)
  extends SpawnAnywhere(sim, route_type, starts, ends)
{
  var last_tick = Agent.sim.tick
  var accumulated_time = 0.0

  override def run(): Either[List[SpawnAgent], Boolean] = {
    accumulated_time += Agent.sim.tick - last_tick
    last_tick = Agent.sim.tick

    return if (start_candidates.isEmpty) {
      Util.log("Generator has no viable starting edges!")
      Right(true)
    } else {
      var new_agents: List[SpawnAgent] = Nil
      while (accumulated_time >= spawn_every) {
        accumulated_time -= spawn_every
        new_agents ++= create_and_poll(1)
      }
      Left(new_agents)
    }
  }

  override def serialize() = "<generator type=\"continuous\" time=\"%f\" starts=\"%s\" ends=\"%s\" spawn_every=\"%f\" route_type=\"%s\"/>".format(
    created_at, serialize_ls(start_candidates), serialize_ls(end_candidates),
    spawn_every, route_type
  )
}

class SpecificGenerator(sim: Simulation, route_type: String,
                        positions: Iterable[(Edge, Edge, Double)])
  extends Generator(sim, route_type)
{
  var done = false

  override def run(): Either[List[SpawnAgent], Boolean] = {
    if (done) {
      if (pending.isEmpty) {
        Right(true)
      } else {
        Left(poll)
      }
    } else {
      done = true
      for (p <- positions) {
        add_specific_agent(p._1, p._2, p._3)
      }
      Left(poll)
    }
  }

  override def serialize() = "<generator type=\"specific\" time=\"%f\" positions=\"%s\" route_type=\"%s\"/>".format(
    created_at, positions.map(serialize_pos).mkString(","), route_type
  )

  def serialize_pos(pos: (Edge, Edge, Double)) = "%d/%d/%f".format(pos._1.id, pos._2.id, pos._3)
}

object SpecificGenerator {
  val pattern = """(\d+)/(\d+)/(.+)""".r

  def parse(pos: String, sim: Simulation) = pos match {
    case pattern(start, end, dist) => (sim.edges(start.toInt),
                                       sim.edges(end.toInt),
                                       dist.toDouble)
  }
}
