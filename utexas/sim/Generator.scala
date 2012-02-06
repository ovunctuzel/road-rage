package utexas.sim

import utexas.map.Edge

import utexas.{Util, cfg}

// TODO abstract class generator. implementations could use blocking routing,
// futures, ...  or generators dispatch to another mechanism for routefinding,
// altho that might not be clean.

// TODO this will really be 'blocking ASAP' generator
abstract class Generator(sim: Simulation, desired_starts: List[Edge], val end_candidates: List[Edge])
{
  // Prune the desired_starts for road type and length
  val start_candidates = desired_starts.filter(e => ok_to_spawn(e))
  var ready_agents: List[Agent] = Nil

  // Returns new agents to try to spawn. Override this!
  def run(): List[Agent]

  def add_specific_agent(start: Edge, end: Edge) = {
    val a = new Agent(sim.next_id, sim, start, safe_spawn_dist(start))
    // TODO the work we do here, if any, actually depends on the behavior.

    // immediately and blockingly compute the path
    a.behavior.give_route(sim.pathfind_astar(start, end))

    // oh hey, lookie
    ready_agents :+= a
  }

  // TODO and length.
  def ok_to_spawn(e: Edge) = e.road.road_type == "residential" && e.length >= 1.0

  // TODO math, please.
  def safe_spawn_dist(e: Edge) = Util.rand_double(.20 * e.length, .80 * e.length)

  // TODO makes sense for delayed computation ones.
  def wait_for_all() = {
  }
}

class FixedSizeGenerator(sim: Simulation, starts: List[Edge], ends: List[Edge], total: Int)
  extends Generator(sim, starts, ends)
{
  var num_to_spawn = total

  override def run(): List[Agent] = {
    // First create new agents
    if (num_to_spawn != 0) {
      if (start_candidates.isEmpty) {
        Util.log("Generator has no viable starting edges!")
        return Nil
      } else {
        Util.log("We'll make " + num_to_spawn)
        for (i <- (0 until num_to_spawn)) {
          val start = Util.choose_rand[Edge](start_candidates)
          val end = Util.choose_rand[Edge](end_candidates)
          add_specific_agent(start, end)
          num_to_spawn -= 1
        }
        Util.log("Done spawning")
      }
    }

    // Then return anybody who's ready.
    val ready = ready_agents
    ready_agents = Nil
    return ready
  }
}

//class ConstantGenerator() extends Generator() {
//}
