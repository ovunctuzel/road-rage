package utexas.sim

import utexas.map.{Edge, Traversable}

import utexas.{Util, cfg}

// TODO introduce a notion of dimension
// TODO logs -> asserts once things work right

// Reason about collisions on edges and within individual turns.
class Queue(t: Traversable) {
  // Descending by distance: head of list is front of traversable.
  var agents = List[Agent]()
  var prev_agents = List[Agent]()   // to verify no collisions occurred in a step

  def last = agents.lastOption

  def start_step() = {
    Agent.q_start_timer.start
    prev_agents = agents
    Agent.q_start_timer.stop
  }
  
  // Check for collisions by detecting abnormal changes in ordering.
  def end_step(): Unit = {
    Agent.q_stop_timer.start
    // TODO if an agent ever looped around to the same edge again in one step,
    // this breaks badly.

    // Everything's fine.
    if (agents.size == 0) {
      Agent.q_stop_timer.stop
      return
    }

    // Find any subsequence of the old crowd.
    val old_crowd = agents.filter(a => prev_agents.contains(a))

    // Make sure they're in the same order. (This also verifies that if one
    // left, everybody ahead also left.)
    if (!prev_agents.containsSlice(old_crowd)) {
      Util.log("!!! Agents on " + t + " changed order!")
    }

    // If anybody from the old crowd is still here, that means all new agents
    // must be at the end. It suffices to check this:
    if (old_crowd.size > 0 && old_crowd.head != agents.head) {
      Util.log("!!! New agents on " + t + " aren't at the tail!")
    }

    // As a final sanity check, make sure the new queue is strictly ordered --
    // no equal distances!
    if (!agents.zip(agents.tail).forall(pair => pair._1.at.dist > pair._2.at.dist)) {
      Util.log("!!! Agents on " + t + " aren't in a valid order!")
    }
    Agent.q_stop_timer.stop
  }

  def enter(a: Agent, dist: Double): Position = {
    Agent.sim.active_queues += this

    // Just find our spot.
    val (ahead, behind) = agents.partition(c => c.at.dist > dist)
    agents = ahead ++ List(a) ++ behind

    return Position(t, dist)
  }

  def exit(a: Agent) = {
    agents = agents.filter(c => c != a)
    if (agents.isEmpty) {
      Agent.sim.active_queues -= this
    }
  }

  def move(a: Agent, new_dist: Double): Position = {
    // TODO more efficiently?
    exit(a)
    return enter(a, new_dist)
  }

  def ahead_of(a: Agent) = agents.takeWhile(_ != a).lastOption

  // Geometric logic for spawning.

  // TODO this gets a bit more conservative when cars have different accelerations.
  // This is hinged on the fact that lookahead works. Agents can't enter e
  // faster than its speed limit, so we have to reason about how far they could
  // possibly go.
  def worst_entry_dist(): Double = {
    val lim = t match {
      case e: Edge => e.road.speed_limit
      case _       => throw new Exception("Only valid for edges, not turns!")
    }
    val accel = cfg.max_accel
    // TODO share this formula with Agent by util or something
    val stopping_dist = Util.dist_at_constant_accel(-accel, lim / accel, lim)
    return (lim * cfg.dt_s) + stopping_dist
  }
  
  // TODO Starting on highways seems weird, but allow it for now
  // TODO justify this better, or cite the paper.
  def ok_to_spawn = t.length >= worst_entry_dist + cfg.end_threshold + (2 * cfg.follow_dist)
  
  // TODO geometric argument
  def safe_spawn_dist = Util.rand_double(
    worst_entry_dist + cfg.follow_dist, t.length - cfg.end_threshold
  )

  // The real-time spawning magic is really quite simple if worst_entry_dist and
  // lookahead work.
  def can_spawn_now(dist: Double): Boolean = {
    val bad_dist = worst_entry_dist
    for (a <- agents.reverse) {
      if (dist > a.at.dist) {
        // make sure nobody's within bad_dist behind this
        if (dist - a.at.dist <= bad_dist) {
          // They could slam into us!
          return false
        }
      } else {
        // don't spawn too close behind somebody
        if (a.at.dist - dist <= cfg.follow_dist) {
          return false
        } else {
          // we can short-circuit; we're fine.
          return true
        }
      }
    }
    // nobody's there? fine.
    return true
  }
}
