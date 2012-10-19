// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Edge, Traversable}

import utexas.aorta.{Util, cfg}

// TODO introduce a notion of dimension
// TODO logs -> asserts once things work right

// Reason about collisions on edges and within individual turns.
class Queue(t: Traversable) {
  // Descending by distance: head of list is front of traversable.
  var agents = List[Agent]()
  var last_tick = -1.0              // last observed
  var prev_agents = List[Agent]()   // to verify no collisions occurred in a step

  def head = agents.headOption
  def last = agents.lastOption

  // Called lazily.
  def start_step() = {
    if (last_tick != Agent.sim.tick) {
      prev_agents = agents
      last_tick = Agent.sim.tick
    }
  }
  
  // Check for collisions by detecting abnormal changes in ordering.
  def end_step(): Unit = {
    // TODO if an agent ever looped around to the same edge again in one step,
    // this breaks badly.
    // TODO likewise, problems if an agent quickly lane-changed out, passed
    // someone, then back in.

    // Everything's fine.
    if (agents.size == 0) {
      return
    }

    // Since we allow lane-changing, some funny things could happen. So first
    // just check that the order of the distances matches the order of the
    // queue.
    if (!agents.zip(agents.tail).forall(pair => pair._1.at.dist > pair._2.at.dist)) {
      throw new Exception(
        s"Agents out of order on $t: " +
        agents.map(a => "%d at %.2f".format(a.id, a.at.dist))
      )
    }

    // Now we just want to make sure that all of the agents here last tick are
    // in the same order. If some left, that's fine.
    val old_crowd = agents.filter(a => prev_agents.contains(a))

    if (old_crowd.size > 1) {
      // Since we know the ordering of the distances matches the ordering of the
      // queue from the first check, it suffices to check the ordering of the
      // distances in this list.
      if (!old_crowd.zip(old_crowd.tail).forall(pair => pair._1.at.dist > pair._2.at.dist)) {
        throw new Exception(s"Agents swapped positions on $t")
      }
    }
  }

  def enter(a: Agent, dist: Double): Position = {
    // Just find our spot.

    start_step  // lazily, if needed

    val (ahead, behind) = agents.partition(c => c.at.dist > dist)
    agents = ahead ++ List(a) ++ behind

    // If we're not entering at the end of the queue, something _could_ be odd,
    // so check it.
    if (behind.nonEmpty) {
      Agent.sim.active_queues += this
    }

    return Position(t, dist)
  }

  def exit(a: Agent) = {
    start_step  // lazily, if needed

    // We should leave from the front of the queue generally, unless
    // lane-changing
    if (agents.head != a) {
      Agent.sim.active_queues += this
    }

    agents = agents.filter(c => c != a)
  }

  def move(a: Agent, new_dist: Double): Position = {
    // TODO more efficiently?
    exit(a)
    return enter(a, new_dist)
  }

  def ahead_of(a: Agent) = agents.takeWhile(_ != a).lastOption
  def closest_behind(dist: Double) = agents.find(a => a.at.dist <= dist)
  def closest_ahead(dist: Double) = agents.takeWhile(_.at.dist >= dist).lastOption

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

  def ok_to_lanechange =
    (t.length >= cfg.lanechange_dist + cfg.end_threshold) &&
    // this second constraint can be removed once lookbehind is implemented
    (t.length >= worst_entry_dist + cfg.follow_dist)
  
  // TODO geometric argument
  // TODO sometimes the max arg is < the min arg. :)
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
