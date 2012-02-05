package utexas.sim

import utexas.map.Traversable
import utexas.Util

// TODO introduce a notion of dimension
// TODO logs -> asserts once things work right

// Reason about collisions on edges and within individual turns.
class Queue(t: Traversable) {
  // Descending by distance: head of list is front of traversable.
  var agents = List[Agent]()
  var prev_agents = List[Agent]()   // to verify no collisions occurred in a step

  def last = agents.lastOption

  def start_step() = {
    prev_agents = agents
  }
  
  // Check for collisions by detecting abnormal changes in ordering.
  def end_step(): Unit = {
    // TODO if an agent ever looped around to the same edge again in one step,
    // this breaks badly.

    // Everything's fine.
    if (agents.size == 0) {
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
  }

  def enter(a: Agent, dist: Double): Position = {
    // Just find our spot.

    val (ahead, behind) = agents.partition(c => c.at.dist > dist)
    agents = ahead ++ List(a) ++ behind

    return Position(t, dist)
  }

  def exit(a: Agent) = {
    agents = agents.filter(c => c != a)
  }

  def move(a: Agent, new_dist: Double): Position = {
    // TODO more efficiently?
    exit(a)
    return enter(a, new_dist)
  }

  def random_spawn(): Double = {
    assert(ok_to_spawn)
    
    // TODO here's where we have to fix old problems:
    // 1) if we're spawning in front of somebody and starting at 0 km/s...,
    //    could have collisions on THIS edge and on predecessors
    // 2) if we're running right now, what do?

    // We have to enter at the end, so...
    val max = if (agents.size == 0)
                .80 * t.length
              else
                .80 * agents.last.at.dist

    return Util.rand_double(.20 * t.length, max)
  }

  // TODO they should really just start a fixed bit back, not more and more as
  // the last agent gets closer to the start...
  def ok_to_spawn(): Boolean = if (agents.size == 0)
                                 true
                               else
                                 .20 * t.length < .80 * agents.last.at.dist

  def ahead_of(a: Agent) = agents.takeWhile(_ != a).lastOption
}
