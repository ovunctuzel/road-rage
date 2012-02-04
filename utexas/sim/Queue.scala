package utexas.sim

import utexas.map.Traversable
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

  def random_spawn(dynamic:Boolean = false): Double = {
    assert(ok_to_spawn(dynamic))
    
    // TODO here's where we have to fix old problems:
    // 1) if we're spawning in front of somebody and starting at 0 km/s...,
    //    could have collisions on THIS edge and on predecessors
    // 2) if we're running right now, what do?

    // We have to enter at the end, so...
    if (dynamic){
      throw new UnsupportedOperationException("We can't spawn agents dynamically yet!"); //TODO
    }
    else{
      var lastSeen = 0.
      for (a <- agents.reverse){
        var dist = a.at.dist
        if (dist - lastSeen > cfg.min_spawn_dist*2) return lastSeen + Util.rand_double(cfg.min_spawn_dist,dist-lastSeen-cfg.min_spawn_dist)
        else lastSeen = dist
      }
      if (t.length - lastSeen > cfg.min_spawn_dist*2) return lastSeen + Util.rand_double(cfg.min_spawn_dist,t.length-lastSeen-cfg.min_spawn_dist)
    }
    throw new IllegalStateException("Failed to find spot for agent on traversable "+t)
  }

  //Since we're trying to have a ton of agents, just make sure we have min_spawn_dist
  def ok_to_spawn(dynamic:Boolean = false): Boolean = {
    if (dynamic){
      return false; //TODO
    }
    else return (agents.size+5)*cfg.min_spawn_dist < t.length //+5 accounts for start/end (2 for each) and rounding
  }

  def ahead_of(a: Agent) = agents.takeWhile(_ != a).lastOption
}
