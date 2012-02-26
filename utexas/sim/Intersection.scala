package utexas.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{SortedSet, TreeSet}

import utexas.map.{Vertex, Turn}

import utexas.{Util, cfg}

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex) {
  //val policy: Policy = new NeverGoPolicy(this)
  val policy: Policy = new StopSignPolicy(this)
  //val policy: Policy = new SignalCyclePolicy(this)
  //val policy: Policy = new ReservationPolicy(this)

  override def toString = "Intersection(" + v + ")"

  // Just delegate.
  def can_go(a: Agent, turn: Turn, far_away: Double) = policy.can_go(a, turn, far_away)
  def unregister(a: Agent) = policy.unregister(a)

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  val turns = MutableMap[Turn, Int]()

  // Check for collisions by detecting agents abnormal changes in ordering.
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight optimization
    // by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns.keys; t2 <- turns.keys if t1.id < t2.id) {
      if (t1.conflicts(t2)) {
        Util.log("!!! Collision in an intersection: " + t1 + " and " + t2)
        assert(t2.conflicts(t1))
      }
    }

    // TODO something more clever with equivalence sets, even though this isn't
    // a transitive relation?
  }

  def enter(a: Agent, t: Turn) = {
    if (!turns.contains(t)) {
      turns(t) = 0
    }
    turns(t) += 1
    if (!policy.validate_entry(a, t)) {
      Util.log("!!! " + a + " illegally entered intersection, going at " + a.speed)
      Util.log("  Incident was near " + t.from + " and " + t + " (vert " + t.from.to.id + ")")
      Util.log("  Origin lane length: " + t.from.length + "; time " + Agent.sim.tick)
      assert(false)
    }
  }

  def exit(a: Agent, t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
    }
    policy.handle_exit(a, t)
  }
}

abstract class Policy(intersection: Intersection) {
  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean
  def validate_entry(a: Agent, turn: Turn): Boolean
  def handle_exit(a: Agent, turn: Turn)
  def unregister(a: Agent)
  def current_greens(): Set[Turn] = Set()
  def dump_info() = {}

  // Since we lookahead over small edges, we maybe won't/can't stop on the edge
  // right before the turn. As long as we validly stopped for us, then fine.
  def is_waiting(a: Agent, t: Turn, far_away: Double) = far_away <= cfg.end_threshold
}

// Great for testing to see if agents listen to this.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def can_go(a: Agent, turn: Turn, far_away: Double) = false
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
  def unregister(a: Agent) = {}
}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // owner of the intersection! may be None when the queue has members. In that
  // case, the first person has to pause a bit longer before continuing.
  var current_owner: Option[Agent] = None
  var queue = List[Agent]()

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // Do they have the lock?
    current_owner match {
      case Some(owner) if a == owner => return true
      case _       =>
    }

    // Schedule them if needed and if they're at the end of the edge.
    if (!queue.contains(a) && is_waiting(a, turn, far_away)) {
      queue :+= a
    }

    // Can we promote them now?
    val ready = !current_owner.isDefined && !queue.isEmpty && a == queue.head &&
                a.how_long_idle >= cfg.pause_at_stop
    if (ready) {
      // promote them!
      current_owner = Some(queue.head)
      queue = queue.tail
      return true
    } else {
      return false
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_owner match {
    case Some(a) => true
    case _       => false
  }

  def handle_exit(a: Agent, turn: Turn) = {
    //assert(a == current_owner.get)    // TODO
    if (!current_owner.isDefined || current_owner.get != a) {
      Util.log(a + " is leaving, but current owner is " + current_owner)
      Util.log("  Crazy guy was attempting " + turn)
      Util.log("  Time was " + Agent.sim.tick)
    }
    current_owner = None
    // Next time queue.head, if it exists, polls, we'll let them go if they've
    // waited patiently.
  }

  def unregister(a: Agent) = {
    current_owner match {
      case Some(agent) if a == agent => {
        // release the lock
        current_owner = None
      }
      case _ => {
        // don't bother with us
        queue = queue.filter(x => x != a)
      }
    }
  }
}

// FIFO based on request, batched by non-conflicting turns.  Possible liveness
// violation, since new agents can pour into the current_turns, and the ones
// that have conflicts wait indefinitely.
// If we found the optimal number of batches, that would be an instance of graph
// coloring.
class ReservationPolicy(intersection: Intersection) extends Policy(intersection) {
  var current_batch = new TurnBatch()
  var reservations = List[TurnBatch]()
  // used to determine if it's an agent's first requent or not
  val current_agents = new MutableSet[Agent]()

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    val first_req = !current_agents.contains(a)

    if (first_req) {
      current_agents += a
      if (current_batch.add_turn(turn)) {
        return true
      } else {
        // A conflicting turn. Add it to the reservations.

        // Is there an existing batch of reservations that doesn't conflict?
        if (!reservations.find(r => r.add_turn(turn)).isDefined) {
          // new batch!
          val batch = new TurnBatch()
          batch.add_turn(turn)
          reservations :+= batch
        }

        return false
      }
    } else {
      return current_batch.has_turn(turn)
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_batch.has_turn(turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_batch.has_turn(turn))
    current_batch.remove_turn(turn)
    current_agents -= a
    if (current_batch.all_done) {
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.size != 0) {
        current_batch = reservations.head
        reservations = reservations.tail
      }
    }
  }

  override def current_greens = current_batch.turns.keys.toSet

  def unregister(a: Agent) = {
    if (current_agents.contains(a)) {
      // TODO what turn do they want to do? decrement that counter...
    }
  }
}

// Count how many agents are doing each type of turn, and add turns that don't
// conflict
class TurnBatch() {
  val turns = new MutableMap[Turn, Int]

  // false if it conflicts with this group
  def add_turn(t: Turn): Boolean = {
    if (turns.contains(t)) {
      // existing turn
      turns(t) += 1
      return true
    } else if (turns.keys.filter(c => t.conflicts(c)).size == 0) {
      // new turn that doesn't conflict
      turns(t) = 1
      return true
    } else {
      // conflict
      return false
    }
  }

  def has_turn(t: Turn) = turns.contains(t)

  def remove_turn(t: Turn) = {
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
    }
  }

  def all_done = turns.isEmpty
}

// A cycle-based light.
class SignalCyclePolicy(intersection: Intersection) extends Policy(intersection) {
  // Assign turns to each cycle
  val cycles: List[Set[Turn]] = find_cycles
  // And have a fixed duration for all of them for now
  val duration = 60.0   // TODO cfg
  // accumulated delay for letting vehicles finish turns.
  var delay = 0.0
  // TODO it'd be great to just store agent, but they aren't officially moved to
  // a turn until too late..
  // these are the agents we've _accepted_, whether or not they've started their
  // turn.
  val current_agents = new MutableSet[(Agent, Turn)]
  // also gives us our state
  var start_waiting: Option[Double] = None
  
  // Must remember we have a delay
  def cur_time = Agent.sim.tick - delay 
  // note that toInt is floor
  // TODO am i absolutely sure about this math?
  def current_cycle = cycles((cur_time / duration).toInt % cycles.size)
  def time_left = duration - (cur_time % duration)

  // Least number of cycles can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach, with the heuristic of trying to
  // include lots of turns from the same edge.
  def find_cycles(): List[Set[Turn]] = {
    // we need deterministic sets
    var turns_left: SortedSet[Turn] = new TreeSet[Turn] ++ intersection.v.turns
    val cycle_list = new ListBuffer[Set[Turn]]()
    while (!turns_left.isEmpty) {
      val this_group = new MutableSet[Turn]()
      val canonical = turns_left.head
      this_group += canonical
      turns_left = turns_left.tail
    
      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      // start with other turns from the same lane as the canonical. really
      // helps throughput.
      //for (candidate <- turns_left) {
      for (candidate <- canonical.from.next_turns ++ turns_left if !this_group(candidate)) {
        if (!this_group.find(t => t.conflicts(candidate)).isDefined) {
          // found one!
          this_group += candidate
        }
      }

      turns_left --= this_group
      cycle_list += this_group.toSet
    }
    return cycle_list.toList
  }

  def state_change = {
    // figure out if a cycle has ended, but agents are still finishing their
    // turn. we're just going to let them finish their turn, and move onto the
    // next cycle after that.
    if (!start_waiting.isDefined) {
      // We can detect slow turning agents if any current agent's turn isn't
      // in the current cycle.
      if (!current_agents.isEmpty && !current_cycle(current_agents.head._2)) {
        // Enter the "waiting for slow agents" state
        start_waiting = Some(Agent.sim.tick)
      }
    }

    if (start_waiting.isDefined) {
      // Filter our agents who haven't started their turn and are not moving;
      // although we told them they could go in this cycle, we can cancel --
      // there's no danger of them entering because it's too late, since
      // they've stopped.
      for (pair <- current_agents) {
        val a = pair._1
        val t = pair._2
        if (a.at.on != t && a.speed == 0.0) {
          //Util.log("canceling " + pair)
          current_agents -= pair
        }
        finish_waiting
      }
    }
    // TODO we want delay to be close to 0 mod duration, so maybe poke at
    // durations every now and then to get back on perfect schedule

    // TODO ooh, this is potentially confusing, but it's a cool cheat:
    // SINCE we aren't activated till somebody polls us ANYWAY, then we don't
    // even start counting delay time till somebody new wants to go.
  }

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // Because the intersection can only act when it's polled here, first we
    // need to do bookkeeping.
    state_change

    if (start_waiting.isDefined) {
      return current_agents((a, turn))
    }

    // Otherwise, try to go.

    if (current_cycle(turn)) {
      // if our worst-case speeding-up distance still lets us back out and stop,
      // then fine, allow it
      val most_dist = a.max_lookahead_dist

      // TODO what thresholds?
      if (most_dist <= far_away - cfg.end_threshold) {
        current_agents += ((a, turn))
        return true
      } else {
        // this is a critical choice. if they accelerate to the speed limit of
        // their destination road (which is certainly an upper bound on their
        // speed), would they make the light?
        val dist_they_need = far_away + turn.length

        // vf = v0 + at
        // min of 0 to account for when they're currently moving faster than the
        // limit. (they'll slow down on their own soon)
        val time_to_reach_limit = math.min(0, math.max(
          time_left,
          (turn.to.road.speed_limit - a.speed) / a.max_accel
        ))
        val accel_dist = Util.dist_at_constant_accel(a.max_accel, time_to_reach_limit, a.speed)
        val speed_dist = turn.to.road.speed_limit * (time_left - time_to_reach_limit)

        // TODO what thresholds?
        if (accel_dist + speed_dist >= dist_they_need) {
          // let them try it
          current_agents += ((a, turn))
          return true
        } else {
          // nope, they won't make it. most_dist > far_away, so hopefully we can
          // still screech to a halt, right? we're not doomed already, are we?
          return false
        }
      }
    } else {
      // No, they definitely need to wait. The light's red for them.
      return false 
    }
  }

  // just make sure they're in the list of current_agents, meaning we accepted
  // them and it's still their normal cycle, or we're waiting on them but
  // they've still been told to go
  def validate_entry(a: Agent, turn: Turn) = current_agents((a, turn))

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_agents((a, turn)))
    current_agents -= ((a, turn))
    // Possibly done waiting.
    finish_waiting
  }

  def finish_waiting() = {
    start_waiting match {
      case Some(time) if current_agents.isEmpty => {
        // Last one! Finally we can hit the next cycle.
        delay += Agent.sim.tick - time
        start_waiting = None
      }
      case _ =>
    }
  }

  override def current_greens = start_waiting match {
    case Some(t) => Set() // nobody can go right now
    case _ => current_cycle
  }

  def unregister(a: Agent) = {}

  override def dump_info() = {
    if (start_waiting.isDefined) {
      Util.log("Waiting on: " + current_agents)
    }
  }
}
