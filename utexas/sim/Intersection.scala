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
  //val policy: Policy = new StopSignPolicy(this)
  val policy: Policy = new SignalCyclePolicy(this)
  //val policy: Policy = new ReservationPolicy(this)

  override def toString = "Intersection(" + v + ")"

  // Just delegate.
  def can_go(a: Agent, turn: Turn, far_away: Double) = policy.can_go(a, turn, far_away)
  def yield_lock(a: Agent) = policy.yield_lock(a)
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
      Util.log("  Incident was near " + t.from + " and " + t)
      Util.log("  Origin lane length: " + t.from.length)
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
  def yield_lock(a: Agent)
  def unregister(a: Agent)

  // TODO common stuff for figuring out first-req

  // Since we lookahead over small edges, we maybe won't/can't stop on the edge
  // right before the turn. As long as we validly stopped for us, then fine.
  def is_waiting(a: Agent, t: Turn, far_away: Double) = far_away <= cfg.end_threshold
}

// Great for testing to see if agents listen to this.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def can_go(a: Agent, turn: Turn, far_away: Double) = false
  def validate_entry(a: Agent, turn: Turn) = false
  def handle_exit(a: Agent, turn: Turn) = {}
  def yield_lock(a: Agent) = {}
  def unregister(a: Agent) = {}
}

// Always stop, then FIFO. Totally unoptimized.
class StopSignPolicy(intersection: Intersection) extends Policy(intersection) {
  // owner of the intersection! may be None when the queue has members. In that
  // case, the first person has to pause a bit longer before continuing.
  var current_owner: Option[Agent] = None
  var queue = List[Agent]()
  val yielders = new MutableSet[Agent]()

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // TODO remove once stagnation fixed.
    Agent.sim.debug_agent match {
      case Some(agent) if agent == a => {
        Util.log(a + " wants to go, but locked from " + intersection.v + " by " + current_owner)
        Util.log("  waiting: " + queue)
        if (current_owner.isDefined) {
          Util.log("  locking guy around? " + Agent.sim.agents.contains(current_owner.get))
          Util.log("  locking guy " + current_owner + " is involved with: " +
          current_owner.get.upcoming_intersections)
          // trace this problem.
          Agent.sim.debug_agent = Some(current_owner.get)
        } else {
          Util.log("  so why not now? " + (!current_owner.isDefined) + " " +
          a.how_long_idle)
        }
      }
      case _ =>
    }

    // Do they have the lock?
    current_owner match {
      case Some(owner) if a == owner => return true
      case _       =>
    }

    // Did they have the lock, but temporarily yielded it to break deadlock? If
    // so, give them the lock again if available by placing them on the front of
    // the queue and letting the rest of our promotion logic handle it
    if (yielders(a)) {
      queue = a :: queue
      yielders -= a
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

  def yield_lock(a: Agent): Unit = {
    // Ignore them if they don't have the lock.
    if (current_owner.isDefined && current_owner.get == a) {
      // Don't do this if theyre already in their turn...
      a.at.on match {
        case t: Turn => {
          if (t.vert == intersection.v) {
            //Util.log(a + " trying to yield lock while doing turn, not letting them")
            return
          }
        }
        case _ =>
      }

      // Release the lock, but remember we did this and gain the front of the
      // queue next time we poll
      
      // If nobody's waiting, don't bother
      if (!queue.isEmpty) {
        // Let multiple people yield?
        yielders += a
        current_owner = None

        //Util.log(a + " yielded lock")
      }
    }
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
    if (yielders(a)) {
      // TODO REALLY weird, I wouldnt expect this
      Util.log("a yielder " + a + " is quitting")
      yielders -= a
    }
  }
}

/*// FIFO based on request, batched by non-conflicting turns.
// Possible deadlock, since new agents can pour into the current_turns, and the
// ones that have conflicts wait indefinitely.
// If we found the optimal number of batches, that would be an instance of graph
// coloring.
class ReservationPolicy(intersection: Intersection) extends Policy(intersection) {
  var current_batch = new TurnBatch()
  var reservations = List[TurnBatch]()

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // TODO rethink this idea so that we don't have to put the burden on agents
    // to figure this out.
    val first_req = false

    if (first_req) {
      if (current_batch.add_turn(turn)) {
        return true
      } else {
        // A conflicting turn. Add it to the reservations.

        // Is there an existing batch of reservations that doesn't conflict?
        val batch = reservations.find(r => r.add_turn(turn))
        if (!batch.isDefined) {
          // new batch!
          val b = new TurnBatch()
          b.add_turn(turn)
          reservations :+= b
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
    if (current_batch.all_done) {
      // Time for the next reservation! If there is none, then keep
      // current_batch because it's empty anyway.
      if (reservations.size != 0) {
        current_batch = reservations.head
        reservations = reservations.tail
      }
    }
  }

  def yield_lock(a: Agent) = {}
  def unregister(a: Agent) = {}
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

  def all_done = turns.size == 0
}*/

// A cycle-based light.
class SignalCyclePolicy(intersection: Intersection) extends Policy(intersection) {
  // Assign turns to each cycle
  val cycles: List[Set[Turn]] = find_cycles
  // And have a fixed duration for all of them for now
  val duration = 20.0   // TODO cfg
  
  // note that toInt is floor
  def current_cycle = cycles((Agent.sim.tick / duration).toInt % cycles.size)
  def time_left = duration - (Agent.sim.tick % duration)

  // Least number of cycles can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach.
  def find_cycles(): List[Set[Turn]] = {
    // we need deterministic sets
    var turns_left: SortedSet[Turn] = new TreeSet[Turn] ++ intersection.v.turns
    val cycle_list = new ListBuffer[Set[Turn]]()
    while (!turns_left.isEmpty) {
      val this_group = new MutableSet[Turn]()
      this_group += turns_left.head
      turns_left = turns_left.tail
    
      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      for (candidate <- turns_left) {
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

  var last_group = cycles.last  // TODO dbug
  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    /*val cur = current_cycle
    if (cur != last_group) {
      Util.log("light change! green: " + cur)
      last_group = cur
    }*/

    if (current_cycle(turn)) {
      // can the agent make the light?
      val dist_they_need = far_away + turn.length

      // average-case analysis: assume they keep their current speed
      val avg_case_dist = a.speed * time_left

      if (a.id == 7) {
        Util.log(time_left + "s left. should be able to travel " + avg_case_dist + ", they need " + dist_they_need)
        Util.log("and their speed is " + a.speed)
      }

      val threshold = 0.0 // TODO what should it be?

      if (avg_case_dist < dist_they_need + threshold) {
        // so, they won't finish based on what they're doing now. but, let them
        // TRY to speed up if that doesn't give them a worst-case stopping
        // distance that's illegal -- basically, give us a chance to back out of
        // telling them to try.
        if (a.id == 7) {
        Util.log("at %.1f; %s needs to cover %.1f but prolly only %.1f".format(Agent.sim.tick, a, dist_they_need, avg_case_dist))
        }

        // TODO please, share this with behavior.
        val worst_speed = a.speed + (a.max_accel * cfg.dt_s)
        val worst_travel = Util.dist_at_constant_accel(a.max_accel, cfg.dt_s, a.speed)
        val worst_stop_dist = a.stopping_distance(worst_speed)
        val if_we_let_them_try = worst_travel + worst_stop_dist

        // a different threshold..
        // TODO but then this is TOO safe, it doesnt take into account whether
        // we have lots of time left.
        if (if_we_let_them_try + cfg.end_threshold >= far_away) {
          // no, if we let them speed up, then there's a chance it might not
          // work out.

          // BUT, is it somehow too late? how did that happen? when
          // if_we_let_them_try is much > far_away, then isn't this already a
          // doomed situation?


          if (a.id == 7) {
          Util.log("  but bad! " + if_we_let_them_try + " >= " + far_away + ", so no")
          }
          return false
        } else {
          // try it, we can always back out
          // TODO this is really a systems-inspired process. can we back out of
          // a transaction?
          return true
        }
      } else {
        return true 
      }
    } else {
      // No, they definitely need to wait.
      return false 
    }
  }

  def validate_entry(a: Agent, turn: Turn) = current_cycle(turn)

  def handle_exit(a: Agent, turn: Turn) = {
    assert(current_cycle(turn))
  }

  // We don't keep track of what any agent wants, so ignore all of these things
  // And everybody in the other lanes are gonna yield; of course.
  def yield_lock(a: Agent) = {}
  //def unregister(a: Agent) = {}

  def unregister(a: Agent) = {
    Util.log(a + " trying to unregister")
  }
}
