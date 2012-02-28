package utexas.sim.policies

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import scala.collection.mutable.ListBuffer

import utexas.map.Turn
import utexas.sim.{Intersection, Policy, Agent}

import utexas.{Util, cfg}

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
  val current_agents = new MutableHashSet[(Agent, Turn)]
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
      val this_group = new MutableHashSet[Turn]()
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
