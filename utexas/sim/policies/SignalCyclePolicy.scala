package utexas.sim.policies

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.map.{Turn, Edge, Vertex}
import utexas.sim.{Intersection, Policy, Agent, EV_Signal_Change}

import utexas.{Util, cfg}

// TODO name 'Phase'?
class Cycle(val offset: Double, val duration: Double) {
  assert(offset >= 0)
  assert(duration > 0)
  // TODO only mutable because we build this incrementally
  val turns = new MutableSet[Turn]()

  // returns false if it conflicts
  // assumes turn is from the correct intersection.
  def add_turn(turn: Turn): Boolean = {
    if (could_add_turn(turn)) {
      turns += turn
      return true
    } else {
      return false
    }
  }

  // Don't actually add turn, just see if it would conflict or not
  def could_add_turn(turn: Turn): Boolean = {
    val conflicts = turn.conflicts  // Cache it
    return !turns.find(t => conflicts(t)).isDefined
  }

  def has(turn: Turn) = turns(turn)

  def vert() = turns.head.vert
}

// factory methods for cycles
object Cycle {
  val nil_cycle = new Cycle(0, 1)
  // TODO not necessarily the nicest way to group all of this or dole it out
  // can't comptue this now; Agent.sim may not exist yet.
  var greenflood_assignments: Map[Vertex, ListBuffer[Cycle]] = null

  // SignalCyclePolicy asks us, so we can do some one-time work and dole out the
  // results or lazily compute
  def cycles_for(i: Intersection): List[Cycle] = {
    if (greenflood_assignments == null) {
      Util.log("Green-flooding from " + i)
      Util.log_push
      // Start from any edge leading to this intersection
      greenflood_assignments = GreenFlood.assign(Agent.sim, i.v.turns.head.from)
      Util.log_pop
    }

    return greenflood_assignments(i.v).toList

    // for now...
    //return arbitrary_cycles(i, 0, 60)   // TODO cfg
  }

  // all of an edge's turns
  def cycle_for_edge(e: Edge, offset: Double, duration: Double): Cycle = {
    val c = new Cycle(offset, duration)
    e.next_turns.foreach(t => c.add_turn(t))
    return c
  }

  // Least number of cycles can be modeled as graph coloring, but we're just
  // going to do a simple greedy approach, with the heuristic of trying to
  // include lots of turns from the same edge.
  def arbitrary_cycles(i: Intersection, initial_offset: Double, duration: Double): List[Cycle] =
  {
    // we need sorted sets to keep determinism
    var turns_left: SortedSet[Turn] = new TreeSet[Turn] ++ i.v.turns
    val cycle_list = new ListBuffer[Cycle]()
    var offset = initial_offset
    while (!turns_left.isEmpty) {
      val this_cycle = new Cycle(offset, duration)
      val canonical = turns_left.head
      this_cycle.add_turn(canonical)
      turns_left = turns_left.tail
    
      // conflict relation is symmetric, but not transitive... so do quadratic
      // conflict checking
      // start with other turns from the same lane as the canonical. really
      // helps throughput.
      //for (candidate <- turns_left) {
      for (candidate <- canonical.from.next_turns ++ turns_left) {
        if (this_cycle.add_turn(candidate)) {
          turns_left -= candidate
        }
      }
        
      cycle_list += this_cycle
      offset += duration
    }
    return cycle_list.toList
  }
}

// A cycle-based light.
class SignalCyclePolicy(intersection: Intersection) extends Policy(intersection) {
  val cycles = Cycle.cycles_for(intersection)
  val initial_offset = cycles.head.offset

  // Do some verification... make sure offsets are strictly increasing,
  // durations make sense, and that all turns are covered
  val total_duration: Double = {
    var expect_offset = initial_offset
    val turns_seen = new MutableSet[Turn]  // don't just count, count uniques.

    for (cycle <- cycles) {
      assert(cycle.offset == expect_offset)
      expect_offset += cycle.duration

      turns_seen ++= cycle.turns
    }

    assert(turns_seen.size == intersection.v.turns.size)

    expect_offset - initial_offset // this is now the total duration
  }

  // accumulated delay for letting vehicles finish turns. just for statistics
  // for now, eventually for modifying timings to get back on schedule.
  var delay = 0.0
  // TODO it'd be great to just store agent, but they aren't officially moved to
  // a turn until too late..
  // these are the agents we've _accepted_, whether or not they've started their
  // turn.
  val current_agents = new MutableSet[(Agent, Turn)]
  // TODO perhaps be Either[Double, Cycle].
  // when None, we're in a normal cycle.
  var start_waiting: Option[Double] = None
  // nil cycle for when we're waiting.
  var current_cycle = Cycle.nil_cycle
  var next_cycles: List[Cycle] = cycles
  // find the cycle that should be active right now and how long it should last
  var end_at = 0.0
  def time_left = end_at - Agent.sim.tick

  def schedule(at: Double) = {
    end_at = at
    Agent.sim.schedule(at, { this.cycle_change })
  }

  {
    var time = (Agent.sim.tick - initial_offset) % total_duration  // TODO make it > 0 too?
    while (current_cycle == Cycle.nil_cycle) {
      val c = next_cycles.head
      next_cycles = next_cycles.tail
      if (time < c.duration) {
        current_cycle = c
      } else {
        time -= c.duration
      }
    }
    // we can also compute the tick when this cycle should finish
    // so if all goes well, make the simulation poke us when we should change
    // cycles / when this cycle should finish
    schedule(Agent.sim.tick + (current_cycle.duration - time))
  }

  def cycle_change(): Unit = {
    if (Agent.sim.tick < end_at) {
      return
    }

    // Are there slow agents?
    if (!current_agents.isEmpty) {
      // Yup. Gotta wait for them.
      start_waiting = Some(Agent.sim.tick)
      current_cycle = Cycle.nil_cycle
    } else {
      // switch to the next cycle
      if (next_cycles.isEmpty) {
        // cycle around
        next_cycles = cycles
      }

      // callback for UI usually
      Agent.sim.tell_listeners(
        EV_Signal_Change(current_cycle.turns.toSet, next_cycles.head.turns.toSet)
      )

      current_cycle = next_cycles.head
      next_cycles = next_cycles.tail
      // schedule the next trigger
      // TODO could account for delay here by adjusting duration.
      schedule(Agent.sim.tick + current_cycle.duration)
    }
  }

  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    // Flush out stalled slowpokes.
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

    // Still waiting?
    if (start_waiting.isDefined) {
      return current_agents((a, turn))
    }

    // Otherwise, try to go.

    if (current_cycle.has(turn)) {
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
    // we could be ready to change
    cycle_change
  }

  override def current_greens = start_waiting match {
    case Some(t) => Set() // nobody can go right now
    case _ => current_cycle.turns.toSet
  }

  def unregister(a: Agent) = {}

  override def dump_info() = {
    if (start_waiting.isDefined) {
      Util.log("Waiting on: " + current_agents)
    }
  }
}
