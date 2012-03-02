package utexas.sim.policies

import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.{MutableList, ListBuffer}
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.map.{Turn, Vertex, Edge}
import utexas.sim.{Simulation, Intersection}

import utexas.Util

// the name comes from http://en.wikipedia.org/wiki/Green_wave
class GreenFlood(sim: Simulation) {
  val cycles = sim.vertices.map(v => (v, new ListBuffer[Cycle]())).toMap

  // TODO non-deterministic results!!! sorted set implementation is SLOW though.
  //var red_turns: SortedSet[Turn] = new TreeSet[Turn]()

  // when we hit a turn that can't be green, just remember so we can do more
  // flooding later
  val red_turns = new MutableSet[Turn]()
  red_turns ++= sim.turns

  // TODO ultimately i don't think we need visited explicitly...
  // TODO and if we have this per flood, it would mean a turn could be green
  // during multiple cycles, not just one. could that ever be undesirable?
  // doesn't seem that way with the current strategy.
  val visited = new MutableSet[Turn]()

  def compute(start_at: Edge): Map[Vertex, ListBuffer[Cycle]] = {
    val duration = 60   // TODO cfg
    var turns_left = red_turns.size
    var start_cycle = Cycle.cycle_for_edge(start_at, 0, duration)
    cycles(start_cycle.vert) += start_cycle

    var flood_cnt = 0
    while (turns_left > 0) {
      flood_cnt += 1
      val new_greens = flood(start_cycle)
      turns_left -= new_greens
      Util.log(new_greens + " green turns during this flood; " + turns_left + " remaining")

      if (turns_left != 0) {
        // Pick the next start cycle.

        // TODO probably some much better heuristics than this.

        // Pick a random turn, find every unscheduled turn at that intersection,
        // and append a cycle there.
        val vert = red_turns.head.vert
        // TODO it could certainly work out that there are no cycles scheduled
        // here yet (if the previous floods never even reach this vert). does it
        // make sense to try to start with an offset and be in effect
        // 'simultaneously' with another flood group?
        start_cycle = new Cycle(next_offset(vert, 0), duration)
        cycles(vert) += start_cycle
        vert.turns.filter(t => red_turns(t)).foreach(t => start_cycle.add_turn(t))
        assert(!start_cycle.turns.isEmpty)
      }
    }
    val max_cycles = cycles.values.foldLeft(0)((a, b) => math.max(a, b.size))
    Util.log(flood_cnt + " total floods; all intersections have <= " + max_cycles + " cycles")

    return cycles
  }

  def next_offset(v: Vertex, default: Double): Double = {
    val ls = cycles(v)
    return if (ls.isEmpty)
             default
           else
             ls.last.offset + ls.last.duration
  }

  // This is only used internally right now. Weight is just for ranking "4 turns
  // away from where we started flooding" to make this breadth-first; we're not
  // sorting by distance outwards. We could be, but I don't think it necessarily
  // makes a difference.
  class Step(val turn: Turn, val offset: Double, val weight: Int) extends Ordered[Step] {
    // Small weights first
    def compare(other: Step) = other.weight.compare(weight)
  }

  def flood(start: Cycle): Int = {
    // Don't forget this
    red_turns --= start.turns
    var green_cnt = start.turns.size

    // We've created a new cycle for which vertices during this flooding?
    val member_cycles = new MutableSet[Vertex]()

    // Initialize
    val queue = new PriorityQueue[Step]()
    for (t <- start.turns) {
      // initial offset is that of the start cycle
      queue.enqueue(new Step(t, start.offset, 0))
      visited += t
    }

    // breadth-first search
    while (!queue.isEmpty) {
      val step = queue.dequeue
      val turn = step.turn

      // what's the minimum delay we should have before making the next lights
      // green?
      // TODO the math for this could be more precise, based on accelerations
      // and following distance delays.
      val min_delay = turn.to.road.speed_limit * (turn.length + turn.to.length)
      val desired_offset = step.offset + min_delay

      // what're the next turns we should try to make green?
      for (next <- turn.to.next_turns if !visited(next)) {
        visited += next
        val ls = cycles(next.vert)

        // have we made a cycle for this vertex during this flood yet?
        val cycle = if (member_cycles(next.vert))
                      // it'll be the most recent cycle added
                      ls.last
                    else
                      {
                        // make a new cycle for this flooding
                        // we want it to immediately follow the last cycle scheduled,
                        // if there is one.
                        val offset = next_offset(next.vert, desired_offset)
                        // offset < desired => turn green early and waste time
                        // offset > desired => potential congestion; wait more

                        // TODO I think it makes sense to keep the same.
                        val c = new Cycle(offset, start.duration)
                        member_cycles += next.vert
                        cycles(next.vert) += c
                        c
                      }

        // is it compatible?
        if (cycle.add_turn(next)) {
          green_cnt += 1
          red_turns -= next
          // continue flooding
          queue.enqueue(new Step(next, desired_offset, step.weight + 1))
        } else {
          // work on it later (TODO may change this later to only do one
          // flooding... after all, if we know offsets...)
        }
      }
    }

    return green_cnt
  }
}

object GreenFlood {
  def assign(sim: Simulation, start_at: Edge) = (new GreenFlood(sim)).compute(start_at)
}
