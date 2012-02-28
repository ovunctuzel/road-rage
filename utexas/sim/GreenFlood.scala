package utexas.sim

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.MutableList
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.map.Turn

import utexas.Util

class Cycle(val offset: Double, val duration: Double) {
  val turns = new MutableSet[Turn]()

  // returns false if it conflicts
  // assumes turn is from the correct intersection.
  def add_turn(turn: Turn): Boolean = {
    val conflicts = turn.conflicts  // Cache it
    if (turns.find(t => conflicts(t)).isDefined) {
      // conflict
      return false
    } else {
      turns += turn
      return true
    }
  }
}

// the name comes from http://en.wikipedia.org/wiki/Green_wave
class GreenFlood(sim: Simulation) {
  // This is only used internally right now. Weight is just for ranking "4 turns
  // away from where we started flooding" to make this breadth-first; we're not
  // sorting by distance outwards. We could be, but I don't think it necessarily
  // makes a difference.
  class Step(val turn: Turn, val offset: Double, val weight: Int) extends Ordered[Step] {
    // Small weights first
    def compare(other: Step) = other.weight.compare(weight)
  }

  def flood(start: Cycle): List[Turn] = {
    // TODO I think it makes sense to keep the same.
    val duration = start.duration

    // output (TODO actually put this in the intersections)
    val cycles = sim.intersections.map(i => (i, new MutableList[Cycle]())).toMap
    // TODO and I'm not sure this is what we want... fit in a new turn into
    // whatever cycle works? then no re-flooding... hmm, maybe that isn't
    // necessary though?
    val cur_cycle = new MutableMap[Intersection, Cycle]()

    // when we hit a turn that can't be green, just remember the intersection so
    // we can do more flooding later
    val red_intersections = new MutableSet[Intersection]()

    // Initialize
    val queue = new PriorityQueue[Step]()
    val visited = new MutableSet[Turn]()   // TODO ideally i dont think this is right...
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
      val min_delay = turn.to.road.speed_limit * (turn.length + turn.to.length)
      val offset = step.offset + min_delay

      // what're the next turns we should try to make green?
      for (next <- turn.to.next_turns if !visited(next)) {
        visited += next
        val intersection = sim.intersections(next.vert)

        // can we do it?
        val cycle = if (cur_cycle.contains(intersection))
                      cur_cycle(intersection)
                    else
                      {
                        val c = new Cycle(offset, duration)
                        cur_cycle(intersection) = c
                        c
                      }
        // TODO record in output cycles

        if (cycle.add_turn(next)) {
          // continue flooding
          queue.enqueue(new Step(next, offset, step.weight + 1))
        } else {
          // work on it later
          // TODO or now, and create a new cycle and get rid of the idea of
          // cur_cycle?
          red_intersections += intersection
        }
      }
    }

    // TODO for now, just visualize.
    // TODO if we're not setting up cycles and instead returning all these
    // turns, then at least build this in-place
    val green_turns = cur_cycle.values.flatMap(c => c.turns).toList

    Util.log(green_turns.size + " green turns in this flood; " +
             (sim.turns.size - green_turns.size) + " red turns remaining")

    return green_turns
  }
}
