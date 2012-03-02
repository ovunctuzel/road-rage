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
  val visited = new MutableSet[Turn]()

  def compute(start_at: Edge): Map[Vertex, ListBuffer[Cycle]] = {
    val duration = 60   // TODO cfg
    var start_cycle = Cycle.cycle_for_edge(start_at, 0, duration)
    cycles(start_cycle.vert) += start_cycle
    flood(start_cycle)

    val max_cycles = cycles.values.foldLeft(0)((a, b) => math.max(a, b.size))
    Util.log("All intersections have <= " + max_cycles + " cycles")

    return cycles
  }

  // TODO pick the cycle tha could fit our turn?
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

  // Just one flood.
  def flood(start: Cycle) = {
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

      // schedule all the next turns we can reach in some cycle.
      for (next <- turn.to.next_turns if !visited(next)) {
        visited += next

        // find a cycle compatible with this turn, or make a new one
        // TODO if there's a cycle that supports this turn but the offset is way
        // off, we could consider making a new cycle instead.
        // TODO this finds the first that matches; it may be better to find all
        // matches and grab the one with the best offset
        val cycle = cycles(next.vert).find(c => c.could_add_turn(next)) match {
          case Some(c) => c   // TODO unless it has a wretched offset
          case None    => {
            // make a new cycle for this flooding
            // we want it to immediately follow the last cycle scheduled,
            // if there is one.
            val offset = next_offset(next.vert, desired_offset)
            // offset < desired => turn green early and waste time
            // offset > desired => potential congestion; wait more

            // TODO I think it makes sense to keep the same.
            val c = new Cycle(offset, start.duration)
            cycles(next.vert) += c
            c
          }
        }

        // add the turn. it WILL work, by the way we've defined the cycle.
        assert(cycle.add_turn(next))

        // continue flooding
        // TODO seems like it makes more sense to use actual offset here, not
        // desired.
        queue.enqueue(new Step(next, cycle.offset, step.weight + 1))
      }
    }
  }
}

object GreenFlood {
  def assign(sim: Simulation, start_at: Edge) = (new GreenFlood(sim)).compute(start_at)
}
