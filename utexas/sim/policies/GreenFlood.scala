package utexas.sim.policies

import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.{MutableList, ListBuffer}
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.cfg
import utexas.map.{Turn, Vertex, Edge}
import utexas.sim.{Simulation, Intersection}

import utexas.Util

// the name comes from http://en.wikipedia.org/wiki/Green_wave
class GreenFlood(sim: Simulation) {
  // output
  val cycles = sim.vertices.map(v => (v, new ListBuffer[Cycle]())).toMap

  val visited = new MutableSet[Vertex]()

  def compute(): Map[Vertex, ListBuffer[Cycle]] = {
    // begin flooding at a critical intersection
    val start_at = sim.vertices.maxBy[Double](v => v.get_priority).in_edges.head
    Util.log("Starting flood from " + start_at)
    // schedule first intersection
    cycles(start_at.to) ++= schedule(start_at, 0)
    // do the main work
    flood(start_at.to)
    // expand turns
    cycles.values.flatten.foreach(c => c.expand_all_turns)
    
    return cycles
  }
  
  // set up all the cycles at one intersection
  def schedule(edge: Edge, first_offset: Double): List[Cycle] = {
    // or standard_turn_set, or any, really.
    val turn_groups = Cycle.grouped_left_turn_sets(edge.to)

    // Schedule all cycles equally
    val duration: Double = cfg.sig_duration / turn_groups.size.toDouble

    // Find the group including the turn our starting edge leads to
    val first_group = turn_groups.find(g => g.contains(edge.leads_to.head)).get
    val first_cycle = new Cycle(first_offset, duration)
    first_group.foreach(t => first_cycle.add_turn(t))

    // Now schedule the rest in any order
    var list: List[Cycle] = List(first_cycle)

    var offset = first_offset + duration
    for (group <- turn_groups if group != first_group) {
      val c = new Cycle(offset, duration)
      offset += duration
      group.foreach(t => c.add_turn(t))
      list :+= c
    }

    return list
  }

  // This is only used internally right now. Weight is just for ranking "4 turns
  // away from where we started flooding" to make this breadth-first; we're not
  // sorting by distance outwards. We could be, but I don't think it necessarily
  // makes a difference.
  class Step(val cycle: Cycle, val offset: Double, val weight: Double) extends Ordered[Step] {
    // Small weights first
    def compare(other: Step) = other.weight.compare(weight)
  }

  // Just one flood.  // Can we get the sentinels to help us with this whole "flood" problem?
  def flood(start: Vertex) = {
    // Initialize
    val queue = new PriorityQueue[Step]()
    for (cycle <- cycles(start)) {
      // initial offset is that of the start cycle
      queue.enqueue(new Step(cycle, cycle.offset, cycle.offset))
    }
    visited += start

    // breadth-first search
    while (!queue.isEmpty) {
      val step = queue.dequeue
      val cycle = step.cycle

      for (turn <- cycle.turns) {
        // what's the minimum delay we should have before making the next lights
        // green?
        // TODO the math for this could be more precise, based on accelerations
        // and following distance delays.
        val min_delay = turn.to.road.speed_limit * (turn.length + turn.to.length)
        val desired_offset = (step.offset + min_delay).toInt

        if (!visited(turn.to.to)) {
          // schedule all the next cycles we can reach
          val next_cycles = schedule(turn.to, desired_offset)
          next_cycles.foreach(
            next => queue.enqueue(new Step(next, next.offset, step.weight + desired_offset))
          )
          cycles(turn.to.to) ++= next_cycles
          visited += turn.to.to
        }
      }
    }
  }
}

object GreenFlood {
  def assign(sim: Simulation, start_at: Edge) = (new GreenFlood(sim)).compute()
}
