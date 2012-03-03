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
  val stdPhases = sim.vertices.map(v => (v, Cycle.standard_turn_sets(v))).toMap
  val cycles = sim.vertices.map(v => (v, new ListBuffer[Cycle]())).toMap

  // TODO non-deterministic results!!! sorted set implementation is SLOW though.
  val visited = new MutableSet[Vertex]()

  def compute(): Map[Vertex, ListBuffer[Cycle]] = {
    val start_at = sim.vertices.maxBy[Double](v => v.get_priority).in_edges.head
    Util.log("Starting flood from "+start_at)
    var startCycles = getCycles(start_at, 0) //Cycle.cycle_for_edge(start_at, 0, duration)
    cycles(start_at.to) ++= startCycles
    flood(start_at.to)

    val max_cycles = cycles.values.foldLeft(0)((a, b) => math.max(a, b.size))
    Util.log("All intersections have <= " + max_cycles + " cycles")

    cycles.values.flatten.foreach(c => c.add_all_turns)
    
    return cycles
  }
  
  def getCycles(edge: Edge, offset: Double):Set[Cycle] = {
    val preCycleSet = stdPhases(edge.to)
    //First do the incoming edge
    var desiredCycle = Set[Turn]()
    for (preCycle <- preCycleSet){
      if (preCycle.contains(edge.leads_to.head)) desiredCycle = preCycle
    }
    assert(!desiredCycle.isEmpty)
    val c = new Cycle(offset, (cfg.sig_duration/preCycleSet.size.toDouble).toInt)
    for (turn <- desiredCycle) c.add_turn(turn)
    //Now do the rest of the cycles
    var cycleSet = Set[Cycle]()
    cycleSet += c
    var i = 1
    for (preCycle <- preCycleSet if preCycle != desiredCycle){
      val oc = new Cycle(offset+((cfg.sig_duration/preCycleSet.size.toDouble)*i).toInt, (cfg.sig_duration/preCycleSet.size.toDouble).toInt)
      i+=1
      for (turn <- preCycle) oc.add_turn(turn)
      cycleSet += oc
    }
    assert(cycleSet.size == preCycleSet.size)
    return cycleSet
  }

  // TODO pick the cycle that could fit our turn?
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
  class Step(val cycle: Cycle, val offset: Double, val weight: Double) extends Ordered[Step] {
    // Small weights first
    def compare(other: Step) = other.weight.compare(weight)
  }

  // Just one flood.  //Can we get the sentinels to help us with this whole "flood" problem?
  def flood(start: Vertex) = {
    // Initialize
    val queue = new PriorityQueue[Step]()
    for (cycle <- cycles(start)) {
      // initial offset is that of the start cycle
      queue.enqueue(new Step(cycle, cycle.offset, cycle.offset))
      visited += start
    }

    // breadth-first search
    while (!queue.isEmpty) {
      val step = queue.dequeue
      val cycle = step.cycle

      // what's the minimum delay we should have before making the next lights
      // green?
      // TODO the math for this could be more precise, based on accelerations
      // and following distance delays.
      for (turn <- cycle.turns){
    	 val min_delay = turn.to.road.speed_limit * (turn.length + turn.to.length)
    	 val desired_offset = (step.offset + min_delay).toInt

    	 if (!visited(turn.to.to)){
    	 	val nextCycles = getCycles(turn.to, desired_offset)
    	 	// schedule all the next cycles we can reach
    	 	for (next <- nextCycles) {
    	 		queue.enqueue(new Step(next, next.offset, step.weight + desired_offset))
    	 	}
    	 	cycles(turn.to.to) ++= nextCycles.toList.sortBy(c => c.offset)
    	 	visited += turn.to.to
    	 }
      }
    }
    assert (visited.size == sim.vertices.size)
  }
}

object GreenFlood {
  def assign(sim: Simulation, start_at: Edge) = (new GreenFlood(sim)).compute()
}
