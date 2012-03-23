// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue}

import utexas.aorta.map.make.Reader

import utexas.aorta.Util

class Graph(val roads: Array[Road], val edges: Array[Edge],
            val vertices: Array[Vertex], val wards: List[Ward],
            val special_ward: Ward, val ubersections: Array[UberSection])
{
  val turns = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)

  // Tell road about their ward
  for (w <- special_ward :: wards) {
    w.roads.foreach(r => r.ward = w)
  }

  def traversables() = edges ++ turns

  // TODO eventually make this very general and reusable
  // Returns the sequence of both edges and turns. Lane-changes are implicit
  // (two edges without a turn in between). First step is NOT 'from', but last
  // step is 'to'.
  def pathfind_astar(from: Edge, to: Edge): List[Traversable] = {
    // This is only used internally right now
    class Step(val on: Traversable, val dist: Double) extends Ordered[Step] {
      // This orders small distances first
      def compare(other: Step) = other.dist.compare(dist)
    }

    // If this is requested, presumably they don't want us to return an empty
    // path
    val loop = from == to
    // Used for heuristic
    val goal_pt = to.start_pt
    // encodes where we've visited and the backreferences for getting there
    val visited = new HashMap[Traversable, Traversable]()
    val open = new PriorityQueue[Step]()

    // Start
    open.enqueue(new Step(from, 0))
    visited(from) = null  // some way of encoding the start

    // Main loop
    var first = true
    while (!open.isEmpty) {
      val step = open.dequeue

      // Are we there yet?
      if (step.on == to && !first) {
        var path: List[Traversable] = Nil
        var at: Option[Traversable] = Some(step.on)
        while (at.isDefined && at.get != null) {
          path = at.get :: path
          // clean as we go to break loops
          at = visited.remove(at.get)
        }
        // the first step is 'from', but we know.
        return path.tail
      }

      // Where can we go next?
      for (next <- step.on.leads_to) {
        if ((loop && next == from) || !visited.contains(next)) {
          // TODO do we have to relax / handle finding a shorter path?
          // TODO there are probably better heuristics than euclid
          val heuristic = next.start_pt.dist_to(goal_pt)
          open.enqueue(new Step(next, step.dist + next.length + heuristic))
          visited(next) = step.on
        }
      }

      first = false
    }

    // We didn't find the way?! The graph is connected!
    throw new Exception("Couldn't A* from " + from + " to " + to)
  }
}

// It's a bit funky, but the actual graph instance doesn't have this; we do.
object Graph {
  var width = 0.0
  var height = 0.0
  var xoff = 0.0
  var yoff = 0.0
  var scale = 0.0

  // this MUST be set before world_to_gps is called.
  def set_params(w: Double, h: Double, x: Double, y: Double, s: Double) = {
    width = w
    height = h
    xoff = x
    yoff = y
    scale = s
  }

  // inverts what PreGraph1's normalize() does.
  def world_to_gps(x: Double, y: Double) = new Coordinate(
    (x / scale) - xoff, ((height - y) / scale) - yoff
  )

  def load(fn: String) = (new Reader(fn)).load_map
}
