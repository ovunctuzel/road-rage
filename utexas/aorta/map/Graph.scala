// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable.{HashMap, PriorityQueue, HashSet}

import utexas.aorta.map.make.Reader

import utexas.aorta.Util

class Graph(val roads: Array[Road], val edges: Array[Edge],
            val vertices: Array[Vertex], val wards: List[Ward],
            val special_ward: Ward)
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
    // Used for heuristic
    val goal_pt = to.start_pt
    // The caller doesn't want an empty path
    val loop = from == to
    // how to trace back where we've been
    val backrefs = new HashMap[Traversable, Traversable]()
    // consider these in order
    val open = new PriorityQueue[Step]()
    val open_members = new HashSet[Traversable]()
    // done evaluating these
    val visited = new HashSet[Traversable]()
    // best distance so far
    val costs = new HashMap[Traversable, Double]()

    class Step(val on: Traversable, heuristic: Double) extends Ordered[Step] {
      def cost = costs(on)
      def score = cost //+ heuristic   TODO
      def compare(other: Step) = other.score.compare(score)

      override def toString = "%.2f away, %.2f heuristic, consider %s".format(cost, heuristic, on)
    }

    // Start
    costs(from) = 0
    open.enqueue(new Step(from, from.start_pt.dist_to(goal_pt)))
    open_members += from
    backrefs(from) = null  // encode the start somehow

    // Main loop
    var first = true
    while (!open.isEmpty) {
      val step = open.dequeue
      visited += step.on
      open_members -= step.on

      // Are we there yet?
      if (step.on == to && !first) {
        var path: List[Traversable] = Nil
        var at: Option[Traversable] = Some(step.on)
        while (at.isDefined && at.get != null) {
          path = at.get :: path
          // clean as we go to break loops
          at = backrefs.remove(at.get)
        }
        // the first step is 'from'
        return path.tail
      }

      // Where can we go next?
      for (next <- step.on.leads_to) {
        if ((loop && next == from) || !visited.contains(next)) {
          val lane_changing = (step.on, next) match {
            case (_: Edge, _: Edge) => true
            case _ => false
          }

          // Lane-changing costs 0 distance, because we've already paid for
          // the current edge's distance.
          val tentative_cost = if (lane_changing)
                                 costs(step.on)
                               else
                                 // TODO but then we wont get ordering till
                                 // later..
                                 costs(step.on) + step.on.length//next.length

          // TODO costs => open_members?
          if (!open_members.contains(next) || tentative_cost < costs(next)) {
            backrefs(next) = step.on
            costs(next) = tentative_cost
            // TODO if they're in open_members, modify weight in the pri
            // queue...
            val heuristic = next.end_pt.dist_to(goal_pt)

            open.enqueue(new Step(next, heuristic))
            open_members += next
          }
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
