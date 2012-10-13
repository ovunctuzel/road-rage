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
  // Returns a sequence of connected roads [from, to]. The user must decide how
  // to lane-change.
  def pathfind_astar(from: DirectedRoad, to: DirectedRoad): List[DirectedRoad] = {
    // Used for heuristic
    val goal_pt = to.end_pt
    // The caller doesn't want an empty path
    val loop = from == to
    // how to trace back where we've been
    val backrefs = new HashMap[DirectedRoad, DirectedRoad]()
    // consider these in order
    val open = new PriorityQueue[Step]()
    val open_members = new HashSet[DirectedRoad]()
    // done evaluating these
    val visited = new HashSet[DirectedRoad]()
    // best distance so far
    val costs = new HashMap[DirectedRoad, Double]()

    class Step(val road: DirectedRoad, heuristic: Double) extends Ordered[Step] {
      def cost = costs(road)
      def score = cost + heuristic
      def compare(other: Step) = other.score.compare(score)

      override def toString = "%.2f away, %.2f heuristic, consider %s".format(cost, heuristic, road)
    }

    // Start
    costs(from) = 0
    open.enqueue(new Step(from, from.start_pt.dist_to(goal_pt)))
    open_members += from
    backrefs(from) = null  // encode the start somehow

    // Main loop
    var first = true
    while (open.nonEmpty) {
      val step = open.dequeue
      visited += step.road
      open_members -= step.road

      // Are we there yet?
      if (step.road == to && !first) {
        var path: List[DirectedRoad] = Nil
        var at: Option[DirectedRoad] = Some(step.road)
        while (at.isDefined && at.get != null) {
          path = at.get :: path
          // clean as we go to break loops
          at = backrefs.remove(at.get)
        }
        // .tail would exclude 'from'
        return path
      }

      // Where can we go next?
      for (next <- step.road.leads_to) {
        if ((loop && next == from) || !visited.contains(next)) {
          val tentative_cost = costs(step.road) + step.road.length

          // TODO costs => open_members?
          if (!open_members.contains(next) || tentative_cost < costs(next)) {
            backrefs(next) = step.road
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
  def pathfind_astar(from: Edge, to: Edge): List[DirectedRoad] =
    pathfind_astar(from.directed_road, to.directed_road)
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
