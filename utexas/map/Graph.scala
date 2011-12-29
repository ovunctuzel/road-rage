package utexas.map

import scala.collection.mutable.{HashMap, PriorityQueue}

import utexas.map.make.Reader

import utexas.Util

class Graph(val roads: List[Road], val edges: List[Edge],
            val vertices: List[Vertex], val wards: List[Ward],
            val special_ward: Ward, val width: Double, val height: Double)
{
  // Tell road about their ward
  for (w <- special_ward :: wards) {
    w.roads.foreach(r => r.ward = w)
  }

  // also fixed constraints: residential types and decent length
  // Note this one of those scary things that might not return
  def random_edge_except(except: Set[Edge]): Edge = {
    val min_len = 1.0 // TODO cfg. what unit is this in?
    val e = Util.choose_rand(edges)
    if (!except.contains(e) && e.road.road_type == "residential" && e.length > min_len) {
      return e
    } else {
      return random_edge_except(except)
    }
  }

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

    def collect_path(at: Traversable): List[Traversable] = at match {
      case null => List()
      case _    => {
        // clean as we go to break loops
        val prev = visited.remove(at).get
        collect_path(prev) ++ List(at)
      }
    }

    // Start
    open.enqueue(new Step(from, 0))
    visited(from) = null  // some way of encoding the start

    // Main loop
    var first = true
    while (!open.isEmpty) {
      val step = open.dequeue

      // Are we there yet?
      if (step.on == to && !first) {
        // Don't return the first step as 'from'
        return collect_path(step.on).tail
      }

      // Where can we go next?
      for (next <- step.on.leads_to) {
        if ((loop && next == from) || !visited.contains(next)) {
          // TODO do we have to relax / handle finding a shorter path?
          // TODO there are probably better heuristics than euclid
          val heuristic = next.start_pt.euclid_dist(goal_pt)
          open.enqueue(new Step(next, step.dist + next.length + heuristic))
          visited(next) = step.on
        }
      }

      first = false
    }

    // We didn't find the way?!
    throw new Exception("Couldn't A* from " + from + " to " + to)
  }
}

object Graph {
  def load(fn: String) = (new Reader(fn)).load_map
}
