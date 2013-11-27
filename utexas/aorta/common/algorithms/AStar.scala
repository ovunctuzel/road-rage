// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable

object AStar {
  // T is the node type
  // TODO All costs are pairs of doubles lexicographically ordered right now. Generalize.
  def path[T](
    start: T, goal: T, successors: (T) => Seq[T], calc_cost: (T, T) => (Double, Double),
    calc_heuristic: (T, T) => (Double, Double),
    add_cost: ((Double, Double), (Double, Double)) => (Double, Double)
  ): List[T] = {
    if (start == goal) {
      return Nil
    }

    // Stitch together our path
    val backrefs = new mutable.HashMap[T, T]()
    // We're finished with these
    val visited = new mutable.HashSet[T]()
    // Best cost so far
    val costs = new mutable.HashMap[T, (Double, Double)]()

    case class Step(state: T) {
      lazy val heuristic = calc_heuristic(state, goal)
      def cost = add_cost(costs(state), heuristic)
    }
    val ordering = Ordering[(Double, Double)].on((step: Step) => step.cost).reverse
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    // Priority queue grabs highest priority first, so reverse to get lowest
    // cost first.
    val open = new mutable.PriorityQueue[Step]()(ordering)
    // Used to see if we've already added a road to the queue
    val open_members = new mutable.HashSet[T]()

    costs(start) = (0, 0)
    open.enqueue(Step(start))
    open_members += start
    // Indicate start in backrefs by not including it

    while (open.nonEmpty) {
      val current = open.dequeue()
      visited += current.state
      open_members -= current.state

      if (current.state == goal) {
        // Reconstruct the path
        val path = new mutable.ListBuffer[T]()
        var pointer: Option[T] = Some(current.state)
        while (pointer.isDefined) {
          path.prepend(pointer.get)
          // Clean as we go to break loops
          pointer = backrefs.remove(pointer.get)
        }
        // Exclude 'start'
        return path.tail.toList
      } else {
        for (next_state <- successors(current.state)) {
          val tentative_cost = add_cost(costs(current.state), calc_cost(current.state, next_state))
          if (!visited.contains(next_state) && (!open_members.contains(next_state) || ordering_tuple.lt(tentative_cost, costs(next_state)))) {
            backrefs(next_state) = current.state
            costs(next_state) = tentative_cost
            // TODO if they're in open_members, modify weight in the queue? or
            // new step will clobber it. fine.
            open.enqueue(Step(next_state))
            open_members += next_state
          }
        }
      }
    }

    throw new Exception("Couldn't A* from " + start + " to " + goal)
  }
}
