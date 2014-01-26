// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable

// TODO perf bug: I think one of the sets calls toString! test with a slow toString
object AStar {
  // T is the node type
  // TODO All costs are pairs of doubles lexicographically ordered right now. Generalize.
  def path[T](
    start: T, goals: Set[T], successors: (T) => Iterable[T],
    calc_cost: (T, T, (Double, Double)) => (Double, Double),
    calc_heuristic: (T) => (Double, Double),
    add_cost: ((Double, Double), (Double, Double)) => (Double, Double) =
      (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
    allow_cycles: Boolean = false
  ): List[T] = {
    if (goals.contains(start) && !allow_cycles) {
      return Nil
    }

    // Stitch together our path
    val backrefs = new mutable.HashMap[T, T]()
    // We're finished with these
    val visited = new mutable.HashSet[T]()
    // Best cost so far
    val costs = new mutable.HashMap[T, (Double, Double)]()

    val open = new PriorityQueue[T]()
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    costs(start) = (0, 0)
    open.insert(start, calc_heuristic(start))
    // Indicate start in backrefs by not including it

    while (open.nonEmpty) {
      val current = open.shift()
      if (!allow_cycles || current != start) {
        visited += current
      }

      // If backrefs doesn't have goal, allow_cycles is true and we just started
      if (goals.contains(current) && goals.intersect(backrefs.keys.toSet).nonEmpty) {
        // Reconstruct the path
        val path = new mutable.ListBuffer[T]()
        var pointer: Option[T] = Some(current)
        while (pointer.isDefined) {
          path.prepend(pointer.get)
          // Clean as we go to break loops
          pointer = backrefs.remove(pointer.get)
        }
        // Exclude 'start'
        return path.tail.toList
      } else {
        for (next_state <- successors(current)) {
          val tentative_cost = add_cost(
            costs(current), calc_cost(current, next_state, costs(current))
          )
          if (!visited.contains(next_state) && (!open.contains(next_state) || ordering_tuple.lt(tentative_cost, costs(next_state)))) {
            backrefs(next_state) = current
            costs(next_state) = tentative_cost
            // if they're in open_members, modify weight in the queue? or
            // new step will clobber it. fine.
            open.insert(next_state, add_cost(tentative_cost, calc_heuristic(next_state)))
          }
        }
      }
    }

    throw new Exception("Couldn't A* from " + start + " to " + goals)
  }

  // TODO make a way to build calls to these easily
}

// TODO generalize score.
class PriorityQueue[T]() {
  private case class Item(item: T, weight: (Double, Double))

  private val pq = new mutable.PriorityQueue[Item]()(
    Ordering[(Double, Double)].on((item: Item) => item.weight).reverse
  )
  private val members = new mutable.HashSet[T]()

  def insert(item: T, weight: (Double, Double)) {
    pq.enqueue(Item(item, weight))
    members += item
  }

  def shift(): T = {
    val item = pq.dequeue().item
    members -= item // TODO not true if there are multiples!
    return item
  }

  // TODO have a change_weight.

  def contains(item: T) = members.contains(item)
  def nonEmpty = pq.nonEmpty
}
