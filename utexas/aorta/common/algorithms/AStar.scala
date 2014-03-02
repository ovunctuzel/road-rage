// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable
import java.util

import utexas.aorta.common.Util

case class Pathfind[T](
  start: T = null,
  goals: Set[T] = null,
  successors: (T) => Iterable[T] = null,
  calc_cost: (T, T, (Double, Double)) => (Double, Double) = null,
  calc_heuristic: (T) => (Double, Double) = (node: T) => (0.0, 0.0),
  add_cost: ((Double, Double), (Double, Double)) => (Double, Double) =
    (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2),
  allow_cycles: Boolean = false,
  cost_start: (Double, Double) = (0, 0),
  banned_nodes: Set[T] = Set[T]()
) {
  def first_succs(succs: Iterable[T]) = this.copy(
    successors = (state: T) => if (state == start) succs else successors(state)
  )
}

// TODO perf bug: I think one of the sets calls toString! test with a slow toString
object AStar {
  // T is the node type
  // TODO All costs are pairs of doubles lexicographically ordered right now. Generalize.
  // The result is the path, then the map from node to cost
  def path[T](spec: Pathfind[T]): (List[T], Map[T, (Double, Double)]) = {
    //Util.assert_eq(banned_nodes.contains(start), false)
    if (spec.banned_nodes.contains(spec.start)) {
      throw new IllegalArgumentException(s"A* from ${spec.start}, but ban ${spec.banned_nodes}")
    }
    Util.assert_eq(spec.banned_nodes.intersect(spec.goals).isEmpty, true)
    if (spec.goals.contains(spec.start) && !spec.allow_cycles) {
      return (List(spec.start), Map())
    }

    // Stitch together our path
    val backrefs = new mutable.HashMap[T, T]()
    // We're finished with these
    val visited = new mutable.HashSet[T]()
    // Best cost so far
    val costs = new mutable.HashMap[T, (Double, Double)]()

    val open = new JavaPriorityQueue[T]()
    val ordering_tuple = Ordering[(Double, Double)].on((pair: (Double, Double)) => pair)

    costs(spec.start) = spec.cost_start
    open.insert(spec.start, spec.calc_heuristic(spec.start))
    // Indicate start in backrefs by not including it

    while (open.nonEmpty) {
      val current = open.shift()
      if (!spec.allow_cycles || current != spec.start) {
        visited += current
      }

      // If backrefs doesn't have goal, allow_cycles is true and we just started
      if (spec.goals.contains(current) && spec.goals.intersect(backrefs.keys.toSet).nonEmpty) {
        // Reconstruct the path
        val path = new mutable.ListBuffer[T]()
        var pointer: Option[T] = Some(current)
        while (pointer.isDefined) {
          path.prepend(pointer.get)
          // Clean as we go to break loops
          pointer = backrefs.remove(pointer.get)
        }
        // Include 'start'
        return (path.toList, costs.toMap)
      } else {
        for (next_state <- spec.successors(current) if !spec.banned_nodes.contains(next_state)) {
          val tentative_cost = spec.add_cost(
            costs(current), spec.calc_cost(current, next_state, costs(current))
          )
          if (!visited.contains(next_state) && (!open.contains(next_state) || ordering_tuple.lt(tentative_cost, costs(next_state)))) {
            backrefs(next_state) = current
            costs(next_state) = tentative_cost
            // if they're in open_members, modify weight in the queue? or
            // new step will clobber it. fine.
            open.insert(next_state, spec.add_cost(tentative_cost, spec.calc_heuristic(next_state)))
          }
        }
      }
    }

    throw new Exception(s"Couldn't A* from ${spec.start} to ${spec.goals}")
  }
}

abstract class PriorityQueue[T]() {
  def insert(item: T, weight: (Double, Double))
  def shift(): T
  def contains(item: T): Boolean
  def nonEmpty(): Boolean

  // TODO have a change_weight.
}

// TODO generalize score.
class ScalaPriorityQueue[T]() extends PriorityQueue[T] {
  private case class Item(item: T, weight: (Double, Double))

  private val pq = new mutable.PriorityQueue[Item]()(
    Ordering[(Double, Double)].on((item: Item) => item.weight).reverse
  )
  private val members = new mutable.HashSet[T]()

  override def insert(item: T, weight: (Double, Double)) {
    pq.enqueue(Item(item, weight))
    members += item
  }

  override def shift(): T = {
    val item = pq.dequeue().item
    members -= item // TODO not true if there are multiples!
    return item
  }

  override def contains(item: T) = members.contains(item)
  override def nonEmpty = pq.nonEmpty
}

class JavaPriorityQueue[T]() extends PriorityQueue[T] {
  private case class Item(item: T, weight_1: Double, weight_2: Double)

  private val pq = new util.PriorityQueue[Item](100, new util.Comparator[Item]() {
    override def compare(a: Item, b: Item) =
      if (a.weight_1 < b.weight_1)
        -1
      else if (a.weight_1 > b.weight_1)
        1
      else if (a.weight_2 < b.weight_2)
        -1
      else if (a.weight_2 > b.weight_2)
        1
      else
        0
  })
  private val members = new mutable.HashSet[T]()

  override def insert(item: T, weight: (Double, Double)) {
    pq.add(Item(item, weight._1, weight._2))
    members += item
  }

  override def shift(): T = {
    val item = pq.poll().item
    members -= item // TODO not true if there are multiples!
    return item
  }

  override def contains(item: T) = members.contains(item)
  override def nonEmpty = !pq.isEmpty
}
