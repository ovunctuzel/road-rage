// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.analysis

import scala.collection.mutable.{PriorityQueue, HashSet}

import utexas.aorta.Util

abstract class AbstractEdge {
  var id: Int // TODO var because fix_ids
  def length: Double
  // List and not Set means there could be multiple transitions, with possibly
  // different weights.
  def succs: Seq[(AbstractEdge, Double)]
  def preds: Seq[(AbstractEdge, Double)]
}

object AbstractGraph {
  // Precomputes a table of the cost from source to everything.
  def dijkstras(size: Int, source: AbstractEdge,
                next: AbstractEdge => Seq[(AbstractEdge, Double)]): Array[Double] =
  {
    val costs = Array.fill[Double](size)(Double.PositiveInfinity)

    // TODO needs tests!
    // TODO pass in a comparator to the queue instead of having a wrapper class
    class Step(val edge: AbstractEdge) extends Ordered[Step] {
      def cost = costs(edge.id)
      def compare(other: Step) = other.cost.compare(cost)
    }

    // Edges in the open set don't have their final distance yet
    val open = new PriorityQueue[Step]()
    val done = new HashSet[AbstractEdge]()

    costs(source.id) = 0
    open.enqueue(new Step(source))

    while (open.nonEmpty) {
      val step = open.dequeue
      
      // Skip duplicate steps, since we chose option 3 for the problem below.
      if (!done.contains(step.edge)) {
        done += step.edge

        for ((next, transition_length) <- next(step.edge) if !done.contains(next)) {
          val dist = step.cost + transition_length + next.length
          if (dist < costs(next.id)) {
            // Relax!
            costs(next.id) = dist
            // TODO ideally, decrease-key
            // 1) get a PQ that uses a fibonacci heap
            // 2) remove, re-insert again
            // 3) just insert a dupe, then skip the duplicates when we get to them
            // Opting for 3, for now.
            open.enqueue(new Step(next))
          }
        }
      }
    }
    return costs
  }

  // Starts at source, hillclimbs to lower costs, and returns the path to 0.
  def hillclimb(costs: Array[Double], start: AbstractEdge): List[AbstractEdge] =
    costs(start.id) match {
      case 0 => start :: Nil
      case c => start :: hillclimb(
        costs, start.succs.minBy(t => costs(t._1.id))._1
      )
    }

  // TODO refactor, and return the reverse, since thats what client has to do.
  def hillclimb_backwards(costs: Array[Double], end: AbstractEdge): List[AbstractEdge] =
    costs(end.id) match {
      case 0 => end :: Nil
      case c => end :: hillclimb_backwards(
        costs, end.preds.minBy(t => costs(t._1.id))._1
      )
    }
}
