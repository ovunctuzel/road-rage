// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable

// T is the node type
// TODO generalize cost passed in
// This is unused and untested right now
class DStarLite[T](
  start: T, goal: T, successors: (T) => Seq[T], predecessors: (T) => Seq[T],
  c: (T, T) => Double, h: (T, T) => Double
) {
  /*private var last_state = start
  private var at = start
  private var k_m = 0.0
  private val g = new mutable.HashMap[T, Double]().withDefaultValue(Double.PositiveInfinity)
  private val rhs = new mutable.HashMap[T, Double]().withDefaultValue(Double.PositiveInfinity)
  rhs(goal) = 0
  private val open = new PQ[T]()
  open.insert(goal, (h(start, goal), 0))
  // Do this to serve the first query.
  compute_path()

  private def calculate_key(state: T) = (
    math.min(g(state), rhs(state)) + h(at, state) + k_m,
    math.min(g(state), rhs(state))
  )

  private def update_vertex(state: T) {
    if (g(state) == rhs(state)) {
      if (open.contains(state)) {
        open.remove(state)
      }
    } else {
      if (open.contains(state)) {
        open.change_priority(state, calculate_key(state))
      } else {
        open.add(state, calculate_key(state))
      }
    }
  }

  private def compute_path() {
    while (open.top_score() < calculate_key(at) || rhs(at) > g(at)) {
      val u = open.peek_top()
      val k_old = u.top_score()
      val k_new = calculate_key(u)
      if (k_old < k_new) {
        open.change_priority(u, k_new)
      } else if (g(u) > rhs(u)) {
        g(u) = rhs(u)
        open.remove(u)
        for (s <- predecessors(u)) {
          if (s != goal) {
            rhs(s) = math.min(rhs(s), c(s, u) + g(u))
          }
          update_vertex(s)
        }
      } else {
        val g_old = g(u)
        g(u) = Double.PositiveInfinity
        for (s <- predecessors(u) ++ List(u)) {
          if (rhs(s) == c(s, u) + g_old) {
            if (s != goal) {
              rhs(s) = successors(s).map(sp => c(s, sp) + g(sp)).min
            }
          }
          update_vertex(s)
        }
      }
    }
  }

  def next_step(from: T): T = {
    if (rhs(at) == Double.PositiveInfinity) {
      throw new Exception("No path!")
    }
    at = successors(from).minBy(s => c(from, s) + g(s))
    return at
  }

  // the c() function should always return the latest cost.
  def update_world(current: T, changed_edges: List[(T, T, Double)]) {
    at = current
    k_m = k_m + h(last_state, at)
    last_state = at
    for ((u, v, c_old) <- changed_edges) {
      val new_cost = c(u, v)
      if (u != goal) {
        if (c_old > new_cost) {
          rhs(u) = math.min(rhs(u), c(u, v) + g(v))
        } else if (rhs(u) == c_old + g(v)) {
          rhs(u) = successors(u).map(sp => c(u, sp) + g(sp)).min
        }
      }
      update_vertex(u)
    }
    compute_path()
  }*/
}
