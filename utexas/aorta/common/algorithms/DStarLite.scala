// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common.algorithms

import scala.collection.mutable

// T is the node type
// TODO All costs are pairs of doubles lexicographically ordered right now. Generalize.
class DStarLite[T](
  start: T, goal: T, successors: (T) => Seq[T],
  calc_cost: (T, T, (Double, Double)) => (Double, Double),
  calc_heuristic: (T, T) => (Double, Double),
  add_cost: ((Double, Double), (Double, Double)) => (Double, Double)
) {
  /*private var last_state = start
  private var at = start
  private var k_m = 0.0
  private val g = new mutable.HashMap[T, Double]().withDefaultValue(Double.PositiveInfinity)
  private val rhs = new mutable.HashMap[T, Double]().withDefaultValue(Double.PositiveInfinity)
  rhs(goal) = 0
  // TODO empty u, i think its the priority queue
  // TODO add to open... goal with score (calc_heuristic(start, goal)._2, 0)
  // Do this to serve the first query.
  compute_path()

  private def calculate_key(state: T) = (
    math.min(g(state), rhs(state)) + calc_heuristic(at, state)._2 + k_m,
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
      val u = open.top()
      val k_old = u.top_score()
      val k_new = calculate_key(u)
      if (k_old < k_new) {
        open.change_priority(u, k_new)
      } else if (g(u) > rhs(u)) {
        g(u) = rhs(u)
        open.remove(u)
        for (s <- predecessors(u)) {
          if (s != goal) {
            rhs(s) = math.min(rhs(s), calc_cost(s, u)._2 + g(u))
          }
          update_vertex(s)
        }
      } else {
        val g_old = g(u)
        g(u) = Double.PositiveInfinity
        for (s <- predecessors(u) ++ List(u)) {
          if (rhs(s) == calc_cost(s, u)._2 + g_old) {
            if (s != goal) {
              rhs(s) = successors(s).map(s_prime => calc_cost(s, s_prime)._2 + g(s_prime)).min
            }
          }
          update_vertex(s)
        }
      }
    }
  }

  def next_step(from: T): T = {
    at = successors(from).minBy(s => c(from, s) + g(s))
    return at
  }

  // cost is on the t in s->t, so just one T. mmm I shouldn't try to parse pseudocode this late.
  def update_world(changed_costs: List[(T, Double)]) {
    // TODO assume at is fresh.
    k_m = k_m + calc_heuristic(last_state, at)
    last_state = at
    for ((v, new_cost) <- changed_costs) {
      val c_old = c(v)
      c(v) = new_cost
      // TODO finish the rest of this. it refers to u directly, so this could be tricky.
    }
  }*/
}
