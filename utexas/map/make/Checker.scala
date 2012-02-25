package utexas.map.make

import utexas.map.Graph
import utexas.Util

object Checker {
  def main(args: Array[String]) = {
    val g = Graph.load("dat/test.map")

    check_edges(g)

    check_turn_conflicts(g)

    //check_turn_length(g)

    // TODO check connectivity more stringently? really not needed; I trust
    // Tarjan's.

    // TODO check for self-loops
  }

  // This causes agents to "float" over other the map as they chase an edge far
  // away
  def check_turn_length(g: Graph) = {
    // TODO check for NaN too
    Util.log("Checking length of turns")
    Util.log_push

    val max = 30.0  // TODO cfg
    for (t <- g.turns.sortBy(t => t.length) if t.length > max) {
      Util.log(t + " is long: " + t.length)
    }

    Util.log_pop
  }

  // This is a weak, but fast, way to measure connectivity
  def check_edges(g: Graph) = {
    Util.log("Checking edges for connectivity and length")
    Util.log_push

    for (e <- g.edges) {
      if (e.next_turns.size == 0) {
        Util.log(e + " leads nowhere")
      }
      if (e.prev_turns.size == 0) {
        Util.log("Nothing leads to " + e)
      }

      // Haha, we end up with some REALLY short edges sometimes...
      if (e.length == 0.0 || e.length.isNaN) {
        Util.log(e + " has length " + e.length)
        Util.log("  Points: " + e.road.points)
      }
    }

    Util.log_pop
  }

  def check_turn_conflicts(g: Graph) = {
    // conflicts should be symmetric
    for (e <- g.edges) {
      for (t1 <- e.next_turns) {
        for (t2 <- t1.conflicts) {
          if (!t2.conflicts(t1)) {
            Util.log("Asymmetric turn conflict between " + t1 + " and " + t2)
          }
        }
      }
    }
  }
}
