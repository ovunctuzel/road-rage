package utexas.map.make

import utexas.map.Graph
import utexas.Util

object Checker {
  def main(args: Array[String]) = {
    val g = Graph.load("dat/test.map")

    check_edges(g)

    check_turn_length(g)

    // TODO check connectivity

    // TODO check for self-loops
  }

  // This causes agents to "float" over other the map as they chase an edge far
  // away
  def check_turn_length(g: Graph) = {
    Util.log("Checking length of turns")
    Util.log_push

    val max = 30.0
    for (t <- g.turns.sortBy(t => t.length) if t.length > max) {
      Util.log(t + " is long: " + t.length)
    }

    Util.log_pop
  }

  // This is a weak, but fast, way to measure connectivity
  def check_edges(g: Graph) = {
    Util.log("Checking edges for connectivity")
    Util.log_push

    for (e <- g.edges) {
      if (e.next_turns.size == 0) {
        Util.log(e + " leads nowhere")
      }
      if (e.prev_turns.size == 0) {
        Util.log("Nothing leads to " + e)
      }
    }

    Util.log_pop
  }
}
