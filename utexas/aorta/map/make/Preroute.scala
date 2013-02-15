// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import com.graphhopper.GraphHopper
import com.graphhopper.storage.{LevelGraphStorage, RAMDirectory}
import com.graphhopper.routing.ch.PrepareContractionHierarchies

import utexas.aorta.map.Graph

import utexas.aorta.{Util, Common}

object Preroute {
  def main(args: Array[String]) = {
    val map = Graph.load(args.head)

    // Build the graph. Vertices are directed roads and edges are turns, but we
    // can mangle the total cost for a turn as its incoming edge's length + its
    // length.
    println("Building GraphHopper graph...")
    val graph = new LevelGraphStorage(
      new RAMDirectory(s"maps/route_${map.name}", true)
    )
    graph.createNew(map.directed_roads.size)
    for (r1 <- map.directed_roads) {
      for ((r2, turn_length) <- r1.succs) {
        // TODO incoming edge length, or road length?
        graph.edge(r1.id, r2.id, r1.length + turn_length, false)
      }
    }

    // Precompute a contraction hierarchy.
    println("Computing contraction hierarchy...")
    val timer = Common.timer("making ch")
    val prepare = new PrepareContractionHierarchies().graph(graph)
    prepare.doWork
    timer.stop

    // Serialize it.
    println("Saving...")
    graph.flush
  }
}
