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

    // Build the graph. Vertices are our vertices, and edges are our roads
    // (oneway or not). Cost of an edge is road length. For now, discard turn
    // length.
    println("Building GraphHopper graph...")
    val graph = new LevelGraphStorage(
      new RAMDirectory(s"maps/route_${map.name}", true)
    )
    graph.createNew(map.directed_roads.size)
    for (r <- map.roads) {
      graph.edge(r.v1.id, r.v2.id, r.length, !r.is_oneway)
    }

    // Precompute a contraction hierarchy.
    println("Computing contraction hierarchy...")
    val timer = Common.timer("CH computation")
    val prepare = new PrepareContractionHierarchies().graph(graph)
    prepare.doWork
    timer.stop

    // Serialize it.
    println("Saving...")
    graph.flush
  }
}
