// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import java.io.{File, FileWriter}

import utexas.aorta.map.Graph

import utexas.aorta.Util

object Builder {
  // Takes a .osm and returns a .map
  def convert(input: String): String = {
    if (!input.endsWith(".osm")) {
      throw new Exception(s"$input must end with .osm")
    }

    val output = input.replace("osm/", "maps/").replace(".osm", ".map")

    val graph1 = new Pass1(input).run()
    Graph.set_params(graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale)

    val graph2 = new Pass2(graph1).run()

    val graph3 = new Pass3(graph2).run()

    Util.log(
      s"Dumping map with ${graph3.roads.length} roads, ${graph3.edges.length}" +
      s" edges, and ${graph3.vertices.length} vertices"
    )
    (new File("./maps")).mkdir
    val file = new FileWriter(output)
    graph3.to_plaintext(file, graph1)
    file.close

    return output
  }

  def main(args: Array[String]) {
    // TODO usage info
    // TODO take params more flexibly again

    if (args.size != 1) {
      throw new Exception("Pass in only a single .osm filename")
    }

    convert(args.head)
  }
}
