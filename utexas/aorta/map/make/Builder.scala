// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import utexas.aorta.map.Graph

import utexas.aorta.common.Util

object Builder {
  def main(args: Array[String]) {
    if (args.size != 1) {
      throw new Exception("Pass in only a single .osm filename")
    }
    convert(args.head)
  }

  // Takes a .osm and returns a .map
  def convert(input: String): String = {
    if (!input.endsWith(".osm")) {
      throw new Exception(s"$input must end with .osm")
    }
    val output = input.replace("osm/", "maps/").replace(".osm", ".map")
    val bldgs = new BuildingScraper()

    val pass1 = new Pass1(input)
    bldgs.scrape(pass1.osm)
    val graph1 = pass1.run()
    Graph.set_params(graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale)
    val graph2 = new Pass2(graph1).run()
    val graph3 = new Pass3(graph2).run()
    bldgs.group(graph3)

    val graph = new Graph(
      graph3.roads.toArray, graph3.edges.toArray, graph3.vertices.toArray,
      graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale,
      input.replace(".osm", "").replace("osm/", "")
    )
    Util.log(
      s"Dumping map with ${graph3.roads.length} roads, ${graph3.edges.length}" +
      s" edges, and ${graph3.vertices.length} vertices"
    )
    Util.mkdir("maps")
    val w = Util.writer(output)
    graph.serialize(w)
    w.done()

    return output
  }
}
