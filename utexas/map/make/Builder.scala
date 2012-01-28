package utexas.map.make

import java.io.FileWriter

import utexas.Util

object Builder {
  val default_ifn = "dat/btr.osm"
  val default_ofn = "dat/test.map"

  // generate a .map from a .osm
  def main(args: Array[String]) {
    // TODO usage info
    // Input that can be specified, with defaults:
    var ifn = default_ifn
    var ofn = default_ofn
    var show_dead = false
    
    if (args.size % 2 != 0) {
      Util.log("Command-line parameters must be pairs of key => value")
    }
    val keys = args.zipWithIndex.filter(p => p._2 % 2 == 0).map(p => p._1)
    val vals = args.zipWithIndex.filter(p => p._2 % 2 == 1).map(p => p._1)

    for ((key, value) <- keys.zip(vals)) {
      key match {
        case "--show-dead" => { show_dead = value == "1" }
        case "--input"     => { ifn = value }
        case "--output"    => { ofn = value }
        case _             => { println("wtf") }
      }
    }

    Util.log("Processing " + ifn)
    Util.log_push

    // first, let's take osm to an undirected graph with vertex intersections
    // and entire-road edges.
    val graph1 = new Pass1(ifn).run()

    // then split roads so that edges are between just two vertices
    val graph2 = new Pass2(graph1).run()

    // now split edges into multiple directed lanes and connect them with some
    // smart intersections
    val graph3 = new Pass3(graph2).run(show_dead)
    Util.log_pop

    // TODO better output location?
    Util.log(
      "Dumping map with %d roads, %d edges, and %d vertices".format(
        graph3.roads.length, graph3.edges.length, graph3.vertices.length
    ))
    val out = new FileWriter(ofn)
    graph3.to_xml(out, graph1)
    out.close
  }
}
