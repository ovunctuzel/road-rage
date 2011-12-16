package map.make

object Builder {
  val default_fn = "dat/btr.osm"

  // generate a .map from a .osm
  def main(args: Array[String]) {
    // TODO usage info
    // Input that can be specified, with defaults:
    var fn = default_fn
    var show_dead = true

    for (arg <- args) {
      arg match {
        case "--show-dead" => { show_dead = true }
        case "--nuke-dead" => { show_dead = false }
        case s             => { fn = s }
      }
    }

    println("Processing " + fn)

    // first, let's take osm to an undirected graph with vertex intersections
    // and entire-road edges.
    val graph1 = new Pass1(fn).run()

    // then split roads so that edges are between just two vertices
    val graph2 = new Pass2(graph1).run()

    // now split edges into multiple directed lanes and connect them with some
    // smart intersections
    val graph3 = new Pass3(graph2).run(show_dead)

    // TODO better output location?
    // TODO single tags look sucky
    // TODO could new xml.PrettyPrinter(80, 2).format(node), but eh
    println(
      "Dumping map with %d roads, %d edges, and %d vertices".format(
        graph3.roads.length, graph3.edges.length, graph3.vertices.length
    ))
    xml.XML.save("dat/test.map", graph3.to_xml(graph1))
  }
}
