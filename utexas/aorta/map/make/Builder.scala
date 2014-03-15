// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import utexas.aorta.map.{Graph, Edge, Line, Vertex, Road}
import utexas.aorta.common.{Util, EdgeID, VertexID, TurnID, RoadID, BinaryStateWriter}

import scala.collection.mutable

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

    // Pass 1
    val pass1 = new Pass1(input)
    bldgs.scrape(pass1.osm)
    val graph1 = pass1.run()
    bldgs.normalize_coords(graph1.fix _)
    Graph.set_params(graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale)

    // Pass 2
    val graph2 = new PreGraph2(graph1)
    val artifacts = new Pass2_Part2(graph2).run()
    new Pass2_Part3(graph2).run()

    // Pass 3
    val graph3 = new PreGraph3(graph2)
    new Pass3_Part2(graph3).run()
    new Pass3_Part3(graph3).run()
    val remap_lines = new Pass3_Part4(graph3).run()
    bldgs.group(graph3)

    new_fix_ids(graph3)
    val graph = new Graph(
      graph3.roads.toArray, graph3.edges.toArray, graph3.vertices.toArray, artifacts,
      graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale,
      input.replace(".osm", "").replace("osm/", "")
    )
    Util.log(
      s"Dumping map with ${graph3.roads.length} roads, ${graph3.edges.length}" +
      s" edges, and ${graph3.vertices.length} vertices"
    )
    Util.mkdir("maps")
    val w = fix_ids(graph, output, remap_lines._1, remap_lines._2)
    graph.serialize(w)
    w.done()

    return output
  }

  private def new_fix_ids(graph: PreGraph3) {
    val vertices = (for ((v, id) <- graph.vertices.zipWithIndex)
      yield v.id -> new VertexID(id)
    ).toMap
    graph.vertices = graph.vertices.map(old => {
      val v = new Vertex(old.location, vertices(old.id))
      v.turns ++= old.turns
      v
    })
    graph.roads = graph.roads.map(old => {
      val r = new Road(
        old.id, old.dir, old.length, old.name, old.road_type, old.osm_id, vertices(old.v1.id),
        vertices(old.v2.id), old.points
      )
      r.houses ++= old.houses
      r.shops ++= old.shops
      r
    })
  }

  private def fix_ids(graph: Graph, fn: String, first: Map[Edge, Line], last: Map[Edge, Line]): MapStateWriter = {
    val edges = (for ((e, raw_id) <- graph.edges.zipWithIndex)
      yield e.id -> new EdgeID(raw_id)
    ).toMap
    val roads = (for ((r, id) <- graph.roads.zipWithIndex)
      yield r.id -> new RoadID(id)
    ).toMap

    return new MapStateWriter(fn, edges, roads, first, last)
  }
}

// TODO nuke this thing.
// Since we propagate a StateWriter to serialize maps anyway, push the re-mapping of IDs with it.
// TODO dont extend a BinaryStateWriter specifically.
// TODO ideally, have methods for each type of id, and override them.
class MapStateWriter(
  fn: String, val edges: Map[EdgeID, EdgeID],
  val roads: Map[RoadID, RoadID], val first_lines: Map[Edge, Line], val last_lines: Map[Edge, Line]
) extends BinaryStateWriter(fn)
// TODO make turn IDs contig too
