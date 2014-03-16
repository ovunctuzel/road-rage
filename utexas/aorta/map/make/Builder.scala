// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import utexas.aorta.map.{Graph, Edge, Line, Vertex, Road, Turn, Coordinate}
import utexas.aorta.common.{Util, EdgeID, VertexID, TurnID, RoadID, BinaryMagicWriter}

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
    val bldg_results = bldgs.group(graph3)

    fix_ids(graph3, remap_lines._1, remap_lines._2, bldg_results._1, bldg_results._2)
    val graph = new Graph(
      graph3.roads.toArray, graph3.edges.toArray, graph3.vertices.toArray, graph3.turns.toArray,
      artifacts, graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale,
      input.replace(".osm", "").replace("osm/", "")
    )
    Util.log(
      s"Dumping map with ${graph3.roads.length} roads, ${graph3.edges.length}" +
      s" edges, and ${graph3.vertices.length} vertices"
    )
    Util.mkdir("maps")
    val w = new BinaryMagicWriter(output)
    Graph.do_magic_save(graph, w)
    w.done()

    return output
  }

  private def fix_ids(
    graph: PreGraph3, first_lines: Map[EdgeID, Line], last_lines: Map[EdgeID, Line],
    houses: Map[Road, Array[Coordinate]], shops: Map[Road, Array[Coordinate]]
  ) {
    val vertices = (for ((v, id) <- graph.vertices.zipWithIndex)
      yield v.id -> new VertexID(id)
    ).toMap
    val roads = (for ((r, id) <- graph.roads.zipWithIndex)
      yield r.id -> new RoadID(id)
    ).toMap
    val edges = (for ((e, raw_id) <- graph.edges.zipWithIndex)
      yield e.id -> new EdgeID(raw_id)
    ).toMap
    val turns = (for ((t, raw_id) <- graph.turns.zipWithIndex)
      yield t.id -> new TurnID(raw_id)
    ).toMap

    graph.vertices = graph.vertices.map(v => new Vertex(v.location, vertices(v.id)))
    graph.turns = graph.turns.map(
      t => new Turn(turns(t.id), edges(t.from.id), edges(t.to.id), t.lines)
    )
    graph.roads = graph.roads.map(r => new Road(
      roads(r.id), r.dir, r.length, r.name, r.road_type, r.osm_id,
      vertices(r.v1.id), vertices(r.v2.id), r.points, houses(r), shops(r)
    ))
    graph.edges = graph.edges.map(old => {
      val lines = if (old.lines.size > 1) {
        val l1 = first_lines.getOrElse(old.id, old.lines.head)
        val l2 = old.lines.tail.dropRight(1)
        val l3 = last_lines.getOrElse(old.id, old.lines.last)
        Array(l1) ++ l2 ++ Array(l3)
      } else {
        Array(first_lines.getOrElse(old.id, old.lines.head))
      }
      new Edge(edges(old.id), roads(old.road.id), old.lane_num, lines)
    })
  }
}
