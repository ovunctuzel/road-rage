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

    new_fix_ids(graph3, remap_lines._1, remap_lines._2)
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
    val w = fix_ids(graph, output)
    graph.serialize(w)
    w.done()

    return output
  }

  private def new_fix_ids(graph: PreGraph3, first_lines: Map[EdgeID, Line], last_lines: Map[EdgeID, Line]) {
    val vertices = (for ((v, id) <- graph.vertices.zipWithIndex)
      yield v.id -> new VertexID(id)
    ).toMap
    val roads = (for ((r, id) <- graph.roads.zipWithIndex)
      yield r.id -> new RoadID(id)
    ).toMap

    graph.vertices = graph.vertices.map(old => {
      val v = new Vertex(old.location, vertices(old.id))
      v.turns ++= old.turns
      v
    })
    graph.roads = graph.roads.map(old => {
      val r = new Road(
        roads(old.id), old.dir, old.length, old.name, old.road_type, old.osm_id,
        vertices(old.v1.id), vertices(old.v2.id), old.points
      )
      r.houses ++= old.houses
      r.shops ++= old.shops
      r
    })
    graph.edges = graph.edges.map(old => {
      val lines = if (old.lines.size > 1) {
        val l1 = first_lines.getOrElse(old.id, old.lines.head)
        val l2 = old.lines.tail.dropRight(1)
        val l3 = last_lines.getOrElse(old.id, old.lines.last)
        Array(l1) ++ l2 ++ Array(l3)
      } else {
        Array(first_lines.getOrElse(old.id, old.lines.head))
      }
      new Edge(old.id, roads(old.road.id), old.lane_num, lines)
    })
  }

  private def fix_ids(graph: Graph, fn: String): MapStateWriter = {
    val edges = (for ((e, raw_id) <- graph.edges.zipWithIndex)
      yield e.id -> new EdgeID(raw_id)
    ).toMap

    return new MapStateWriter(fn, edges)
  }
}

// TODO nuke this thing.
// Since we propagate a StateWriter to serialize maps anyway, push the re-mapping of IDs with it.
// TODO dont extend a BinaryStateWriter specifically.
// TODO ideally, have methods for each type of id, and override them.
class MapStateWriter(
  fn: String, val edges: Map[EdgeID, EdgeID]
) extends BinaryStateWriter(fn)
// TODO make turn IDs contig too
