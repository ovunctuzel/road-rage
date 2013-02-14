// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import java.io.{File, FileWriter}

import utexas.aorta.map.{Graph, Direction}

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
    // TODO Move these methods to individual classes.
    val mk = MkGraph(
      graph1.width, graph1.height, graph1.offX, graph1.offY, graph1.scale,
      graph3.roads.map(r => MkRoad(
        r.name, r.road_type, r.osm_id, r.v1.id, r.v2.id, r.length, r.id,
        r.points.map(pt => MkCoordinate(pt.x, pt.y))
      )).toArray,
      graph3.edges.map(e => MkEdge(
        e.id, e.road.id, e.dir, e.lane_num, e.length,
        e.lines.map(l => MkLine(l.x1, l.y1, l.x2, l.y2))
      )).toArray,
      graph3.vertices.map(v => MkVertex(
        v.id, v.location.x, v.location.y, v.turns.map(t => MkTurn(
          t.id, t.from.id, t.to.id, t.length,
          MkLine(t.conflict_line.x1, t.conflict_line.y1, t.conflict_line.x2,
                 t.conflict_line.y2)
        )).toArray
      )).toArray
    )
    Util.serialize(mk, output)

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

// TODO Spread these out, or directly associate them with their respective
// classes?

case class MkGraph(
  width: Double, height: Double, offX: Double, offY: Double, scale: Double,
  roads: Array[MkRoad], edges: Array[MkEdge], vertices: Array[MkVertex]
)

case class MkRoad(
  name: String, road_type: String, osm_id: Int, v1: Int, v2: Int,
  length: Double, id: Int, points: Array[MkCoordinate]
)

case class MkEdge(
  id: Int, road: Int, dir: Direction.Direction, lane_num: Int, length: Double,
  lines: Array[MkLine]
)

case class MkVertex(id: Int, x: Double, y: Double, turns: Array[MkTurn])

case class MkTurn(
  id: Int, from: Int, to: Int, length: Double, conflict_line: MkLine
)

case class MkCoordinate(x: Double, y: Double)
case class MkLine(x1: Double, y1: Double, x2: Double, y2: Double)
