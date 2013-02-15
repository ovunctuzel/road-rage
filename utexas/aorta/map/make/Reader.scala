// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.io.Source

import scala.collection.mutable.{MutableList, HashMap, ArrayBuffer}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.map.{Coordinate, Road, Vertex, Edge, Direction, Line,
                   Turn, Graph}
import utexas.aorta.sim.{Simulation, Scenario}

import utexas.aorta.{Util, cfg}

abstract class Reader(fn: String, with_geometry: Boolean) {
  // we know all of this as soon as we read the graph tag...
  var vertLinks: Array[Array[TmpLink]] = null

  // per graph
  var roads: Array[Road] = null
  var edges: Array[Edge] = null
  var verts: Array[Vertex] = null

  class TmpLink(val id: Int, val from: Int, val to: Int, val length: Double, val conflict_line: Line)

  def load_map() = load match {
    case (r, e, v) => new Graph(r, e, v, fn)
  }
  def load_simulation(scenario: Scenario) = load match {
    case (r, e, v) => new Simulation(r, e, v, scenario)
  }

  // Side effect of populating the per-graph data.
  def read_file()

  private def load(): (Array[Road], Array[Edge], Array[Vertex]) = {
    Util.log("Loading map " + fn)
    Util.log_push
    read_file
    Util.log_pop

    // turns just want edges, doesn't matter what trait they're endowed with
    for (v <- verts) {
      for (link <- vertLinks(v.id)) {
        // TODO in principle, these should be equivalent, but it doesnt cost us
        // extra time/memory to use this always, and its at least consistent
        // until some weird bugs are hashed out.
        val t = new Turn(link.id, edges(link.from), edges(link.to), link.length, link.conflict_line)
        if (with_geometry) {
          t.set_lines(Array(new Line(t.from.lines.last.end, t.to.lines.head.start)))
        }
        v.turns = t :: v.turns
        /*if (with_geometry) {
          v.turns = Turn(link.id, edges(link.from), edges(link.to)) :: v.turns
          Util.assert_eq(link.length, v.turns.head.length)
        } else {
          v.turns = new Turn(link.id, edges(link.from), edges(link.to), link.length, link.conflict_line) :: v.turns
        }*/
      }
    }

    return (roads, edges, verts)
  }
}

class BinaryReader(fn: String, with_geometry: Boolean) extends Reader(fn, with_geometry)
{
  override def read_file() = {
    val graph = Util.unserialize(fn).asInstanceOf[MkGraph]
    // TODO set_params is a dumb idea.
    Graph.set_params(graph.width, graph.height, graph.offX, graph.offY,
                     graph.scale)

    // Vertices
    verts = graph.vertices.map(v => new Vertex(new Coordinate(v.x, v.y), v.id))
    vertLinks = new Array[Array[TmpLink]](verts.size)
    graph.vertices.foreach(v => {
      vertLinks(v.id) = v.turns.map(t => new TmpLink(
        t.id, t.from, t.to, t.length, new Line(
          t.conflict_line.x1, t.conflict_line.y1, t.conflict_line.x2,
          t.conflict_line.y2
        )
      ))
    })

    // Roads
    roads = graph.roads.map(r => new Road(
      r.id, r.length, r.name, r.road_type, r.osm_id, verts(r.v1), verts(r.v2)
    ))
    if (with_geometry) {
      graph.roads.foreach(r => {
        roads(r.id).set_points(r.points.map(
          pt => new Coordinate(pt.x, pt.y)
        ))
      })
    }
    // TODO so why MkRoad at all? ;)

    // Edges
    edges = graph.edges.map(e => new Edge(e.id, roads(e.road), e.dir))
    graph.edges.foreach(dat => {
      val e = edges(dat.id)
      e.lane_num = dat.lane_num
      if (with_geometry) {
        e.set_lines(dat.lines.map(l => new Line(l.x1, l.y1, l.x2, l.y2)))
        Util.assert_eq(dat.length, e.length)
      } else {
        e.set_length(dat.length)
      }

      // TODO nonsense.
      if (e.length <= cfg.min_lane_length) {
        e.length = 0.1
      }

      Util.assert_eq(e.other_lanes.length, e.lane_num)
      e.other_lanes += e
    })
  }
}
