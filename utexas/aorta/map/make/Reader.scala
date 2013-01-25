// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

import scala.collection.mutable.{MutableList, HashMap, MultiMap, ArrayBuffer}
import scala.collection.mutable.{Set => MutableSet}

import utexas.aorta.map.{Coordinate, Road, Vertex, Edge, Direction, Line,
                   Turn, Graph, Ward}
import utexas.aorta.sim.Simulation

import utexas.aorta.{Util, cfg}
import utexas.aorta.analysis.Profiling  // TODO

abstract class Reader(fn: String, with_geometry: Boolean) {
  // we know all of this as soon as we read the graph tag...
  var vertLinks: Array[Array[TmpLink]] = null

  // per graph
  var roads: Array[Road] = null
  var edges: Array[Edge] = null
  var verts: Array[Vertex] = null

  val wards_map = new HashMap[Int, MutableSet[Road]] with MultiMap[Int, Road]
  var special_ward_id: Int = -1

  class TmpLink(val id: Int, val from: Int, val to: Int, val length: Double, val conflict_line: Line)

  def load_map() = load match {
    case (r, e, v, w, special_ward) => new Graph(r, e, v, w, special_ward)
  }
  def load_simulation() = load match {
    case (r, e, v, w, special_ward) => new Simulation(r, e, v, w, special_ward)
  }

  // Side effect of populating the per-graph data.
  def read_file()

  private def load(): (Array[Road], Array[Edge], Array[Vertex], List[Ward], Ward) = {
    Util.log("Loading map " + fn)
    Util.log_push
    read_file
    Util.log("")
    Util.log_pop

    Util.log("Adding references at intersections")
    // turns just want edges, doesn't matter what trait they're endowed with
    for (v <- verts) {
      for (link <- vertLinks(v.id)) {
        if (with_geometry) {
          v.turns = Turn(link.id, edges(link.from), edges(link.to)) :: v.turns
        } else {
          v.turns = new Turn(link.id, edges(link.from), edges(link.to), link.length, link.conflict_line) :: v.turns
        }

      }
    }

    Util.log("Recovering wards as well")
    // Don't forget to separate out the special ward first
    // Missing the special ward can happen on some oddly-constructed maps
    val special_ward = if (wards_map.contains(special_ward_id))
                         new Ward(special_ward_id, wards_map(special_ward_id).toSet)
                       else
                         new Ward(special_ward_id, Set())
    wards_map -= special_ward_id
    val wards: List[Ward] = wards_map.map(pair => new Ward(pair._1, pair._2.toSet)).toList

    return (roads, edges, verts, wards, special_ward)
  }
}

/*
class XMLReader(fn: String, with_geometry: Boolean) extends Reader(fn, with_geometry) {
  // TODO this is one nasty long routine...
  // and it changes behavior at a few places based on its parameter.
  override def read_file() = {
    val event_reader = new XMLEventReader( Source.fromFile(fn) )

    var ev_count = 0

    // TODO probably a better way to unpack than casting to string. also, .head
    // was when I didn't understand Option.
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text
    def has_attrib(attribs: MetaData, key: String) = attribs.get(key).isDefined
    def get_int(attribs: MetaData) = get_attrib(attribs, (_: String)).toInt
    def get_double(attribs: MetaData) = get_attrib(attribs, (_: String)).toDouble

    // per road
    var rd_name, rd_type: String = ""
    var rd_osm, rd_id, rd_ward, rd_v1, rd_v2: Int = -1
    var rd_len: Double = -1
    var rd_pts: MutableList[Coordinate] = null

    // per edge
    var e_id, e_rd, e_lane: Int = -1
    var e_length: Double = -1
    var e_dir: Direction.Direction = Direction.POS
    var e_lines = new MutableList[Line]

    // per vertex
    var v_id: Int = -1
    var v_x, v_y: Double = 0.0
    var v_links = new ArrayBuffer[TmpLink]

    event_reader.foreach(ev => {
      ev_count += 1
      if (ev_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print("\r" + Util.indent + "Processed %,d XML events".format(ev_count))
      }

      ev match {
        case EvElemStart(_, "graph", attribs, _) => {
          val width  = get_double(attribs)("width")
          val height = get_double(attribs)("height")
          val xoff = get_double(attribs)("xoff")
          val yoff = get_double(attribs)("yoff")
          val scale = get_double(attribs)("scale")
          // Set this as early as possible! length() of any lines we make needs
          // it.
          Graph.set_params(width, height, xoff, yoff, scale)

          special_ward_id = get_int(attribs)("special_ward")

          val num_roads = get_int(attribs)("roads")
          val num_edges = get_int(attribs)("edges")
          val num_verts = get_int(attribs)("verts")

          // set up all the temporary things to accumulate stuff
          roads = new Array[Road](num_roads)
          edges = new Array[Edge](num_edges)
          verts = new Array[Vertex](num_verts)

          vertLinks = new Array[Array[TmpLink]](num_verts)
        }

        case EvElemStart(_, "road", attribs, _) => {
          rd_name = get_attrib(attribs, "name")
          rd_type = get_attrib(attribs, "type")
          rd_osm = get_int(attribs)("osmid")
          rd_len = get_double(attribs)("length")
          rd_id = get_int(attribs)("id")
          rd_ward = get_int(attribs)("ward")
          rd_v1 = get_int(attribs)("v1")
          rd_v2 = get_int(attribs)("v2")
          rd_pts = new MutableList[Coordinate]
        }
        case EvElemEnd(_, "road") => {
          val road = new Road(
            rd_id, rd_len, rd_name, rd_type, rd_osm,
            // it's vital that we break the interdependence by dumping vertices
            // first
            verts(rd_v1), verts(rd_v2)
          )
          roads(rd_id) = road
          if (with_geometry) {
            road.set_points(rd_pts.toArray)
          }
          wards_map.addBinding(rd_ward, road)

          // reset stuff
          rd_name = ""
          rd_type = ""
          rd_osm = -1
          rd_len = -1
          rd_id = -1
          rd_ward = -1
          rd_v1 = -1
          rd_v2 = -1
          rd_pts.clear
        }

        case EvElemStart(_, "pt", attribs, _) => {
          rd_pts += new Coordinate(
            get_double(attribs)("x"), get_double(attribs)("y")
          )
        }

        case EvElemStart(_, "edge", attribs, _) => {
          e_id = get_int(attribs)("id")
          e_rd = get_int(attribs)("road")
          val dir = get_attrib(attribs, "dir")(0)
          e_lane = get_int(attribs)("laneNum")
          e_length = get_double(attribs)("length")

          if (dir == '+') {
            e_dir = Direction.POS
          } else {
            e_dir = Direction.NEG
          }

          e_lines = new MutableList[Line]
        }
        case EvElemEnd(_, "edge") => {
          val e = new Edge(e_id, roads(e_rd), e_dir)
          edges(e_id) = e
          e.lane_num = e_lane
          if (with_geometry) {
            e.set_lines(e_lines.toArray)
          } else {
            e.set_length(e_length)
          }
          // Sucks, but we have some REALLY small edges.
          if (e.length <= cfg.min_lane_length) {
            // Cheat. Agents NEED the ability to exist somewhere on this edge.
            //Util.log("WARNING " + e + " is tiny")
            e.length = 0.1
          }

          // tell the road about the edge, too
          // TODO edges in the xml will be ordered by their id, which we created
          // in order of lane numbering too
          Util.assert_eq(e.other_lanes.length, e.lane_num)
          e.other_lanes += e

          // reset
          e_id = -1
          e_rd = -1
          e_lane = -1
          e_length = -1
          e_dir = Direction.POS
          e_lines.clear
        }

        case EvElemStart(_, "line", attribs, _) => {
          val List(x1, y1, x2, y2) = List("x1", "y1", "x2", "y2") map get_double(attribs)
          e_lines += new Line(x1, y1, x2, y2)
        }

        case EvElemStart(_, "vertex", attribs, _) => {
          v_id = get_int(attribs)("id")
          v_x = get_double(attribs)("x")
          v_y = get_double(attribs)("y")

          v_links = new ArrayBuffer[TmpLink]
        }
        case EvElemEnd(_, "vertex") => {
          verts(v_id) = new Vertex(new Coordinate(v_x, v_y), v_id)
          vertLinks(v_id) = v_links.toArray

          // reset
          v_id = -1
          v_x = 0.0
          v_y = 0.0
          v_links.clear
        }

        case EvElemStart(_, "link", attribs, _) => {
          v_links += new TmpLink(
            get_int(attribs)("id"), get_int(attribs)("from"), get_int(attribs)("to"),
            get_double(attribs)("length")
          )
        }

        case _ =>
      }
    })
  }
}
*/

class PlaintextReader(fn: String, with_geometry: Boolean) extends Reader(fn, with_geometry)
{
  // TODO splitting strings, converting to int/double is still expensive.
  override def read_file() = {
    // Brittle, but faster parser than xml.

    // TODO enum, dont do an if everytime, fix lots...
    var state = 0
    val t = Profiling.timer("loading map")  // TODO
    for (line <- Source.fromFile(fn).getLines) {
      if (state == 0) {
        // first line, the graph
        val Array(width, height, xoff, yoff, scale, num_roads, num_edges,
                  num_verts, ward_id) = line.split(",")
        // Set this as early as possible! length() of any lines we make needs
        // it.
        Graph.set_params(width.toDouble, height.toDouble, xoff.toDouble,
                         yoff.toDouble, scale.toDouble)
        special_ward_id = ward_id.toInt
        
        // set up all the temporary things to accumulate stuff
        roads = new Array[Road](num_roads.toInt)
        edges = new Array[Edge](num_edges.toInt)
        verts = new Array[Vertex](num_verts.toInt)
        vertLinks = new Array[Array[TmpLink]](num_verts.toInt)

        state = 1
      } else if (state == 1) {
        // second line, beginning of vertices
        Util.assert_eq(line, "---vertices---")
        state = 2
      } else if (state == 2) {
        // expecting a vertex
        if (line == "---roads---") {
          state = 3
        } else {
          // TODO this case is only for when bad edges aren't removed
          if (line.endsWith(":")) {
            val Array(metadata) = line.split(":")
            val Array(id, x, y) = metadata.split(",")
            verts(id.toInt) = new Vertex(new Coordinate(x.toDouble, y.toDouble), id.toInt)
            vertLinks(id.toInt) = Array()
          } else {
            val Array(metadata, turns) = line.split(":")
            val Array(id, x, y) = metadata.split(",")

            verts(id.toInt) = new Vertex(new Coordinate(x.toDouble, y.toDouble), id.toInt)
            vertLinks(id.toInt) = turns.split(";").map(link => {
              val Array(from, to, length, link_id, x1, y1, x2, y2) = link.split(",")
              new TmpLink(
                link_id.toInt, from.toInt, to.toInt, length.toDouble,
                new Line(x1.toDouble, y1.toDouble, x2.toDouble, y2.toDouble)
              )
            })
          }
        }
      } else if (state == 3) {
        // expecting a road
        if (line == "---edges---") {
          state = 4
        } else {
          val Array(metadata, points) = line.split(":")
          val Array(name, road_type, osm_id, v1, v2, ward, len, id) = metadata.split(",")

          val road = new Road(
            id.toInt, len.toDouble, name, road_type, osm_id.toInt,
            // it's vital that we break the interdependence by dumping vertices
            // first
            verts(v1.toInt), verts(v2.toInt)
          )
          roads(id.toInt) = road
          if (with_geometry) {
            road.set_points(points.split(";").map(pt => {
              val Array(x, y) = pt.split(",")
              new Coordinate(x.toDouble, y.toDouble)
            }))
          }
          wards_map.addBinding(ward.toInt, road)
        }
      } else if (state == 4) {
        // expecting an edge
        val Array(metadata, lines) = line.split(":")
        val Array(id, road, dir, lane_num, length) = metadata.split(",")

        val e = new Edge(id.toInt, roads(road.toInt),
                         if (dir == "+") Direction.POS else Direction.NEG)
        e.lane_num = lane_num.toInt
        edges(id.toInt) = e
        if (with_geometry) {
          e.set_lines(lines.split(";").map(e_line => {
            val Array(x1, y1, x2, y2) = e_line.split(",")
            new Line(x1.toDouble, y1.toDouble, x2.toDouble, y2.toDouble)
          }))
        } else {
          e.set_length(length.toDouble)
        }

        // Sucks, but we have some REALLY small edges.
        if (e.length <= cfg.min_lane_length) {
          // Cheat. Agents NEED the ability to exist somewhere on this edge.
          //Util.log("WARNING " + e + " is tiny")
          e.length = 0.1
        }

        // tell the road about the edge, too
        // TODO edges in the xml will be ordered by their id, which we created
        // in order of lane numbering too
        Util.assert_eq(e.other_lanes.length, e.lane_num)
        e.other_lanes += e
      }
    }
  }
}
