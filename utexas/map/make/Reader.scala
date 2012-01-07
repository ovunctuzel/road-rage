package utexas.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

import scala.collection.mutable.MutableList
import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import scala.collection.mutable.{Set => MutableSet}

import utexas.map.{Coordinate, Road, Vertex, Edge, Direction, Line, TurnType,
                   Turn, Graph, Ward}
import utexas.sim.Simulation

import utexas.Util

class Reader(fn: String) {
  // per road
  class TmpLink(val from: Int, val to: Int, val link_type: TurnType.TurnType)

  // we know all of this as soon as we read the graph tag...
  var vertLinks: Array[List[TmpLink]] = null

  def load_map(): Graph             = return load(false).right.get
  def load_simulation(): Simulation = return load(true).left.get
  
  // TODO this is one nasty long routine...
  // and it changes behavior at a few places based on its parameter.
  def load(with_agents: Boolean): Either[Simulation, Graph] = {
    Util.log("Loading map " + fn)
    val event_reader = new XMLEventReader( Source.fromFile(fn) )
    Util.log_push

    var ev_count = 0

    // TODO probably a better way to unpack than casting to string
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text
    def get_ints(attribs: MetaData) = get_attrib(attribs, (_: String)).toInt
    def get_doubles(attribs: MetaData) = get_attrib(attribs, (_: String)).toDouble

    // per graph
    var roads: Array[Road] = null
    var edges: Array[Edge] = null
    var verts: Array[Vertex] = null
    var width, height: Double = 0.0
    val wards_map = new HashMap[Int, MutableSet[Road]] with MultiMap[Int, Road]
    var special_ward_id: Int = -1

    // per road
    var rd_name, rd_type: String = ""
    var rd_osm, rd_id, rd_ward, rd_v1, rd_v2: Int = -1
    var rd_pts: MutableList[Coordinate] = null

    // per edge
    var e_id, e_rd, e_lane: Int = -1
    var e_dir: Direction.Direction = Direction.POS
    var e_lines = new MutableList[Line]

    // per vertex
    var v_id: Int = -1
    var v_x, v_y: Double = 0.0
    var v_links = new MutableList[TmpLink]

    event_reader.foreach(ev => {
      ev_count += 1
      if (ev_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print("\r" + Util.indent + "Processed %,d XML events".format(ev_count))
      }

      ev match {
        case EvElemStart(_, "graph", attribs, _) => {
          width  = get_doubles(attribs)("width")
          height = get_doubles(attribs)("height")
          special_ward_id = get_ints(attribs)("special_ward")

          val num_roads = get_ints(attribs)("roads")
          val num_edges = get_ints(attribs)("edges")
          val num_verts = get_ints(attribs)("verts")

          // set up all the temporary things to accumulate stuff
          roads = new Array[Road](num_roads)
          edges = new Array[Edge](num_edges)
          verts = new Array[Vertex](num_verts)

          vertLinks = new Array[List[TmpLink]](num_verts)
        }

        case EvElemStart(_, "road", attribs, _) => {
          rd_name = get_attrib(attribs, "name")
          rd_type = get_attrib(attribs, "type")
          rd_osm = get_ints(attribs)("osmid")
          rd_id = get_ints(attribs)("id")
          rd_ward = get_ints(attribs)("ward")
          rd_v1 = get_ints(attribs)("v1")
          rd_v2 = get_ints(attribs)("v2")
          rd_pts = new MutableList[Coordinate]
        }
        case EvElemEnd(_, "road") => {
          roads(rd_id) = new Road(
            rd_id, rd_pts.toList, rd_name, rd_type, rd_osm,
            // it's vital that we break the interdependence by dumping vertices
            // first
            verts(rd_v1), verts(rd_v2)
          )
          wards_map.addBinding(rd_ward, roads(rd_id))

          // reset stuff
          rd_name = ""
          rd_type = ""
          rd_osm = -1
          rd_id = -1
          rd_ward = -1
          rd_v1 = -1
          rd_v2 = -1
          rd_pts.clear
        }

        case EvElemStart(_, "pt", attribs, _) => {
          rd_pts += new Coordinate(
            get_doubles(attribs)("x"), get_doubles(attribs)("y")
          )
        }

        case EvElemStart(_, "edge", attribs, _) => {
          e_id = get_ints(attribs)("id")
          e_rd = get_ints(attribs)("road")
          val dir = get_attrib(attribs, "dir")(0)
          e_lane = get_ints(attribs)("laneNum")

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
          e.lines = e_lines.toList

          // tell the road about the edge, too
          // TODO edges in the xml will be ordered by their id, which we created
          // in order of lane numbering too
          assert(e.other_lanes.length == e.lane_num)
          e.other_lanes += e

          // reset
          e_id = -1
          e_rd = -1
          e_lane = -1
          e_dir = Direction.POS
          e_lines.clear
        }

        case EvElemStart(_, "line", attribs, _) => {
          val List(x1, y1, x2, y2) = List("x1", "y1", "x2", "y2") map get_doubles(attribs)
          e_lines += new Line(x1, y1, x2, y2)
        }

        case EvElemStart(_, "vertex", attribs, _) => {
          v_id = get_ints(attribs)("id")
          v_x = get_doubles(attribs)("x")
          v_y = get_doubles(attribs)("y")

          v_links = new MutableList[TmpLink]
        }
        case EvElemEnd(_, "vertex") => {
          verts(v_id) = new Vertex(new Coordinate(v_x, v_y), v_id)
          vertLinks(v_id) = v_links.toList

          // reset
          v_id = -1
          v_x = 0.0
          v_y = 0.0
          v_links.clear
        }

        case EvElemStart(_, "link", attribs, _) => {
          v_links += new TmpLink(
            get_ints(attribs)("from"), get_ints(attribs)("to"),
            TurnType.withName(get_attrib(attribs, "type"))
          )
        }

        case _ => {}
      }
    })
    Util.log("")
    Util.log_pop

    Util.log("Adding references at intersections")
    // turns just want edges, doesn't matter what trait they're endowed with
    for (v <- verts) {
      for (link <- vertLinks(v.id)) {
        v.turns += new Turn(edges(link.from), link.link_type, edges(link.to))
      }
    }

    Util.log("Recovering wards as well")
    // Don't forget to separate out the special ward first
    // Missing the special ward  can happen on some oddly-constructed maps
    val special_ward = if (wards_map.contains(special_ward_id))
                         new Ward(special_ward_id, wards_map(special_ward_id).toSet)
                       else
                         new Ward(special_ward_id, Set())
    wards_map -= special_ward_id
    val wards: List[Ward] = wards_map.map(pair => new Ward(pair._1, pair._2.toSet)).toList

    if (with_agents) {
      return Left(new Simulation(
        roads.toList, edges.toList, verts.toList, wards, special_ward, width, height
      ))
    } else {
      return Right(new Graph(
        roads.toList, edges.toList, verts.toList, wards, special_ward, width, height
      ))
    }
    // TODO anything to more explicitly free up?
  }
}
