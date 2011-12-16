package utexas.map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.map.{Coordinate, Vertex, Road, Edge, Direction}

// TODO we should really subclass the real Graph, but not sure yet.

class PreGraph3(old_graph: PreGraph2) {
  var vertices = new MutableList[Vertex]

  // create vertices lazily!
  private val vert_lookup = new HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new MutableList[Edge]          // a directed lane
  var roads = old_graph.edges map add_road   // collections of edges

  def add_road(old_edge: PreEdge2): Road = {
    // do addEdges too, once we decide how many lanes to do
    // TODO shit, decide roads.size while we're assigning to it?
    val r = new Road(
      road_id_cnt, old_edge.points.toList,
      old_edge.dat.name, old_edge.dat.road_type, old_edge.dat.orig_id,
      get_vert(old_edge.points.head), get_vert(old_edge.points.last)
    )
    road_id_cnt += 1

    // now make the directed edges too
    val lanes = how_many_lanes(old_edge.dat.road_type)

    // v1 -> v2 lanes
    for (l <- 1 to lanes) {
      add_edge(r, Direction.POS)
    }

    // v2 -> v1 lanes unless we're oneway, in which case we keep the original
    // osm order
    if (!old_edge.dat.oneway) {
      for (l <- 1 to lanes) {
        add_edge(r, Direction.NEG)
      }
    }

    return r
  }

  private def get_vert(at: Coordinate): Vertex = {
    if (vert_lookup.contains(at)) {
      return vert_lookup(at)
    } else {
      val v = new Vertex(at, vertices.length)
      vertices += v
      vert_lookup += ((at, v))
      return v
    }
  }

  private def add_edge(r: Road, dir: Direction.Direction) = {
    val e = new Edge(edges.length, r, dir)
    edges += e
    // first in a list of lanes is defined as the rightmost
    var lanes = e.other_lanes
    e.lane_num = lanes.length
    lanes += e
  }

  // TODO osm types are inconsistent and wacky, but do our best
  private def how_many_lanes(road_type: String): Int = road_type match {
    case "residential"   => 1
    case "motorway_link" => 1   // these merge ramps should be one-way too
    case "service"       => 1   // I don't know what these are supposed to be
    case _               => 2
  }

  // TODO pregraph1 should have a 'data' structure or something
  // TODO dont we want to stream this out? or is this xml construction fine?
  // it's important to dump vertices first
  def to_xml(dat: PreGraph1) =
    <graph width={dat.width.toString} height={dat.height.toString}
           xoff={dat.offX.toString} yoff={dat.offY.toString} scale={dat.scale.toString}
           roads={roads.length.toString} edges={edges.length.toString}
           verts={vertices.length.toString}>
      {vertices.map(v => v.to_xml)}
      {roads.map(r => r.to_xml)}
      {edges.map(e => e.to_xml)}
    </graph>
}
