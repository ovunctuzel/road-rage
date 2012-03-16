// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.map.make

import java.io.FileWriter

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.map.{Coordinate, Vertex, Road, Edge, Direction, Ward, Turn}

// TODO we should really subclass the real Graph, but not sure yet.

class PreGraph3(old_graph: PreGraph2) {
  var vertices = new MutableList[Vertex]

  // create vertices lazily!
  private val vert_lookup = new HashMap[Coordinate, Vertex]

  // TODO i hope this wont be necessary eventually
  var road_id_cnt = 0

  var edges = new MutableList[Edge]           // a directed lane
  var roads = old_graph.edges.map(add_road)   // collections of edges
  var wards = List[Ward]()
  var special_ward: Ward = null

  // magic borrowed from Graph to support Tarjan's. Each of these expensive
  // things should only be called once.
  def turns() = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)
  def traversables() = edges ++ turns

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
    val lanes = how_many_lanes(old_edge.dat)

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

  // osm types are inconsistent and wacky, but do our best
  private def how_many_lanes(dat: PreEdge1): Int = (dat.lanes, dat.road_type) match {
    // OSM actually told us!
    // TODO This number should be total, but I'm seeing examples of '3 lanes'...
    // how do we split those in each direction?
    case (Some(n), _)            => n
    case (None, "residential")   => 1
    // not all types of links between highways are one-laners, but most seem to
    // be.
    case (None, "motorway_link")  => 1
    case (None, "trunk_link")     => 1
    case (None, "primary_link")   => 1
    case (None, "secondary_link") => 1
    case (None, "tertiary_link")  => 1
    case (None, "service")        => 1   // I don't know what these are supposed to be
    case _                        => 2
  }

  // TODO pregraph1 should have a 'data' structure or something
  // it's important to dump vertices first, since roads need them. and edges
  // need roads, so the order works out well.
  // scala has nice xml construction, but it's a memory hog.
  def to_xml(out: FileWriter, dat: PreGraph1) = {
    out.write(
      "<graph width=\"" + dat.width + "\" height=\"" + dat.height
      + "\" xoff=\"" + dat.offX + "\" yoff=\"" + dat.offY
      + "\" scale=\"" + dat.scale + "\" roads=\"" + roads.length
      + "\" edges=\"" + edges.length + "\" verts=\"" + vertices.length 
      + "\" special_ward=\"" + special_ward.id + "\">\n"
    )
    vertices.foreach(v => v.to_xml(out))
    roads.foreach(r => r.to_xml(out))
    edges.foreach(e => e.to_xml(out))
    out.write("</graph>\n")
  }
}
