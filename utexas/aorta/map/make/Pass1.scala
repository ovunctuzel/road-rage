// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable.{HashMap, MutableList, HashSet}

import utexas.aorta.map.Coordinate

import utexas.aorta.common.Util

class Pass1(fn: String) {
  // How many OSM roads reference a point?
  val node_uses = new HashMap[OsmNode, Int]()
  val osm = new OsmReader(fn)
  val graph = new PreGraph1()

  osm.listen("pass1", _ match {
    case EV_OSM(node: OsmNode) => node_uses(node) = 0
    case EV_OSM(way: OsmWay) if !skip(way) => {
      way.refs.foreach(node => node_uses(node) += 1)
      graph.edges += PreEdge1(
        way.name, way.road_type, way.oneway, way.id, way.refs.map(_.coordinate), way.lanes
      )
    }
    case EV_OSM(relation: OsmRelation) if skip_members(relation) => {
      // We won't worry about the vertices associated with these obselete
      // edges; they'll only survive to the end if they're referenced by
      // something else.
      graph.remove_edges(relation.member_ways.map(_.id).toSet)
    }
    case _ =>
  })

  def run(): PreGraph1 = {
    osm.parse()
    // Vertices are defined as nodes used by >1 way
    for ((node, uses) <- node_uses if uses >= 2) {
      graph.add_vertex(node.coordinate)
    }
    // The tip and tail of every edge are always vertices
    for (edge <- graph.edges) {
      graph.add_vertex(edge.points.head)
      graph.add_vertex(edge.points.last)
    }
    Util.log(s"OSM nodes: ${osm.id_to_node.size}, ways: ${graph.edges.size}")
    return graph
  }

  def skip(way: OsmWay): Boolean = {
    if (way.tags.getOrElse("name", "").isEmpty && way.road_type == "service") {
      // TODO wacky alleys, driveways, cemetary paths. when they have a name, they're valid.
      return true
    }
    // TODO break down the ignore_me set for these cases, be really precise using the wiki.
    if (way.tags.keys.filter(way.tags(_) != "no").toSet.intersect(Pass1.ignore_me).nonEmpty) {
      return true
    }
    if (Pass1.ignore_me(way.road_type)) {
      return true
    }
    return false
  }

  def skip_members(rel: OsmRelation): Boolean = {
    // TODO break down the ignore_me set for these cases, be really precise using the wiki.
    if (rel.tags.keys.filter(rel.tags(_) != "no").toSet.intersect(Pass1.ignore_me).nonEmpty) {
      return true
    }
    // TODO other tags? highway?
    if (Pass1.ignore_me(rel.tags.getOrElse("type", ""))) {
      return true
    }
    return false
  }
}

object Pass1 {
  // if an osm node mentions these, it's not an edge we care about.
  val ignore_me = Set(
    "boundary",       // edge of a city
    "railway",        // tracks would be too easy
    "amenity",        // no stops on this trip
    "aeroway",        // we're not cl0ud
    "landuse",        // we don't want to use it
    "natural",        // naturally useless to us
    "waterway",       // we don't swim
    "building",       // we try not to drive through these
    "foot",           // we don't have feet
    "man_made",       // man-made things tend to suck
    "crossing",       // TODO dunno?
    "area",           // these, according to a forgotten old comment, are weird
    "leisure",        // NO TIME FOR THAT NOW
    "multipolygon",   // WHY ARE THERE SO MANY
    "power",          // AHHH DON'T SHOCK ME
    // cycleway is marked in addition to being highway=tertiary... geez.
    "path", "footway", "bridleway", "steps", "pedestrian", "bus_guideway"
    // TODO cemeteries in Houston, real roads in BTR, alleys in ATX...
    // they all cause problems when they have no name:
    // "service"
  )

  // according to http://wiki.openstreetmap.org/wiki/Key:oneway
  val forced_oneways = Set("motorway", "motorway_link", "trunk")
}

class PreGraph1() {
  var edges = new MutableList[PreEdge1]
  private val vert_lookup = new HashSet[Coordinate]()

  // Further evidence in the OSM source suggests these edges are bogus
  def remove_edges(ids: Set[String]) {
    edges = edges.filter(e => !ids.contains(e.orig_id))
  }
  def add_vertex(where: Coordinate) {
    vert_lookup += where
  }
  def is_vert(pt: Coordinate) = vert_lookup.contains(pt)
}

case class PreEdge1(
  name: String, road_type: String, oneway: Boolean, orig_id: String, points: List[Coordinate],
  lanes: Option[Int]
)
