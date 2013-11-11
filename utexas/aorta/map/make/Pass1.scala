// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._
import scala.collection.mutable.{HashMap, MutableList, HashSet}

import utexas.aorta.map.Coordinate

import utexas.aorta.common.Util

class Pass1(fn: String) {
  val id_to_node = new HashMap[String, OSM_Node]()
  val id_to_way = new HashMap[String, OSM_Way]()

  // How many OSM roads reference a point?
  val id_to_uses = new HashMap[String, Int]()
  val graph = new PreGraph1()

  def run(): PreGraph1 = {
    // Parse the XML
    match_events(new XMLEventReader(Source.fromFile(fn, "UTF-8")))
    // Vertices are defined as nodes used by >1 way
    for ((id, uses) <- id_to_uses if uses >= 2) {
      graph.add_vertex(id_to_node(id).coordinate)
    }
    // The tip and tail of every edge are always vertices
    for (edge <- graph.edges) {
      graph.add_vertex(edge.points.head)
      graph.add_vertex(edge.points.last)
    }
    graph.normalize()
    Util.log(s"OSM nodes: ${id_to_node.size}, ways: ${graph.edges.size}")
    return graph
  }

  // XML parsing stuff below.
  // TODO split me out into a separate class, and broadcast events. id_to_node moves there,
  // id_to_uses stays here. then refactor once its done.
  type XMLIter = BufferedIterator[XMLEvent]

  private def get(attribs: MetaData, key: String): String = attribs.get(key).head.text
  private def get(attribs: MetaData, key: String, default: String): String =
    attribs.get(key) match {
      case Some(ls) => ls.head.text
      case None => default
    }

  private def read_tags(xml: XMLIter): Map[String, String] = {
    val tags = new HashMap[String, String]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "tag", attribs, _) => {
          tags(get(attribs, "k")) = get(attribs, "v")
          xml.next()
        }
        case EvElemEnd(_, "tag") => xml.next()
        case EvText(_) => xml.next()
        case _ => return tags.toMap
      }
    }
    throw new IllegalArgumentException("XML ended with tags")
  }

  case class OSM_Node(id: String, lon: Double, lat: Double, tags: Map[String, String]) {
    def coordinate = new Coordinate(lon, lat)
  }

  private def read_node(attribs: MetaData, xml: XMLIter): Option[OSM_Node] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "node") =>
    }
    val node = OSM_Node(
      get(attribs, "id"), get(attribs, "lon").toDouble, get(attribs, "lat").toDouble, tags
    )
    id_to_node(node.id) = node
    return Some(node)
  }

  case class OSM_Way(id: String, refs: List[OSM_Node], tags: Map[String, String]) {
    def name = tags.getOrElse("name", s"NO-NAME (ID $id)")
    def road_type = tags.getOrElse("highway", "null")
    def oneway = tags.getOrElse("oneway", "") == "yes" || Pass1.forced_oneways(road_type)
    // TODO fancy "x; y" format... handle eventually
    def lanes = try {
      Some(tags.getOrElse("lanes", "").toInt)
    } catch {
      case _: NumberFormatException => None
    }
    def skip_as_edge: Boolean = {
      if (tags.getOrElse("name", "").isEmpty && road_type == "service") {
        // TODO wacky alleys, driveways, cemetary paths. when they have a name, they're valid.
        return true
      }
      // TODO break down the ignore_me set for these cases, be really precise using the wiki.
      if (tags.keys.filter(key => tags(key) != "no").toSet.intersect(Pass1.ignore_me).nonEmpty) {
        return true
      }
      if (Pass1.ignore_me(road_type)) {
        return true
      }
      return false
    }
  }

  private def read_way(attribs: MetaData, xml: XMLIter): Option[OSM_Way] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    // TODO Do tags go before or after nodes? Interspersed?
    val refs = read_refs(xml)
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "way") =>
    }

    val way = OSM_Way(get(attribs, "id"), refs, tags)
    id_to_way(way.id) = way
    return Some(way)
  }

  private def read_refs(xml: XMLIter): List[OSM_Node] = {
    val ls = new MutableList[OSM_Node]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "nd", attribs, _) => {
          ls += id_to_node(get(attribs, "ref"))
          // TODO handle when that node's missing
          xml.next()
        }
        case EvElemEnd(_, "nd") => xml.next()
        case EvText(_) => xml.next()
        case _ => return ls.toList
      }
    }
    throw new IllegalArgumentException("XML ended with node refs")
  }

  case class OSM_Relation(
    id: String, member_ways: List[OSM_Way], member_nodes: List[OSM_Node], tags: Map[String, String]
  ) {
    def skip_members: Boolean = {
      // TODO break down the ignore_me set for these cases, be really precise using the wiki.
      if (tags.keys.filter(key => tags(key) != "no").toSet.intersect(Pass1.ignore_me).nonEmpty) {
        return true
      }
      // TODO other tags? highway?
      if (Pass1.ignore_me(tags.getOrElse("type", ""))) {
        return true
      }
      return false
    }
  }

  private def read_relation(attribs: MetaData, xml: XMLIter): Option[OSM_Relation] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    // TODO Do tags go before or after nodes? Interspersed?
    val (ways, nodes) = try {
      read_members(xml)
    } catch {
      case _: java.util.NoSuchElementException => return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "relation") =>
    }

    return Some(OSM_Relation(get(attribs, "id"), ways, nodes, tags))
  }

  private def read_members(xml: XMLIter): (List[OSM_Way], List[OSM_Node]) = {
    val ways = new MutableList[OSM_Way]()
    val nodes = new MutableList[OSM_Node]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "member", attribs, _) => {
          (get(attribs, "type"), get(attribs, "ref")) match {
            case ("way", ref) => ways += id_to_way(ref)
            // TODO handle when that node's missing
            case ("node", ref) => nodes += id_to_node(ref)
            case ("relation", _) => // TODO super-relations! whoa!
          }
          xml.next()
        }
        case EvElemEnd(_, "member") => xml.next()
        case EvText(_) => xml.next()
        case _ => return (ways.toList, nodes.toList)
      }
    }
    throw new IllegalArgumentException("XML ended with member relations")
  }

  def match_events(event_reader: XMLEventReader) {
    val xml = event_reader.buffered
    var toplevel_count = 0
    // TODO emit events instead.
    while (xml.hasNext) {
      xml.next match {
        case EvElemStart(_, "node", attribs, _) => read_node(attribs, xml).foreach(node => {
          id_to_uses(node.id) = 0  // no edges reference it yet
        })
        case EvElemStart(_, "way", attribs, _) => read_way(attribs, xml).foreach(way => {
          if (!way.skip_as_edge) {
            way.refs.foreach(node => id_to_uses(node.id) += 1)
            graph.edges += PreEdge1(
              way.name, way.road_type, way.oneway, way.id, way.refs.map(_.coordinate), way.lanes
            )
          }
        })
        case EvElemStart(_, "relation", attribs, _) => read_relation(attribs, xml).foreach(rel => {
          if (rel.skip_members) {
            // We won't worry about the vertices associated with these obselete
            // edges; they'll only survive to the end if they're referenced by
            // something else.
            graph.remove_edges(rel.member_ways.map(_.id).toSet)
          }
        })
        case _ => // TODO make sure we're missing nothing
      }
      toplevel_count += 1
      if (toplevel_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print("\r" + Util.indent + "Processed %,d XML top-level objects".format(toplevel_count))
      }
    }
    println("")
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
  // TODO they're vals, but i don't want to set them yet!
  var width: Double = 0
  var height: Double = 0
  var offX: Double = 0
  var offY: Double = 0
  var scale: Double = 5000    // TODO in the future, dont scale.

  // Further evidence in the OSM source suggests these edges are bogus
  def remove_edges(ids: Set[String]) {
    edges = edges.filter(e => !ids.contains(e.orig_id))
  }
  def add_vertex(where: Coordinate) {
    vert_lookup += where
  }
  def is_vert(pt: Coordinate) = vert_lookup.contains(pt)

  // Longitude, Latitude origin is bottom-left; we draw from top-left
  // Hence height - y
  // This is the ONLY place we handle y inversion. Don't make things
  // confusing after this!
  private def fix(pt: Coordinate) = 
    new Coordinate((pt.x + offX) * scale, height - ((pt.y + offY) * scale))

  def normalize() = {
    var minX = Double.MaxValue
    var minY = Double.MaxValue
    var maxX = Double.MinValue
    var maxY = Double.MinValue
    for (e <- edges; pt <- e.points) {
      minX = math.min(minX, pt.x)
      minY = math.min(minY, pt.y)
      maxX = math.max(maxX, pt.x)
      maxY = math.max(maxY, pt.y)
    }
    Util.log("Bounds: %f .. %f, %f .. %f".format(minX, maxX, minY, maxY))

    // so to make (minX, minY) the new origin...
    offX = 0 - minX
    offY = 0 - minY

    // Scale everything up by some fixed ratio...
    width = (maxX + offX) * scale
    height = (maxY + offY) * scale

    val new_pts = vert_lookup.map(fix)
    vert_lookup.clear()
    vert_lookup ++= new_pts
    for (e <- edges) {
      e.points = e.points.map(fix)
    }
  }
}

case class PreEdge1(
  name: String, road_type: String, oneway: Boolean, orig_id: String, var points: List[Coordinate],
  lanes: Option[Int]
)
