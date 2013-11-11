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
  // OSM's id to (longitude, latitude)
  val id_to_node = new HashMap[String, Coordinate]()
  // How many OSM roads reference a point?
  val id_to_uses = new HashMap[String, Int]()
  val graph = new PreGraph1()

  def run(): PreGraph1 = {
    // Parse the XML
    match_events(new XMLEventReader(Source.fromFile(fn, "UTF-8")))
    // Vertices are defined as nodes used by >1 way
    for ((id, uses) <- id_to_uses if uses >= 2) {
      graph.add_vertex(id_to_node(id))
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
  type XMLIter = BufferedIterator[XMLEvent]

  private def read_tags(xml: XMLIter): Map[String, String] {
    val tags = new HashMap[String, String]()
    while (xml.hasNext) {
      xml.head match {
        case EvElemStart(_, "tag", attribs, _) => {
          tags(get(attribs, "k")) = get(attribs, "v")
          xml.next()
        }
        case _ => return tags.toMap
      }
    }
  }

  case class OSM_Node(id: String, lon: Double, lat: Double, tags: Map[String, String]) {
    def coord = new Coordinate(lon, lat)
  }

  private def read_node(attribs: MetaData, xml: XMLIter): Option[OSM_Node] = {
    if (get(attribs, "visible", "true") != "true" || get(attribs, "action", "modify") != "modify") {
      return None
    }
    val tags = read_tags(xml)
    xml.next match {
      case EvElemEnd(_, "node") =>
    }
    return Some(OSM_Node(
      get(attribs, "id"), get(attribs, "lon").toDouble, get(attribs, "lat").toDouble, tags
    )
  }

  case class OSM_Way(id: String, refs: List[OSM_Node], tags: Map[String, String]) {
    def name = tags.getOrElse("name", s"NO-NAME (ID $id)")
    def road_type = tags.getOrElse("highway", "null")
    def oneway = tags.getOrElse("oneway", "") == "yes" || Pass1.forced_oneways(road_type)
    // TODO fancy "x; y" format... handle eventually
    def lanes = try {
      Some(tags("lanes").toInt)
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
      if (ignore_me(road_type)) {
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

    return Some(OSM_Way(get(attribs, "id"), refs, tags))
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
        case _ => return ls.toList
      }
    }
  }

  def match_events(event_reader: XMLEventReader) {
    val xml = event_reader.iterator.buffered
    while (iter.hasNext) {
      iter.next match {
        case EvElemStart(_, "node", attribs, _) => read_node(attribs, xml).foreach(node => {
          id_to_node(node.id) = node
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
      }
    }
  }






    // per relation:
    var relation_members: Set[String] = Set()
    var skip_relation: Boolean = false

    var ev_count = 0

    def get(attribs: MetaData, key: String): String = attribs.get(key).head.text
    def get_default(attribs: MetaData, key: String, default: String): String =
      attribs.get(key) match {
        case Some(ls) => ls.head.text
        case None => default
      }

    event_reader.foreach(ev => {
      ev_count += 1
      if (ev_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print("\r" + Util.indent + "Processed %,d XML events".format(ev_count))
      }


        case EvElemStart(_, "relation", attribs, _) => {
          relation_members = Set()
          skip_relation = false
        }

        // The version for relations, not ways
        case EvElemStart(_, "tag", attribs, _) if id == -1 => {
          val key = get(attribs, "k")
          val value = get(attribs, "v")
          if (ignore_me(key) || ((key == "highway" || key == "type") && ignore_me(value))) {
            skip_relation = true
          }
        }

        case EvElemStart(_, "member", attribs, _) => {
          if (get(attribs, "type") == "way") {
            relation_members += get(attribs, "ref")
          }
        }

        case EvElemEnd(_, "relation") => {
          if (skip_relation) {
            // We won't worry about the vertices associated with these obselete
            // edges; they'll only survive to the end if they're referenced by
            // something else.
            graph.remove_edges(relation_members)
          }
        }

        case _ =>
      }
    })
    Util.log("")
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
  name: String, road_type: String, oneway: Boolean, orig_id: String,
  var points: MutableList[Coordinate], lanes: Option[Int]
)
