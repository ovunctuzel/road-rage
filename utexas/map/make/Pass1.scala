package utexas.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.map.Coordinate

import utexas.Util

class Pass1(fn: String) {
  // OSM's id to (longitude, latitude)
  val id_to_node = new HashMap[Int, Coordinate]

  // How many OSM roads reference a point?
  val id_to_uses = new HashMap[Int, Int]

  val graph = new PreGraph1()

  // if an osm node mentions these, it's not an edge we care about.
  val ignore_me = Set(
    "boundary",   // edge of a city
    "railway",    // tracks would be too easy
    "amenity",    // no stops on this trip
    "aeroway",    // we're not cl0ud
    "landuse",    // we don't want to use it
    "natural",    // naturally useless to us
    "waterway",   // we don't swim
    "building",   // we try not to drive through these
    "foot",       // we don't have feet
    "man_made",   // man-made things tend to suck
    "crossing",   // TODO dunno?
    "area",       // these, according to a forgotten old comment, are weird
    "path", "cycleway", "footway", "bridleway", "steps", "pedestrian",
    "bus_guideway"
    // TODO cemeteries in Houston, real roads in BTR, alleys in ATX...
    // they all cause problems when they have no name.
    // "service"
  )

  // according to http://wiki.openstreetmap.org/wiki/Key:oneway
  val forced_oneways = Set("motorway", "motorway_link", "trunk")

  def run(): PreGraph1 = {
    // fill out graph with roads and collect all info
    match_events( new XMLEventReader( Source.fromFile(fn) ) )

    Util.log("Normalizing graph coordinates")
    graph.normalize()

    return graph
  }

  def match_events(event_reader: XMLEventReader) = {
    // per way, we accumulate:
    // TODO i cant leave these uninitialized or set them to _
    var name: String = ""
    var road_type: String = ""
    var oneway: Boolean = false
    var skip_way: Boolean = false
    var id: Int = -1
    var refs: MutableList[Int] = new MutableList[Int]
    var lanes: Option[Int] = None

    var ev_count = 0

    // TODO probably a better way to unpack than casting to string
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text

    event_reader.foreach(ev => {
      ev_count += 1
      if (ev_count % 1000 == 0) {
        // it's expensive to spam System.out, believe it or not :P
        print("\r" + Util.indent + "Processed %,d XML events".format(ev_count))
      }

      ev match {
        case EvElemStart(_, "node", attribs, _) => {
          val viz = get_attrib(attribs, "visible")
          if (viz != None && viz != "true") {
            Util.log("WARNING: invisible node in osm")
          } else {
            // record the node
            val id = get_attrib(attribs, "id").toInt
            val x = get_attrib(attribs, "lon").toDouble
            val y = get_attrib(attribs, "lat").toDouble

            id_to_node(id) = new Coordinate(x, y)
            id_to_uses(id) = 0  // no edges reference it yet
          }
        }

        case EvElemStart(_, "way", attribs, _) => {
          // TODO refactor the skip invisible check
          val viz = get_attrib(attribs, "visible")
          if (viz != None && viz != "true") {
            Util.log("WARNING: invisible way in osm")
          } else {
            id = get_attrib(attribs, "id").toInt
            name = ""
            road_type = ""
            oneway = false
            skip_way = false
            refs = new MutableList[Int]
            lanes = None
          }
        }
        case EvElemEnd(_, "way") => {
          if (name == "" && road_type == "service") {
            // TODO wacky alleys, driveways, cemetary paths. when they
            // have a name, they're valid.
            skip_way = true
          }

          if (!skip_way) {
            // still handle missing names
            // TODO write a String.||= maybe?
            name = if (name == "") "NO-NAME (ID %d)".format(id) else name
            road_type = if (road_type == "") "null" else road_type

            if (forced_oneways(road_type)) {
              oneway = true
            }

            // what points does this edge touch?
            var points = new MutableList[Coordinate]
            for (ref <- refs) {
              points += id_to_node(ref)
              id_to_uses(ref) += 1

              // as soon as we hit the second reference, it's a vertex
              if (id_to_uses(ref) == 2) {
                graph.add_vertex(id_to_node(ref))
              }
            }

            graph.add_edge(name, road_type, oneway, id, points, lanes)

            // The tip and tail of this edge's points are always vertices
            graph.add_vertex(points.head)
            graph.add_vertex(points.last)
          }
        }

        case EvElemStart(_, "tag", attribs, _) => {
          if (!skip_way) {
            val key = get_attrib(attribs, "k")
            val value = get_attrib(attribs, "v")
            if (ignore_me(key) || (key == "highway" && ignore_me(value))) {
              skip_way = true
            } else {
              key match {
                case "name"    => { name = value }
                case "highway" => { road_type = value }
                case "oneway"  => { oneway = value == "yes" }
                case "lanes"   => { lanes = Some(value.toInt) }
                case _         => {}
              }
            }
          }
        }

        case EvElemStart(_, "nd", attribs, _) => {
          if (!skip_way) {
            val ref = get_attrib(attribs, "ref").toInt
            if (id_to_node.contains(ref)) {
              refs :+= ref
            } else {
              Util.log("WARNING: way references unknown node")
            }
          }
        }

        case _ => {}
      }
    })
    Util.log("")
  }
}
