// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.aorta.map.Coordinate

import utexas.aorta.Util

class Pass1(fn: String) {
  // OSM's id to (longitude, latitude)
  val id_to_node = new HashMap[String, Coordinate]

  // How many OSM roads reference a point?
  val id_to_uses = new HashMap[String, Int]

  val graph = new PreGraph1()

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

  def run(): PreGraph1 = {
    // fill out graph with roads and collect all info
    match_events( new XMLEventReader(Source.fromFile(fn, "UTF-8")))

    Util.log("Normalizing graph coordinates")
    graph.normalize()

    return graph
  }

  def match_events(event_reader: XMLEventReader) = {
    // per way, we accumulate:
    var name: String = ""
    var road_type: String = ""
    var oneway: Boolean = false
    var skip_way: Boolean = false
    var id: String = ""
    var refs: MutableList[String] = new MutableList[String]
    var lanes: Option[Int] = None

    // per relation:
    var relation_members: Set[String] = Set()
    var skip_relation: Boolean = false

    var ev_count = 0

    // TODO probably a better way to unpack than casting to string
    def get_attrib(attribs: MetaData, key: String): String = attribs.get(key).head.text
    def get_attrib_default(attribs: MetaData, key: String, default: String): String =
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

      ev match {
        case EvElemStart(_, "node", attribs, _) =>
          (get_attrib_default(attribs, "visible", "true"), get_attrib_default(attribs, "action", "modify")) match {
            case ("true", "modify") => {
              // record the node
              val id = get_attrib(attribs, "id")
              val x = get_attrib(attribs, "lon").toDouble
              val y = get_attrib(attribs, "lat").toDouble

              id_to_node(id) = new Coordinate(x, y)
              id_to_uses(id) = 0  // no edges reference it yet
            }
            case _ =>
          }

        case EvElemStart(_, "way", attribs, _) =>
          (get_attrib_default(attribs, "visible", "true"), get_attrib_default(attribs, "action", "modify")) match {
            case ("true", "modify") => {
              id = get_attrib(attribs, "id")
              name = ""
              road_type = ""
              oneway = false
              skip_way = false
              refs = new MutableList[String]
              lanes = None
            }
            case _ =>
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
            name = if (name == "") "NO-NAME (ID %s)".format(id) else name
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

            // The tip and tail of this way's points are always vertices
            graph.add_vertex(points.head)
            graph.add_vertex(points.last)
          }

          id = ""
        }

        case EvElemStart(_, "tag", attribs, _) if id != -1 => {
          if (!skip_way) {
            val key = get_attrib(attribs, "k")
            val value = get_attrib(attribs, "v")
            if ((ignore_me(key) && value != "no") || (key == "highway" && ignore_me(value))) {
              skip_way = true
            } else {
              key match {
                case "name"    => { name = value }
                case "highway" => { road_type = value }
                case "oneway"  => { oneway = value == "yes" }
                case "lanes"   => {
                  // TODO fancy "x; y" format... handle eventually
                  try {
                    lanes = Some(value.toInt)
                  } catch {
                    case _: NumberFormatException =>
                  }
                }
                case _         => {}
              }
            }
          }
        }

        case EvElemStart(_, "nd", attribs, _) => {
          if (!skip_way) {
            val ref = get_attrib(attribs, "ref")
            if (id_to_node.contains(ref)) {
              refs :+= ref
            } else {
              //Util.log(s"Way $id references unknown node $ref")
              // Nothing else we can do...
              skip_way = true
            }
          }
        }

        case EvElemStart(_, "relation", attribs, _) => {
          relation_members = Set()
          skip_relation = false
        }

        // The version for relations, not ways
        case EvElemStart(_, "tag", attribs, _) if id == -1 => {
          val key = get_attrib(attribs, "k")
          val value = get_attrib(attribs, "v")
          if (ignore_me(key) || ((key == "highway" || key == "type") && ignore_me(value))) {
            skip_relation = true
          }
        }

        case EvElemStart(_, "member", attribs, _) => {
          if (get_attrib(attribs, "type") == "way") {
            relation_members += get_attrib(attribs, "ref")
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
