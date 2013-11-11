// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable
import utexas.aorta.map.Coordinate

class BuildingScraper() {
  case class Bldg(road: String, points: List[Coordinate], residential: Boolean)
  private val bldgs = new mutable.ListBuffer[Bldg]()

  // TODO missing any?
  private val bldg_tags = Set("addr:housenumber", "shop")
  def is_bldg(tags: Map[String, String]) = tags.keys.exists(bldg_tags.contains(_))

  def scrape(osm: OsmReader) {
    osm.listen("building-scraper", _ match {
      case EV_OSM_Node(node) if is_bldg(node.tags) => {
        println("node: " + node.tags)
      }
      case EV_OSM_Way(way) if is_bldg(way.tags) => {
        println("way: " + way.tags)
      }
      case EV_OSM_Relation(rel) if is_bldg(rel.tags) => {
        println("rel: " + rel.tags)
      }
      case _ =>
    })
  }
}
