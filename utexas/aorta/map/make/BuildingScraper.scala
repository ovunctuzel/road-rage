// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable
import utexas.aorta.map.Coordinate

class BuildingScraper() {
  case class Bldg(road: Option[String], points: List[Coordinate], residential: Boolean)
  private val bldgs = new mutable.ListBuffer[Bldg]()

  // TODO missing any?
  private val bldg_tags = Set("addr:housenumber", "shop")
  private val nonresidence_tags = Set("amenity", "shop")
  private def is_bldg(tags: Map[String, String]) = tags.keys.exists(bldg_tags.contains(_))
  private def is_residential(tags: Map[String, String]) =
    !tags.keys.exists(nonresidence_tags.contains(_))

  def scrape(osm: OsmReader) {
    osm.listen("building-scraper", _ match {
      case EV_OSM(elem) if is_bldg(elem.tags) =>
        bldgs += Bldg(elem.tags.get("addr:street"), elem.points, is_residential(elem.tags))
      case _ =>
    })
  }
}
