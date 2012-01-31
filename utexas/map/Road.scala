package utexas.map

import java.io.FileWriter

import scala.collection.mutable.MutableList

// TODO enum for type. also, it's var because of tarjan's...
// TODO var id due to tarjan
class Road(var id: Int, val points: List[Coordinate], val name: String,
           var road_type: String, val osm_id: Int, val v1: Vertex,
           val v2: Vertex)
{
  // an invariant: v1 = vertex at points.first, v2 = vertex at points.last

  // + lanes go from v1->v2; - lanes go from v2->v1
  // pass 3 doesn't set this, only Reader does. kinda sucks how we do it now.
  val pos_lanes = new MutableList[Edge]
  val neg_lanes = new MutableList[Edge]

  // This is fixed, but we don't know it immediately...
  var ward: Ward = null

  def all_lanes() = pos_lanes ++ neg_lanes
  def other_vert(v: Vertex) = if (v == v1) v2 else v1

  def is_oneway = pos_lanes.length == 0 || neg_lanes.length == 0
  // TODO assert is_oneway, maybe. or maybe even case class...
  def oneway_lanes = if (pos_lanes.length == 0) neg_lanes else pos_lanes

  def num_lanes = pos_lanes.length + neg_lanes.length

  def to_xml(out: FileWriter) = {
    out.write(
      "  <road name=\"" + scala.xml.Utility.escape(name) + "\" type=\"" + road_type
      + "\" osmid=\"" + osm_id + "\" v1=\"" + v1.id + "\" v2=\"" + v2.id
      + "\" ward=\"" + ward.id + "\" id=\"" + id + "\">\n"
    )
    points.foreach(pt => pt.to_xml(out))
    out.write("</road>\n")
  }

  override def toString = name + " [R" + id + "]"
  
  // TODO don't ask for vertex that isn't either v1 or v2.
  def incoming_lanes(v: Vertex) = if (v == v1) neg_lanes else pos_lanes
  def outgoing_lanes(v: Vertex) = if (v == v1) pos_lanes else neg_lanes

  def pairs_of_points = points zip points.tail
}
