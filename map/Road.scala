package map

import scala.collection.mutable.MutableList

// TODO enum for type
class Road(val id: Int, val points: List[Coordinate], val name: String,
           val road_type: String, val osm_id: Int, val v1: Vertex,
           val v2: Vertex)
{
  // an invariant: v1 = vertex at points.first, v2 = vertex at points.last

  // + lanes go from v1->v2; - lanes go from v2->v1
  // pass 3 doesn't set this, only Reader does. kinda sucks how we do it now.
  var pos_lanes = new MutableList[Edge]
  var neg_lanes = new MutableList[Edge]

  def is_oneway = pos_lanes.length == 0 || neg_lanes.length == 0
  // TODO assert is_oneway, maybe. or maybe even case class...
  def oneway_lanes = if (pos_lanes.length == 0) neg_lanes else pos_lanes

  def num_lanes = pos_lanes.length + neg_lanes.length

  def to_xml = <road name={name} type={road_type} osmid={osm_id.toString}
                     v1={v1.id.toString} v2={v2.id.toString} id={id.toString}>
                 {points.map(pt => pt.to_xml)}
               </road>

  override def toString = name + " [R" + id + "]"
  
  // TODO overload equality better? also, bizarre unreachable code bugs.
  def incoming_lanes(v: Vertex): MutableList[Edge] = {
    if (v == v1) {
      return neg_lanes
    } else if (v == v2) {
      return pos_lanes
    } else {
      throw new Exception("road doesnt involve v")  // TODO
    }
    /*v match {
      case v1 => neg_lanes
      case v2 => pos_lanes
      case _  => throw new Exception("road doesnt involve v") // TODO }
    }*/
  }

  def outgoing_lanes(v: Vertex): MutableList[Edge] = {
    if (v == v1) {
      return pos_lanes
    } else if (v == v2) {
      return neg_lanes
    } else {
      throw new Exception("road doesnt involve v")  // TODO
    }
    /*v match {
      case v1 => pos_lanes
      case v2 => neg_lanes
      case _  => throw new Exception("road doesnt involve v") // TODO
    }*/
  }

  // by v1: 0..1
  def first_line = new Line(points.head, points.tail.head)
  // by v2: -2..-1
  def last_line = new Line(points.dropRight(1).last, points.last)

  // clockwise is > 0, counter-clockwise is < 0
  def angle_to(r: Road): Double = {
    var from_angle = 0.0
    var to_angle = 0.0

    // find the vertex in common to determine ordering
    if (v1 == r.v1) {
      from_angle = first_line.angle
      to_angle   = r.first_line.angle
    } else if (v1 == r.v2) {
      from_angle = first_line.angle
      to_angle   = r.last_line.angle
    } else if (v2 == r.v1) {
      from_angle = last_line.angle
      to_angle   = r.first_line.angle
    } else if (v2 == r.v2) {
      from_angle = last_line.angle
      to_angle   = r.last_line.angle
    } else {
      throw new Exception("roads dont meet up") // TODO
    }

    println("rotate from " + from_angle.toDegrees + " to " + to_angle.toDegrees)

    // which way is the shortest rotation?
    // can we just use dot product after making vectors from the two angles?
    val l1 = new Line(0, 0, math.cos(from_angle), math.sin(from_angle))
    val l2 = new Line(0, 0, math.cos(to_angle), math.sin(to_angle))
    return l1.angle_to(l2)
  }
    

  def pairs_of_points = points zip points.tail
}
