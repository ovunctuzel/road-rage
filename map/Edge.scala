package map

import scala.collection.immutable.{List => ImmutableList}

// TODO subclass Edge for pos/neg.. seems easier for lots of things

class Edge(val id: Int, val road: Road, val dir: Direction.Direction) {
  var lane_num: Int = -1  // TODO needs to be initialized to be defined.. bleh.

  // TODO finally, something that isnt mutable! make them all this way.
  // TODO what is this silliness though
  var lines: List[Line] = List()

  def other_lanes = if (dir == Direction.POS) road.pos_lanes else road.neg_lanes

  // TODO rewrite sexilyer
  // the rightmost lane actually becomes the center line
  def lane_offset = if (road.is_oneway) road.num_lanes - lane_num - 1
                    else other_lanes.length - lane_num

  override def toString = "Lane %s%d of %s (%d)".format(dir, lane_num, road.name, id)

  def from: Vertex = if (dir == Direction.POS) road.v1 else road.v2
  def to: Vertex   = if (dir == Direction.POS) road.v2 else road.v1

  def to_xml = <edge id={id.toString} road={road.id.toString} dir={dir.toString}
                     laneNum={lane_num.toString}>
                 {lines.map(l => l.to_xml)}
               </edge>

  // recall + means v1->v2, and that road's points are stored in that order
  // what's the first line segment we traverse following this lane?
  def first_line = if (dir == Direction.POS)
                     new Line(road.points.head, road.points.tail.head) // 0 -> 1
                   else
                     new Line(road.points.last, road.points.dropRight(1).last) // -1 -> -2
  // what's the last line segment we traverse following this lane?
  def last_line = if (dir == Direction.POS)
                    new Line(road.points.dropRight(1).last, road.points.last) // -2 -> -1
                  else
                    new Line(road.points.tail.head, road.points.head) // 1 -> 0
}

class Line(val x1: Double, val y1: Double, val x2: Double, val y2: Double) {
  def this(pt1: Coordinate, pt2: Coordinate) = this(pt1.x, pt1.y, pt2.x, pt2.y)
  def this(v1: Vertex, v2: Vertex) = this(v1.location, v2.location)

  // return [0, 2pi) like a reasonable bloody...
  // also, this is a place where we have to recall the coordinate system has y
  // increasing down, but trig thinks y increases up...
  def angle: Double = {
    val theta = math.atan2(y1 - y2, x2 - x1)  // y inversion
    if (theta < 0)
      return theta + (2 * math.Pi)
    else
      return theta
  }

  // TODO shiftline() broke with the seemingly correct angle(). testing.
  def broken_angle = math.atan2(y2 - y1, x2 - x1)

  def midpt = new Coordinate((x1 + x2) / 2, (y1 + y2) / 2)

  override def toString = "%f, %f ---> %f, %f".format(x1, y1, x2, y2)

  def to_xml = <line x1={x1.toString} y1={y1.toString} x2={x2.toString}
                     y2={y2.toString}/>

  // TODO theres also a version of this in world's meters
  def length = math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))

  // assuming the two lines share an origin
  def dot(l2: Line) = (x2 - x1) * (l2.x2 - l2.x1) + (y2 - y1) * (l2.y2 - l2.y1)

  // line intersection and overlap and crap
}

object Direction extends Enumeration {
  type Direction = Value
  val POS = Value("+")  // v1 -> v2
  val NEG = Value("-")  // v2 -> v1
}
