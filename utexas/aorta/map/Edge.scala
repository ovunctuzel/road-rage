// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import java.io.FileWriter

import utexas.aorta.cfg

// TODO subclass Edge for pos/neg.. seems easier for lots of things

// TODO var id due to tarjan
class Edge(var id: Int, val road: Road, val dir: Direction.Direction) extends Traversable {
  var lane_num: Int = -1  // TODO needs to be initialized to be defined.. bleh.

  // This is diagnostic.
  var doomed = false

  // no lane-changing
  //def leads_to = next_turns
  // with lane-changing
  def leads_to = next_turns ++ List(shift_left, shift_right).flatten
  def speed_limit = road.speed_limit

  def directed_road = if (dir == Direction.POS)
                        road.pos_group
                      else
                        road.neg_group
  def turns_leading_to(group: DirectedRoad) =
    next_turns.filter(t => t.to.directed_road == group)

  def other_lanes = if (dir == Direction.POS) road.pos_lanes else road.neg_lanes
  def other_vert(v: Vertex) = road.other_vert(v)
  def opposite_lanes = if (dir == Direction.POS) road.neg_lanes else road.pos_lanes
  def rightmost_lane = other_lanes.head
  def leftmost_lane  = other_lanes.last

  def shift_left: Option[Edge]  = if (is_leftmost)  None else Some(other_lanes(lane_num + 1))
  def shift_right: Option[Edge] = if (is_rightmost) None else Some(other_lanes(lane_num - 1))
  def adjacent_lanes: List[Edge] = List(shift_left, shift_right, Some(this)).flatten
  def best_adj_lane(to_reach: Edge)
    = adjacent_lanes.sortBy(e => math.abs(to_reach.lane_num - e.lane_num)).head

  def next_turns = to.turns_from(this)
  def prev_turns = from.turns_to(this)
  def succs: List[Edge] = next_turns.map(t => t.to)
  def preds: List[Edge] = prev_turns.map(t => t.from)
  def right_turns = next_turns.filter(t => t.turn_type == TurnType.RIGHT)
  def left_turns  = next_turns.filter(t => t.turn_type == TurnType.LEFT)
  def crosses     = next_turns.filter(t => t.turn_type == TurnType.CROSS)
  def right_turns_to = right_turns.map(t => t.to)
  def left_turns_to  = left_turns.map(t => t.to)
  def crosses_to     = crosses.map(t => t.to)

  def is_rightmost = lane_num == 0
  def is_leftmost  = lane_num == other_lanes.size - 1

  // It really is this simple.
  def next_counterclockwise_to: Option[Edge] =
    if (is_rightmost) {
      val ordering = right_turns_to ++ crosses_to ++ left_turns_to
      ordering.headOption
    } else {
      Some(other_lanes(lane_num - 1))
    }

  // not for one-ways right now. but TODO it'd be cool to put that here.
  def lane_offset = other_lanes.length - lane_num

  override def toString = "Lane %s%d of %s (%d)".format(dir, lane_num, road.name, id)
  //override def toString = "Lane %d".format(id)

  def from: Vertex = if (dir == Direction.POS) road.v1 else road.v2
  def to: Vertex   = if (dir == Direction.POS) road.v2 else road.v1

  def to_xml(out: FileWriter) = {
    val is_doomed = if (doomed)
                      " doomed=\"absolutely\""
                    else
                      ""
    out.write(
      "  <edge id=\"" + id + "\" road=\"" + road.id + "\" dir=\"" + dir
      + "\" laneNum=\"" + lane_num + "\"" + is_doomed + ">\n"
    )
    lines.foreach(l => l.to_xml(out))
    out.write("  </edge>\n")
  }

  def to_plaintext(out: FileWriter) = {
    out.write(
      id + "," + road.id + "," + dir + "," + lane_num + "," + doomed + ":"
    )
    lines.foreach(l => l.to_plaintext(out))
    out.write("\n")
  }

  //////// Geometry. TODO separate somewhere?

  // recall + means v1->v2, and that road's points are stored in that order
  // what's the first line segment we traverse following this lane?
  def first_road_line = if (dir == Direction.POS)
                          new Line(road.points.head, road.points.tail.head) // 0 -> 1
                        else
                          new Line(road.points.last, road.points.dropRight(1).last) // -1 -> -2
  // what's the last line segment we traverse following this lane?
  def last_road_line = if (dir == Direction.POS)
                         new Line(road.points.dropRight(1).last, road.points.last) // -2 -> -1
                       else
                         new Line(road.points.tail.head, road.points.head) // 1 -> 0

  // this takes a point along a line and moves it back
  private val shift_mag = 1.0 // TODO cfg
  // TODO y inversion problems still?!
  private def shift_pt(x: Double, y: Double, theta: Double) = new Coordinate(
    x + (shift_mag * math.cos(theta)), y - (shift_mag * math.sin(theta))
  )
  def shifted_start_pt(l: Line = lines.head) = shift_pt(l.x1, l.y1, l.angle)
  def shifted_end_pt(l: Line = lines.last)
    = shift_pt(l.x2, l.y2, l.angle + math.Pi)  // reverse the angle
}

// TODO noooo not var >_<
class Line(var x1: Double, var y1: Double, var x2: Double, var y2: Double) {
  // Compute and store it once, since the math isn't free
  // TODO the other math has issues with the synthetic map
  //val length = math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
  val length = Coordinate.gps_dist_in_meters(
    Graph.world_to_gps(x1, y1), Graph.world_to_gps(x2, y2)
  )

  def this(pt1: Coordinate, pt2: Coordinate) = this(pt1.x, pt1.y, pt2.x, pt2.y)
  def this(v1: Vertex, v2: Vertex) = this(v1.location, v2.location)

  // return [0, 2pi) like a reasonable bloody...
  // also, this is a place where we have to recall the coordinate system has y
  // increasing down, but trig thinks y increases up...
  def angle: Double = {
    val theta = math.atan2(y1 - y2, x2 - x1)  // y inversion
    return if (theta < 0)
      theta + (2 * math.Pi)
    else
      theta
  }

  // TODO shiftline() broke with the seemingly correct angle(). testing.
  def broken_angle = math.atan2(y2 - y1, x2 - x1)

  def midpt = new Coordinate((x1 + x2) / 2, (y1 + y2) / 2)
  def width = x2 - x1
  def height = y2 - y1
  def start = new Coordinate(x1, y1)
  def end = new Coordinate(x2, y2)

  override def toString = "%f, %f ---> %f, %f".format(x1, y1, x2, y2)

  def to_xml(out: FileWriter) = {
    out.write(
      "    <line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\""
      + y2 + "\"/>\n"
    )
  }

  def to_plaintext(out: FileWriter) = {
    out.write(
      x1 + "," + y1 + "," + x2 + "," + y2 + ";"
    )
  }

  // assuming the two lines share an origin
  def dot(l2: Line) = (x2 - x1) * (l2.x2 - l2.x1) + (y2 - y1) * (l2.y2 - l2.y1)

  // this is line intersection, not line segment.
  def intersection(other: Line): Option[Coordinate] = {
    // Ripped from http://www.java-gaming.org/index.php?topic=22590.0
    def det(a: Double, b: Double, c: Double, d: Double) = (a * d) - (b * c)

    val x3 = other.x1
    val x4 = other.x2
    val y3 = other.y1
    val y4 = other.y2

    val det1And2 = det(x1, y1, x2, y2)
    val det3And4 = det(x3, y3, x4, y4)
    val detDiff = det(x1 - x2, y1 - y2, x3 - x4, y3 - y4)
    return if (detDiff <= cfg.epsilon) {
      None  // parallel
    } else {
      val x = det(det1And2, x1 - x2, det3And4, x3 - x4) / detDiff
      val y = det(det1And2, y1 - y2, det3And4, y3 - y4) / detDiff
      return Some(new Coordinate(x, y))
    }
  }

  // where are we on this line? even handles negative distances
  def point_on(dist_along: Double): Coordinate = {
    val percent = dist_along / length
    return new Coordinate(
      x1 + (width * percent),
      y1 + (height * percent)
    )
  }

  def shift_line(off: Double): Line = {
    // just move in the direction of the road (as given by the ordering of the
    // points) plus 90 degrees clockwise
    // TODO why does the angle() that respects inversion fail?
    val theta = broken_angle + (math.Pi / 2)
    val dx = off * cfg.lane_width * math.cos(theta)
    val dy = off * cfg.lane_width * math.sin(theta)

    return new Line(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  }

  // When percent = 0, returns this. When percent = 1, return l. Otherwise,
  // interpolates between the two lines.
  def add_scaled(l: Line, percent: Double) = new Line(
    x1 + ((l.x1 - x1) * percent), y1 + ((l.y1 - y1) * percent),
    x2 + ((l.x2 - x2) * percent), y2 + ((l.y2 - y2) * percent)
  )
}

object Direction extends Enumeration {
  type Direction = Value
  val POS = Value("+")  // v1 -> v2
  val NEG = Value("-")  // v2 -> v1
}

// Represent a group of directed edges on one road
class DirectedRoad(val road: Road, val dir: Direction.Direction) {
  override def toString = "%s's %s lanes".format(road, dir)

  def edges = if (dir == Direction.POS)
                road.pos_lanes
              else
                road.neg_lanes

  def start_pt = edges.head.from.location
  def end_pt = edges.head.to.location
  // TODO dont assume some edge being lane-changeable means others are too
  // TODO could even predict/take into account the current distance to see if
  // there's room left
  def naive_leads_to = edges.flatMap(_.succs).map(_.directed_road).toSet
  def leads_to(from: Edge) = if (from.queue.ok_to_lanechange)
                               naive_leads_to
                             else
                               from.succs.map(_.directed_road).toSet

  def length = road.length
}
