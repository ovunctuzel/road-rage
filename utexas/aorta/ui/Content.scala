// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import Function.tupled
import java.awt.Color
import java.awt.geom.{Line2D, Rectangle2D}
import scala.collection.mutable

import utexas.aorta.map.{Coordinate, Edge, Line, Vertex, Zone, Road, Direction, RoadArtifact}
import utexas.aorta.sim.drivers.Agent

import utexas.aorta.common.{cfg, RNG}

trait Renderable {
  def debug(): Unit
  def tooltip(): List[String] = List(toString)

  // TODO someday, right-click context menus!
}

class DrawDriver(val agent: Agent, state: GuiState) {
  // When we don't want to mark this agent in a special way, display a random
  // but fixed color
  val personal_color = GeomFactory.rand_color

  def hits(bbox: Rectangle2D.Double) = agent_bubble.intersects(bbox)

  def render() {
    state.g2d.setColor(ColorScheme.color(this, state))
    if (state.canvas.zoomed_in) {
      // TODO cfg. just tweak these by sight.
      val vehicle_length = 0.2  // along the edge
      val vehicle_width = 0.15  // perpendicular

      var (line, front_dist) = agent.at.on.current_pos(agent.at.dist)
      agent.lc.old_lane match {
        case Some(l) => {
          val (line2, _) = l.current_pos(agent.at.dist)
          // TODO I'd think 1 - progress should work, but by visual inspection,
          // apparently not.
          val progress = (agent.lc.lanechange_dist_left / cfg.lanechange_dist)
          line = line.add_scaled(line2, progress)
        }
        case None =>
      }
      val front_pt = line.point_on(front_dist)

      // the front center of the vehicle is where the location is. ascii
      // diagrams are hard, but line up width-wise
      val rect = new Rectangle2D.Double(
        front_pt.x - vehicle_length, front_pt.y - (vehicle_width / 2),
        vehicle_length, vehicle_width
      )
      // play rotation tricks
      // TODO val g2d_rot = g2d.create
      // rotate about the front center point, aka, keep that point fixed/pivot
      state.g2d.rotate(-line.angle, front_pt.x, front_pt.y)
      state.g2d.fill(rect)
      // TODO undoing it this way is dangerous... try to make a new g2d context
      // and dispose of it
      state.g2d.rotate(line.angle, front_pt.x, front_pt.y)

      if (state.show_tooltips) {
        state.tooltips += Tooltip(
          agent.at.location.x, agent.at.location.y, agent.wallet.tooltip,
          agent.wallet.dark_tooltip
        )
      }
    } else {
      state.g2d.fill(agent_bubble)
    }
  }

  private def agent_bubble = state.bubble(agent.at.location)

  def moused_over() {
    // Circle our main constraint
    state.g2d.setColor(Color.WHITE)
    state.g2d.setStroke(GeomFactory.center_stroke)
    agent.cur_queue.ahead_of(agent) match {
      case Some(a) => {
        state.g2d.draw(state.bubble(a.at.location))
      }
      case None => {
        // Waiting on the intersection, then
        state.g2d.draw(state.bubble(agent.cur_vert.location))
      }
    }
    // Draw where we're headed
    // TODO deal with multiple per vert in a much better way
    agent.all_tickets(agent.cur_vert.intersection).headOption match {
      case Some(t) => state.draw_turn(t.turn, Color.WHITE)
      case None =>
    }
  }
}

// TODO tmp way of doing this.
object ZoneColor {
  private val rng = new RNG()
  private val colors = new mutable.HashMap[Zone, Color]()
  def color(zone: Zone): Color = {
    if (!colors.contains(zone)) {
      colors(zone) = new Color(rng.int(0, 255), rng.int(0, 255), rng.int(0, 255))
    }
    return colors(zone)
  }
}

class DrawRoad(val r: Road, state: GuiState) {
  val edges = r.lanes.map(e => new DrawEdge(e, state))

  val lines = r.lines.map(l => new Line2D.Double(l.x1, l.y1, l.x2, l.y2))
  // Use the positive direction to render the yellow center line
  val center_lines = if (r.dir == Direction.POS)
    r.points.zip(r.points.tail).map(
      pair => new Line2D.Double(pair._1.x, pair._1.y, pair._2.x, pair._2.y)
    )
  else
    Array[Line2D.Double]()

  def hits(bbox: Rectangle2D.Double) = lines.exists(l => l.intersects(bbox))

  def render_road() {
    if (!state.canvas.zoomed_in && state.route_members.contains(r)) {
      state.g2d.setStroke(GeomFactory.strokes(2 * r.num_lanes))
    } else {
      state.g2d.setStroke(GeomFactory.strokes(r.num_lanes))
    }
    state.g2d.setColor(color)
    lines.foreach(l => state.g2d.draw(l))
  }

  def render_edges() {
    edges.foreach(e => e.render())
    state.g2d.setColor(Color.YELLOW)
    state.g2d.setStroke(GeomFactory.center_stroke)
    center_lines.foreach(l => state.g2d.draw(l))
  }

  def render_buildings() {
    state.g2d.setColor(Color.GREEN)
    for (bldg <- r.shops) {
      state.g2d.draw(state.bubble(bldg))
    }
    // TODO rectangles? triangles?
    state.g2d.setColor(Color.BLACK)
    for (bldg <- r.houses) {
      state.g2d.draw(state.bubble(bldg))
    }
  }

  private def color(): Color =
    if (state.chosen_road.getOrElse(null) == r)
      cfg.chosen_road_color
    else if (state.route_members.contains(r))
      state.route_members.color(r).get
    else if (state.polygon_roads1(r))
      cfg.src_polygon_color
    else if (state.polygon_roads2(r))
      cfg.dst_polygon_color
    else if (r.doomed)
      Color.RED
    else
      state.highlight_type match {
        case (Some(x)) if x == r.road_type => Color.GREEN
        // just show one zone arbitrarily
        case _ if state.show_zone_colors => ZoneColor.color(state.canvas.sim.graph.zones(r).head)
        case _ => Color.BLACK
      }
}

class DrawRoadArtifact(val a: RoadArtifact, state: GuiState) {
  val lines = a.lines.map(l => new Line2D.Double(l.x1, l.y1, l.x2, l.y2))

  def hits(bbox: Rectangle2D.Double) = lines.exists(l => l.intersects(bbox))

  def render_road() {
    // 2 lanes to make the artifact cover both sides of the road
    state.g2d.setStroke(GeomFactory.strokes(2))
    state.g2d.setColor(color)
    lines.foreach(l => state.g2d.draw(l))
  }

  private def color() = new Color(14, 14, 14) // dark gray to blend with the road's black
}

class DrawEdge(val edge: Edge, state: GuiState) {
  private val lines = edge.lines.map(shift_edge_line)

  def hits(bbox: Rectangle2D.Double) = lines.exists(l => l.intersects(bbox))

  def render() {
    // TODO forget about the arrows, unless maybe highlighted?
    /*GeomFactory.draw_arrow(l, l.midpt, 1)
    state.g2d.setColor(cfg.lane_color)
    state.g2d.fill(l.arrow)*/

    state.g2d.setStroke(GeomFactory.lane_stroke)
    state.g2d.setColor(color)
    for (l <- lines) {
      state.g2d.draw(l)
    }
  }

  private def color(): Color =
    // TODO cfg
    if (state.chosen_edge1.getOrElse(null) == edge)
      Color.BLUE
    else if (state.chosen_edge2.getOrElse(null) == edge)
      Color.RED
    else if (edge.doomed)
      Color.RED
    else
      Color.WHITE

  private def shift_edge_line(line: Line): Line2D.Double = {
    // Draw the lines on the borders of lanes, not in the middle
    val l = line.perp_shift(0.5)
    return new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  }
}
