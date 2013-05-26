// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color
import java.awt.geom.{Line2D, Rectangle2D}

import utexas.aorta.map.{Coordinate, Line, Road, Vertex}
import utexas.aorta.sim.Agent

import utexas.aorta.cfg

trait Renderable {
  def debug(): Unit
  def tooltip(): List[String] = List(toString)

  // TODO someday, right-click context menus!
}

class DrawDriver(val agent: Agent, state: GuiState) {
  // When we don't want to mark this agent in a special way, display a random
  // but fixed color
  private val personal_color = GeomFactory.rand_color

  def render() {
    state.g2d.setColor(color)
    if (state.canvas.zoomed_in) {
      // TODO cfg. just tweak these by sight.
      val vehicle_length = 0.2  // along the edge
      val vehicle_width = 0.15  // perpendicular

      var (line, front_dist) = agent.at.on.current_pos(agent.at.dist)
      agent.old_lane match {
        case Some(l) => {
          val (line2, more_dist) = l.current_pos(agent.at.dist)
          // TODO I'd think 1 - progress should work, but by visual inspection,
          // apparently not.
          val progress = (agent.lanechange_dist_left / cfg.lanechange_dist)
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

  def color(): Color = state.current_obj match {
    case Some(v: Vertex) => agent.all_tickets(v.intersection).toList match {
      case Nil => Color.GRAY
      case ls if ls.find(_.is_approved).isDefined => Color.GREEN
      case ls if ls.find(_.is_interruption).isDefined => Color.YELLOW
      case _ => Color.RED
    }
    case _ => state.camera_agent match {
      case Some(a) if agent == a => Color.WHITE
      case _ =>
        // try to avoid flashing red, this feature is used to visually spot true
        // clumps
        if (agent.how_long_idle >= 30.0)
          Color.RED
        else if (!state.canvas.zoomed_in)
          Color.GRAY
        else
          personal_color
    }
  }

  def agent_bubble = state.bubble(agent.at.location)
}

class DrawRoad(val road: Road, state: GuiState) {
  private val center_lines = road.pairs_of_points.map(pair => new Line2D.Double(
    pair._1.x, pair._1.y, pair._2.x, pair._2.y
  ))

  // A heuristic used for quick collision checks. Will have false negatives.
  // TODO breaks mousing over roads with weird shapes.
  val collision_line = new Line2D.Double(
    road.points.head.x, road.points.head.y,
    road.points.last.x, road.points.last.y
  )

  def render() {
    state.g2d.setColor(color)
    if (state.route_members(road) && !state.canvas.zoomed_in) {
      state.g2d.setStroke(GeomFactory.strokes(road.num_lanes * 2))
    } else {
      state.g2d.setStroke(GeomFactory.strokes(road.num_lanes))
    }

    render_bg_line()
  }

  def render_bg_line() {
    center_lines.foreach(l => state.g2d.draw(l))
  }

  // Assume the stroke and color have been set by our caller, for efficiency.
  def render_center_line() {
    center_lines.foreach(l => state.g2d.draw(l))
  }

  def color(): Color =
    if (state.chosen_road.getOrElse(null) == road)
      cfg.chosen_road_color
    else if (state.route_members(road))
      cfg.route_member_color
    else if (state.polygon_roads1(road))
      cfg.src_polygon_color
    else if (state.polygon_roads2(road))
      cfg.dst_polygon_color
    else if (road.doomed)
      Color.RED
    else
      state.highlight_type match {
        case (Some(x)) if x == road.road_type => Color.GREEN
        case _                             => Color.BLACK
      }
}

class DrawOneWayRoad(r: Road, state: GuiState) extends DrawRoad(r, state) {
  val bg_lines = road.pairs_of_points.map(shift_line)

  override def render_bg_line() {
    bg_lines.foreach(l => state.g2d.draw(l))
  }

  private def shift_line(pair: (Coordinate, Coordinate)): Line2D.Double = {
    val l = new Line(pair._1, pair._2).perp_shift(road.num_lanes / 2.0)
    return new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  }
}
