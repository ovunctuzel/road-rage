// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color
import java.awt.geom.Rectangle2D

import utexas.aorta.map.Vertex
import utexas.aorta.sim.Agent

import utexas.aorta.cfg

trait Renderable {
  def debug(): Unit
  def tooltip(): List[String] = List(toString)

  // TODO someday, right-click context menus!
}

class DrawDriver(val a: Agent, state: GuiState) {
  // When we don't want to mark this agent in a special way, display a random
  // but fixed color
  private val personal_color = GeomFactory.rand_color

  def render() {
    // Do a cheap intersection test before potentially expensive rendering
    // work
    if (!agent_bubble.intersects(state.window)) {
      return
    }

    state.g2d.setColor(color)
    if (state.canvas.zoomed_in) {
      // TODO cfg. just tweak these by sight.
      val vehicle_length = 0.2  // along the edge
      val vehicle_width = 0.15  // perpendicular

      var (line, front_dist) = a.at.on.current_pos(a.at.dist)
      a.old_lane match {
        case Some(l) => {
          val (line2, more_dist) = l.current_pos(a.at.dist)
          // TODO I'd think 1 - progress should work, but by visual inspection,
          // apparently not.
          val progress = (a.lanechange_dist_left / cfg.lanechange_dist)
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
          a.at.location.x, a.at.location.y, a.wallet.tooltip,
          a.wallet.dark_tooltip
        )
      }
    } else {
      state.g2d.fill(agent_bubble)
    }
  }

  def color(): Color = state.current_obj match {
    case Some(v: Vertex) => a.all_tickets(v.intersection).toList match {
      case Nil => Color.GRAY
      case ls if ls.find(_.is_approved).isDefined => Color.GREEN
      case ls if ls.find(_.is_interruption).isDefined => Color.YELLOW
      case _ => Color.RED
    }
    case _ => state.camera_agent match {
      case Some(agent) if a == agent => Color.WHITE
      case _ =>
        // try to avoid flashing red, this feature is used to visually spot true
        // clumps
        if (a.how_long_idle >= 30.0)
          Color.RED
        else if (!state.canvas.zoomed_in)
          Color.GRAY
        else
          personal_color
    }
  }

  def agent_bubble = state.bubble(a.at.location)
}
