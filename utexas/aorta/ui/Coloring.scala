// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color
import scala.collection.mutable

import utexas.aorta.map.{Vertex, Road}
import utexas.aorta.sim.AgentMap

import utexas.aorta.common.{cfg, AgentID}

class RoadColorScheme() {
  // TODO some kind of multi-layer system would be cool, if we had widgets to let people
  // toggle/order them
  private val colors = new mutable.HashMap[Road, Color]()
  private var current_layer = "default layer"

  def reset() {
    set_layer("default layer")
  }
  def set_layer(name: String) {
    colors.clear()
    current_layer = name
  }
  def set(r: Road, c: Color) {
    colors(r) = c
  }
  def unset(r: Road) {
    colors -= r
  }

  def layer = current_layer
  def apply(r: Road) = colors(r)
  def has(r: Road, layer: String) = layer == current_layer && colors.contains(r)
  def has(r: Road) = colors.contains(r)
}

object DriverColorScheme {
  def color(d: DrawDriver, state: GuiState) = Stream(
    AccelerationScheme.color _, FocusVertexScheme.color _, CameraScheme.color _,
    StalledScheme.color _, PersonalScheme.color _
  ).flatMap(fxn => fxn(d, state)).head
}

// Reveal who's acting differently from their past life
// TODO disable sometimes. also, revive this?
object ReplayDiffScheme {
  private val deltas = new AgentMap[Double](0.0)

  def add_delta(id: AgentID, delta: Double) {
    deltas.put(id, deltas.get(id) + delta)
  }

  def color(d: DrawDriver, state: GuiState) =
    if (state.canvas.zoomed_in)
      FocusVertexScheme.color(d, state)
    else deltas.get(d.agent.id) match {
      case 0.0 => Color.GRAY
      case x if x < 0.0 => Color.RED
      case _ => Color.GREEN
    }
}

object AccelerationScheme {
  var enabled = false

  def color(d: DrawDriver, state: GuiState) =
    if (!enabled)
      None
    else if (d.agent.target_accel > 0)
      Some(Color.GREEN)
    else if (d.agent.target_accel == 0)
      Some(Color.BLUE)
    else
      Some(Color.RED)
}

// Focus on tickets at one intersection
object FocusVertexScheme {
  def color(d: DrawDriver, state: GuiState) = state.current_obj match {
    case Some(v: Vertex) => d.agent.all_tickets(v.intersection).toList match {
      case Nil => Some(Color.GRAY)
      case ls if ls.exists(_.is_approved) => Some(Color.GREEN)
      case ls if ls.exists(_.is_interruption) => Some(Color.YELLOW)
      case _ => Some(Color.RED)
    }
    case _ => None
  }
}

// Focus on the agent followed by the camera
object CameraScheme {
  def color(d: DrawDriver, state: GuiState) = state.camera_agent match {
    case Some(a) if d.agent == a => Some(Color.WHITE)
    case _ => None
  }
}

// Focus on cars that aren't moving
object StalledScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (d.agent.how_long_idle >= 30.0)
      Some(Color.RED)
    else
      None
}

// Color each car individually
object PersonalScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (state.canvas.zoomed_in)
      Some(d.personal_color)
    else
      Some(Color.BLUE)
}
