// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.Color

import utexas.aorta.map.Vertex
import utexas.aorta.sim.AgentMap

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.common.cfg

// The hierarchy: ReplayDiffScheme, FocusVertexScheme, CameraScheme, StalledScheme, PersonalScheme
// TODO make that order explicit here. color(...): Option[Color]
object ColorScheme {
  // TODO choose based on if replay is enabled
  def color(d: DrawDriver, state: GuiState) = FocusVertexScheme.color(d, state)
}

// Reveal who's acting differently from their past life
// TODO disable sometimes
object ReplayDiffScheme {
  private val deltas = new AgentMap[Double](0.0)

  def add_delta(id: Int, delta: Double) {
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

// Focus on tickets at one intersection
object FocusVertexScheme {
  def color(d: DrawDriver, state: GuiState) = state.current_obj match {
    case Some(v: Vertex) => d.agent.all_tickets(v.intersection).toList match {
      case Nil => Color.GRAY
      case ls if ls.find(_.is_approved).isDefined => Color.GREEN
      case ls if ls.find(_.is_interruption).isDefined => Color.YELLOW
      case _ => Color.RED
    }
    case _ => CameraScheme.color(d, state)
  }
}

// Focus on the agent followed by the camera
object CameraScheme {
  def color(d: DrawDriver, state: GuiState) = state.camera_agent match {
    case Some(a) if d.agent == a => Color.WHITE
    case _ => StalledScheme.color(d, state)
  }
}

// Focus on cars that aren't moving
object StalledScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (d.agent.how_long_idle >= 30.0)
      Color.RED
    else
      PersonalScheme.color(d, state)
}

// Color each car individually
object PersonalScheme {
  def color(d: DrawDriver, state: GuiState) =
    if (state.canvas.zoomed_in)
      d.personal_color
    else
      Color.BLUE
}
