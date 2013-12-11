// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import scala.collection.mutable
import java.awt.Color

import utexas.aorta.map.Coordinate
import utexas.aorta.common.RNG

// These are useless graphical effects for fun.

class SnowEffect(canvas: ScrollingCanvas) {
  private val rng = new RNG()
  private def new_flake = rng.percent(30)
  private def rand_width = rng.int(10, 50)
  private def rand_fall_speed = rng.double(1, 10)
  private def rand_pos = rng.double(0, canvas.size.width)

  private val flakes = new mutable.HashSet[Snowflake]()

  def move() {
    if (new_flake) {
      flakes += new Snowflake(rand_width, rand_pos, rand_fall_speed)
    }
    flakes.foreach(f => f.move())
    flakes --= flakes.filter(f => f.y > canvas.size.height)
  }

  def render(state: GuiState) {
    flakes.foreach(f => f.render(state))
  }
}

class Snowflake(width: Int, x1: Double, fall_speed: Double) {
  private var x = x1
  private var sway = width  // TODO start left/right randomly?
  var y = 0.0

  def render(state: GuiState) {
    state.g2d.setColor(Color.WHITE) // TODO shades
    // TODO more than just circles! make shapes.
    state.g2d.fill(state.bubble(new Coordinate(
      // To float or not?
      state.canvas.screen_to_map_x(x), state.canvas.screen_to_map_y(y)
    )))
  }

  def move() {
    y += fall_speed

    if (sway > 0) {
      x += 1
      sway -= 1
      if (sway == 0) {
        sway = -width
      }
    } else if (sway < 0) {
      x -= 1
      sway += 1
      if (sway == 0) {
        sway = width
      }
    }
  }
}
