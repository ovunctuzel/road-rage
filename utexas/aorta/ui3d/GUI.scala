// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui3d

import utexas.aorta.common.Util
import utexas.aorta.sim.Simulation

import org.lwjgl.opengl.GL11
import org.lwjgl.input.Keyboard

class GUI(sim: Simulation) extends ScrollingCanvas with PauseToggling {
  override def setup_opengl() {
    super.setup_opengl()
    // Gray background
    GL11.glClearColor(0.75f, 0.75f, 0.75f, 1)
  }

  override def render() {
    // Roads...
    GL11.glColor3d(0, 0, 0)
    for (road <- sim.graph.roads) {
      GL11.glLineWidth(3.6f * road.num_lanes)
      GL11.glBegin(GL11.GL_LINE_STRIP)
      for (pt <- road.points) {
        GL11.glVertex3d(pt.x, pt.y, 0)
      }
      GL11.glEnd()
    }

    // Simulate a little
    if (!paused) {
      sim.step()
      println(s"its ${sim.tick}")
    }
  }
}

object GUI {
  def main(args: Array[String]) {
    val sim = Util.process_args(args)
    new GUI(sim).run()
  }
}

trait PauseToggling extends Controls {
  var paused = true

  // TODO a hack to just detect a keypress
  private var keydown = false

  override def handle_input() {
    if (keydown && !Keyboard.isKeyDown(Keyboard.KEY_P)) {
      keydown = false
      paused = !paused
    }
    if (!keydown && Keyboard.isKeyDown(Keyboard.KEY_P)) {
      keydown = true
    }

    super.handle_input()
  }
}
