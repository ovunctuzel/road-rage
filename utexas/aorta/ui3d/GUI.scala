// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui3d

import utexas.aorta.common.Util
import utexas.aorta.sim.Simulation

import org.lwjgl.opengl.GL11

class GUI(sim: Simulation) extends ScrollingCanvas {
  override def setup_opengl() {
    super.setup_opengl()
    // Gray background
    GL11.glClearColor(0.75f, 0.75f, 0.75f, 1)
  }

  override def render() {
    // Roads...
    GL11.glColor3d(0, 0, 0)
    for (road <- sim.graph.roads) {
      GL11.glLineWidth(1 * road.num_lanes)
      GL11.glBegin(GL11.GL_LINE_STRIP)
      for (pt <- road.points) {
        GL11.glVertex3d(pt.x, pt.y, 0)
      }
      GL11.glEnd()
    }
  }
}

object GUI {
  def main(args: Array[String]) {
    val sim = Util.process_args(args)
    new GUI(sim).run()
  }
}
