// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui3d

import org.lwjgl.LWJGLException
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{Display, DisplayMode, GL11}
import org.lwjgl.util.glu.{GLU, Sphere}

object Primitives {
  private val sphere_template = new Sphere()
  private val radius = 1

  def sphere(x: Double, y: Double, z: Double) {
    GL11.glPushMatrix()
    GL11.glTranslated(x.toFloat, y.toFloat, z.toFloat)
    new Sphere().draw(radius, 4, 4)
    GL11.glPopMatrix()
  }
}
