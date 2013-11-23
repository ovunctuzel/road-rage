// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui3d

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.Sphere

object Primitives {
  private val sphere_template = new Sphere()
  private val radius = 1

  def sphere(x: Double, y: Double, z: Double) {
    GL11.glPushMatrix()
    GL11.glTranslated(x.toFloat, y.toFloat, z.toFloat)
    sphere_template.draw(radius, 4, 4)
    GL11.glPopMatrix()
  }
}

case class Color(r: Double, g: Double, b: Double) {
  def use() {
    GL11.glColor3d(r, g, b)
  }
}

object Color {
  val WHITE = Color(1, 1, 1)
  val BLACK = Color(0, 0, 0)
  val RED   = Color(1, 0, 0)
  val GREEN = Color(0, 1, 0)
  val BLUE =  Color(0, 0, 1)
}
