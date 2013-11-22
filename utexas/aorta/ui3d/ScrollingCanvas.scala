// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui3d

import org.lwjgl.LWJGLException
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{Display, DisplayMode, GL11}
import org.lwjgl.util.glu.{GLU, Sphere}

// TODO note: cool trick... to lower the camera and approach an angle somewhere, just lower eye
// height but hold thing youre looking at fixed
// TODO maybe middle click+hold does the above effect by holding the clicked point fixed

// TODO click+drag of the cube should keep the cube under the cursor the whole time.

trait Controls {
  // TODO defaults should change
  var zoom = 35.0
  var x_off = 1200.0
  var y_off = 887.0
  var angle = 0.0

  // Subclasses should call super.handle_input() as the last step
  def handle_input() {}
}

trait MousePanning extends Controls {
  // Constants
  private val left_mouse = 0

  private var click_x, click_y = 0.0
  private var base_x, base_y = 0.0
  private var panning_mousedown = false
  private def mouse_at_x = Mouse.getX
  private def mouse_at_y = Mouse.getY

  override def handle_input() {
    if (!panning_mousedown && Mouse.isButtonDown(left_mouse)) {
      panning_mousedown = true
      click_x = mouse_at_x
      click_y = mouse_at_y
      base_x = x_off
      base_y = y_off
    }
    if (!Mouse.isButtonDown(left_mouse)) {
      panning_mousedown = false
    }

    if (panning_mousedown) {
      val dx = click_x - mouse_at_x
      val dy = click_y - mouse_at_y
      x_off = base_x + dx
      y_off = base_y + dy
    }

    super.handle_input()
  }
}

trait MouseZooming extends Controls {
  // Constants
  private val slow_zoom_step = (1.0 / 120) * 5
  private val fast_zoom_step = (1.0 / 120) * 30
  private val min_zoom = 0.1

  override def handle_input() {
    val scroll_delta = Mouse.getDWheel()
    if (scroll_delta != 0) {
      val step = if (Keyboard.isKeyDown(Keyboard.KEY_LCONTROL)) fast_zoom_step else slow_zoom_step
      zoom = math.max(min_zoom, zoom - (step * scroll_delta))
      // make mouse still point at the same thing after zooming?
      //x_off = ((zoom / old_zoom) * (mouse_at_x + x_off)) - mouse_at_x
      //y_off = ((zoom / old_zoom) * (mouse_at_y + y_off)) - mouse_at_y
    }

    super.handle_input()
  }
}

trait KeyboardRotating extends Controls {
  // Constants
  private val rotate_delta = 1.0

  override def handle_input() {
    if (Keyboard.isKeyDown(Keyboard.KEY_LEFT)) {
      angle -= rotate_delta
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) {
      angle += rotate_delta
    }

    super.handle_input()
  }
}

trait KeyboardPanning extends Controls {
  override def handle_input() {
    if (Keyboard.isKeyDown(Keyboard.KEY_LEFT)) {
      x_off -= 1
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) {
      x_off += 1
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_UP)) {
      y_off += 1
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_DOWN)) {
      y_off -= 1
    }

    super.handle_input()
  }
}

// TODO descr coord system.
abstract class ScrollingCanvas() extends Controls
  with MousePanning with MouseZooming with KeyboardPanning
{
  // constants...
  private val screen_width = 800
  private val screen_height = 600
  private val fps = 60
  private val field_of_view = 45
  private val max_view_dist = 1000

  def render()

  def run() {
    try {
      Display.setDisplayMode(new DisplayMode(screen_width, screen_height))
      Display.create()
    } catch {
      case e: LWJGLException => sys.exit()
    }
 
    setup_opengl()
 
    while (!Display.isCloseRequested()) {
      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT)
      GL11.glLoadIdentity()

      handle_input()
      set_camera()
      render()

      // TODO for debug: Draw the point we're looking at
      GL11.glColor3d(1, 1, 1)
      Primitives.sphere(x_off, y_off, 0)

      Display.update()
      Display.sync(fps)
    }
    Display.destroy()
  }

  protected def setup_opengl() {
    GL11.glMatrixMode(GL11.GL_PROJECTION)
    GL11.glLoadIdentity()
    GLU.gluPerspective(
      field_of_view, Display.getWidth.toFloat / Display.getHeight, 1, max_view_dist
    )
    GL11.glMatrixMode(GL11.GL_MODELVIEW)
    GL11.glEnable(GL11.GL_DEPTH_TEST)
  }

  private def set_camera() {
    GLU.gluLookAt(
      // The camera position
      x_off.toFloat, y_off.toFloat, zoom.toFloat,
      // The target to look at
      x_off.toFloat, y_off.toFloat, 0,
      // The "up" direction. TODO whys poking this fix orientation?
      0, 1, 0
    )
  }
}
