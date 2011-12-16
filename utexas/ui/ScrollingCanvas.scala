package utexas.ui

import swing._  // TODO figure out exactly what
import swing.event._
import java.awt.{Color,RenderingHints}
import java.awt.geom.AffineTransform

// TODO maybe adaptor class for our map geometry?

abstract class ScrollingCanvas extends Component {
  override def background = Color.LIGHT_GRAY

  // this defines the current viewing window
  protected var zoom = 1.0
  protected var x_off, y_off = 0.0

  // this defines controls
  protected var key_speed = 10
  protected var zoom_step = 0.1
  protected var zoom_mult = 10  // it's kind of weird why this isn't just ^

  // translate between screen and map coordinate systems
  // define (x_off, y_off) to be the top-left corner of the screen
  def screen_to_map_x(x: Double) = (x + x_off) / zoom
  def screen_to_map_y(y: Double) = (y + y_off) / zoom
  def map_to_screen_x(x: Double) = (x - x_off) * zoom
  def map_to_screen_y(y: Double) = (y - y_off) * zoom

  // react to mouse events, tracking various parameters
  protected var mouse_at_x, mouse_at_y = 0.0
  protected var click_x, click_y = 0.0
  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  listenTo(mouse.wheel)
  //listenTo(keyboard)    // TODO

  reactions += {
    // TODO fast_mode
    case e: MouseMoved => {
      // TODO nicer reassignment?
      mouse_at_x = e.point.x
      mouse_at_y = e.point.y
      // TODO publish event
    }

    case e: MousePressed => {
      click_x = e.point.x
      click_y = e.point.y
    }

    case e: MouseDragged => {
      val dx = click_x - e.point.x
      val dy = click_y - e.point.y

      // TODO use screen<->map formulas better?
      x_off += dx
      y_off += dy

      fix_oob()

      // show the pan
      repaint()

      // reset for the next event
      click_x = e.point.x
      click_y = e.point.y
      mouse_at_x = e.point.x
      mouse_at_y = e.point.y
      // TODO publish event. aka, mouseMoved.
    }

    case e: MouseWheelMoved => {
      val old_zoom = zoom
      val dz = e.rotation * zoom_mult
      zoom -= zoom_step * dz
      // cap zoom
      zoom = math.max(zoom_step, zoom)

      // make screen_to_map of mouse_at still point to the same thing after
      // zooming. TODO comment the derivation; it was a proportion
      x_off = ((zoom / old_zoom) * (mouse_at_x + x_off)) - mouse_at_x
      y_off = ((zoom / old_zoom) * (mouse_at_y + y_off)) - mouse_at_y

      fix_oob()
      // show the zoom
      repaint()
    }

  }

  // prevent coordinates from leaving the canvas
  private def fix_oob() = {
    // upper logical bounds of the current screen
    val x2 = screen_to_map_x(size.width)
    val y2 = screen_to_map_y(size.height)

    val x_fix = canvas_width - x2
    val y_fix = canvas_height - y2
    if (x_fix < 0) {
      x_off += x_fix
    }
    if (y_fix < 0) {
      y_off += y_fix
    }

    // the lower logical bounds are, of course, the origin
    x_off = math.max(0, x_off)
    y_off = math.max(0, y_off)
  }

  override def paintComponent(g2d: Graphics2D) = {
    // clear things
    super.paintComponent(g2d)

    // Perform the transformations to mimic scrolling and zooming
    val transform = new AffineTransform()
    transform.translate(-x_off, -y_off)
    transform.scale(zoom, zoom)
    g2d.transform(transform)

    // TODO antialias cfg
    // ew, clunky old java crap.
    val antialiasing = new java.util.HashMap[Any,Any]()
    antialiasing.put(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )
    g2d.setRenderingHints(antialiasing)

    // what logical coordinates are in view?
    val x1 = screen_to_map_x(0)
    val y1 = screen_to_map_y(0)
    val x2 = screen_to_map_x(size.width)
    val y2 = screen_to_map_y(size.height)
    // do the actual work
    render_canvas(g2d, x1, y1, x2, y2, zoom)
  }

  // implement these
  def render_canvas(g2d: Graphics2D, x1: Double, y1: Double,
                    x2: Double, y2: Double, zoom: Double)
  def canvas_width: Int
  def canvas_height: Int

  // TODO callback mechanism of some sort.
}
