// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing._  // TODO figure out exactly what
import swing.event._
import java.awt.{Color, RenderingHints, Polygon}
import java.awt.geom.{Rectangle2D, RoundRectangle2D}

import utexas.aorta.map.Coordinate

import utexas.aorta.Util

// TODO we can't hear the tab key until we figure out how to
// have 'with Component.SuperMixin' so we can do setFocusTraversalEnabled(false)

// TODO for now, we'll implement the free-hand drawing here, but eventually it
// makes more sense as some kind of nifty mixin thing.

// SuperMixin lets us get tab back
abstract class ScrollingCanvas extends Component {
  // TODO percentages, ideally!
  preferredSize = new Dimension(600, Int.MaxValue)

  // It's nice to have access to this.
  protected var status = Status_Bar

  override def focusable = true   // for keys to work

  // this defines the current viewing window. these values are arbitrary;
  // reset_window will clobber them.
  var zoom = 1.0    // public for Snapshot
  protected var x_off, y_off = 0.0
  reset_window

  // this defines controls
  // TODO cfg
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
  listenTo(keys) // TODO work damnit!

  // Free-hand drawing stuff
  protected var drawing_mode = false
  var polygon = new Polygon()

  reactions += {
    // Free-hand drawing stuff
    case KeyPressed(_, Key.Shift, _, _) => {
      drawing_mode = true
    }
    case KeyReleased(_, Key.Shift, _, _) => {
      drawing_mode = false
    }
    case e: MousePressed if drawing_mode => {
      grab_focus
      val x = screen_to_map_x(e.point.x).toInt
      val y = screen_to_map_y(e.point.y).toInt
      polygon.addPoint(x, y)
      repaint
    }
    case e: MouseDragged if drawing_mode => {
      val x = screen_to_map_x(e.point.x).toInt
      val y = screen_to_map_y(e.point.y).toInt
      if (polygon.npoints == 1) {
        // add it, so we can drag and make the first line
        polygon.addPoint(x, y)
      }
      polygon.xpoints(polygon.npoints - 1) = x
      polygon.ypoints(polygon.npoints - 1) = y
      repaint
    }
    case KeyPressed(_, Key.S, _, _) if drawing_mode => {
      // finish it off
      if (polygon.npoints < 3) {
        Util.log("A polygon needs more than one line")
      } else {
        handle_ev(EV_Select_Polygon_For_Army())
      }
      drawing_mode = false
      polygon = new Polygon()
      repaint
    }
    case KeyPressed(_, Key.P, _, _) if drawing_mode => {
      // TODO refactor
      if (polygon.npoints < 3) {
        Util.log("A polygon needs more than one line")
      } else {
        handle_ev(EV_Select_Polygon_For_Policy())
      }
      drawing_mode = false
      polygon = new Polygon()
      repaint
    }

    // TODO fast_mode
    case e: MouseMoved => {
      // TODO nicer reassignment?
      mouse_at_x = e.point.x
      mouse_at_y = e.point.y
      handle_ev(EV_Mouse_Moved(
        screen_to_map_x(mouse_at_x), screen_to_map_y(mouse_at_y)
      ))
    }

    case e: MousePressed => {
      grab_focus
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
      repaint

      // reset for the next event
      click_x = e.point.x
      click_y = e.point.y

      mouse_at_x = e.point.x
      mouse_at_y = e.point.y
      handle_ev(EV_Mouse_Moved(
        screen_to_map_x(mouse_at_x), screen_to_map_y(mouse_at_y)
      ))
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

      fix_oob
      // show the zoom
      status.zoom.text = "%.1f".format(zoom)
      repaint
    }

    // on my system, this is fired constantly at a repeat rate, rather than
    // having the usual semantics of one press/release pair.
    case KeyPressed(_, key, _, _) => {
      key match {
        case Key.Up    => y_off -= zoom * key_speed
        case Key.Down  => y_off += zoom * key_speed
        case Key.Left  => x_off -= zoom * key_speed
        case Key.Right => x_off += zoom * key_speed
        case Key.R     => reset_window
        case _         => handle_ev(EV_Key_Press(key))
      }

      fix_oob
      repaint
    }
  }

  protected def grab_focus() = {
    if (!hasFocus) {
      requestFocus
    }
  }

  // begin in the center
  private def reset_window() = {
    x_off = canvas_width / 2
    y_off = canvas_height / 2
    zoom = 1.0
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

  def center_on(pt: Coordinate) = {
    x_off = (pt.x * zoom) - (size.width / 2)
    y_off = (pt.y * zoom) - (size.height / 2)
  }

  // TODO swing SuperMixin has a window focus listener...
  var first_focus = true

  override def paintComponent(g2d: Graphics2D) = {
    // clear things
    super.paintComponent(g2d)

    if (first_focus) {
      grab_focus
      first_focus = false
    }

    val orig_transform = g2d.getTransform

    // Perform the transformations to mimic scrolling and zooming
    g2d.translate(-x_off, -y_off)
    g2d.scale(zoom, zoom)

    // TODO antialias cfg
    // ew, clunky old java crap.
    val antialiasing = new java.util.HashMap[Any,Any]()
    antialiasing.put(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )
    g2d.setRenderingHints(antialiasing)

    // provide a tool-tip rendering service to the client.
    render_canvas(g2d, viewing_window) match {
      case Some(text) => {
        // TODO only if you want tooltips...
        g2d.setTransform(orig_transform)
        val width = g2d.getFontMetrics.getStringBounds(text, g2d).getWidth
        val height = g2d.getFontMetrics.getStringBounds(text, g2d).getHeight
        // center the text
        val x = mouse_at_x - (width / 2.0)
        // don't draw right on top of the cursor
        val y = mouse_at_y - 10.0

        // draw a nice backdrop
        g2d.setColor(Color.WHITE)
        g2d.fill(new RoundRectangle2D.Double(
          x, y - height - 2.0, width, height + 4.0, 1.0, 1.0
        ))
        g2d.setColor(Color.BLACK)
        // TODO tweak font size, colors.
        g2d.drawString(text, x.toFloat, y.toFloat)
      }
      case None =>
    }
  }

  // what logical coordinates are in view?
  def x1 = screen_to_map_x(0)
  def y1 = screen_to_map_y(0)
  def x2 = screen_to_map_x(size.width)
  def y2 = screen_to_map_y(size.height)
  def viewing_window: Rectangle2D.Double =
    new Rectangle2D.Double(x1, y1, x2 - x1, y2 - y1)

  // TODO ew, even refactored, these are a bit ugly.
  def prompt_int(msg: String): Option[Int]
    = Dialog.showInput(message = msg, initial = "") match
  { 
    case Some(num) => {
      try {
        Some(num.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    }
    case _ => None
  }
  
  def prompt_double(msg: String): Option[Double]
    = Dialog.showInput(message = msg, initial = "") match                   
  {                                                                         
    case Some(num) => {
      try {
        Some(num.toDouble)
      } catch {
        case _: NumberFormatException => None
      }
    }
    case _ => None
  }

  // implement these. render_canvas returns tooltip text desired
  def render_canvas(g2d: Graphics2D, window: Rectangle2D.Double): Option[String]
  def canvas_width: Int
  def canvas_height: Int
  def handle_ev(ev: UI_Event)
}

// TODO is this the right way to do this?
// TODO hey, why not use the existing swingish publisher/reactions framework? :P
sealed trait UI_Event {}
final case class EV_Mouse_Moved(x: Double, y: Double) extends UI_Event {}
// TODO type erasure issue here; this should really be Option[Any]
final case class EV_Param_Set(key: String, value: Option[String])
  extends UI_Event {}
// TODO this is really a swing.event.Key (a Enumeration.Value), but I can't get
// that to work...
final case class EV_Key_Press(key: Any) extends UI_Event {}
final case class EV_Action(key: String) extends UI_Event {}
final case class EV_Select_Polygon_For_Army() extends UI_Event {}
final case class EV_Select_Polygon_For_Policy() extends UI_Event {}

trait Renderable {
  def debug(): Unit

  // TODO someday, right-click context menus!
}
