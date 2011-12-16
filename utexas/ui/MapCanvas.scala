package utexas.ui

import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Set => MutableSet}
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import java.awt.geom._

import utexas.map._  // TODO yeah getting lazy.

import utexas.Util.{log, log_push, log_pop}

// TODO maybe adaptor class for our map geometry? stick in a 'render road' bit

class MapCanvas(g: Graph) extends ScrollingCanvas {
  // state
  private var current_edge: Option[Edge] = None


  def canvas_width = g.width.toInt
  def canvas_height = g.height.toInt

  // pre-render base roads.
  // TODO this was too ridiculous a sequence comprehension, so use a ListBuffer,
  // which is supposed to be O(1) append and conversion to list.
  log("Pre-rendering road geometry...")
  val bg_lines = build_bg_lines

  // this is only used for finer granularity searching...
  val edge2lines = new HashMap[Edge, MutableSet[EdgeLine]] with MultiMap[Edge, EdgeLine]
  // pre-render lanes
  val fg_lines = build_fg_lines

  // just used during construction.
  private def build_bg_lines(): List[RoadLine] = {
    val list = new ListBuffer[RoadLine]()
    for (r <- g.roads; (from, to) <- r.pairs_of_points) {
      if (r.is_oneway) {
        // TODO this still looks uber sucky.
        val l = r.oneway_lanes.head.lines.last
        // shift the road over a half-lane?
        list += new RoadLine(
          new Coordinate((from.x + l.x1) / 2, (from.y + l.y1) / 2),
          new Coordinate((to.x + l.x2) / 2, (to.y + l.y2) / 2),
          r
        )
      } else {
        list += new RoadLine(from, to, r)
      }
    }
    return list.toList
  }
  private def build_fg_lines(): List[EdgeLine] = {
    val list = new ListBuffer[EdgeLine]()
    for (e <- g.edges) {
      for (l <- e.lines) {
        val line = new EdgeLine(l, e)
        edge2lines.addBinding(e, line)
        list += line
      }
    }
    return list.toList
  }

  // begin in the center
  x_off = canvas_width / 2
  y_off = canvas_height / 2

  // TODO all of this is config.
  private val center_stroke = new BasicStroke(
    0.1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array(1.0f), 0.0f
  )
  private val lane_stroke = new BasicStroke(0.05f)
  private val picked_lane_stroke = new BasicStroke(0.15f)
  private val lane_width = 0.6f
  private val max_lanes = 10
  private val zoom_threshold = 5.0

  // pre-compute; we don't have more than max_lanes
  private val strokes = (0 until max_lanes).map(n => new BasicStroke(lane_width * n.toFloat))

  // Coordinates passed in are logical/map
  def render_canvas(g2d: Graphics2D) = {
    // a window of our logical bounds
    val window = viewing_window

    // remember these so we can draw center lines more efficiently
    val roads_seen = new ListBuffer[RoadLine]

    // Draw the first layer (roads)
    g2d.setColor(Color.BLACK)
    for (l <- bg_lines if l.line.intersects(window)) {
      g2d.setStroke(strokes(l.road.num_lanes))
      g2d.draw(l.line)
      roads_seen += l
    }

    // don't show tiny details when it doesn't matter (and when it's expensive
    // to render them all)
    if (zoom > zoom_threshold) {
      // then the second layer (lanes)
      g2d.setColor(Color.BLUE)
      g2d.setStroke(lane_stroke)
      // TODO if it's an edge of a road whose line has been seen?
      for (l <- fg_lines if l.line.intersects(window)) {
        g2d.draw(l.line)
        g2d.fill(l.arrow)
      }

      // and the third layer (dashed center lines)
      g2d.setColor(Color.YELLOW)
      g2d.setStroke(center_stroke)
      for (l <- roads_seen) {
        g2d.draw(l.line)
      }

      // draw turns?
      current_edge match {
        case Some(e) => draw_intersection(g2d, e)
        case None    => {}
      }

      // TODO agents, cursor, turns
    }

    // and the map boundary
    g2d.setColor(Color.GREEN)
    g2d.setStroke(lane_stroke)
    // TODO these coords are wrong!
    g2d.draw(new Line2D.Double(x1, y1, x2, y1))
    g2d.draw(new Line2D.Double(x1, y2, x2, y2))
    g2d.draw(new Line2D.Double(x1, y1, x1, y2))
    g2d.draw(new Line2D.Double(x2, y1, x2, y2))
  }

  val turn_colors = Map( // cfg
    TurnType.CROSS       -> Color.WHITE,
    TurnType.CROSS_MERGE -> Color.RED,   // TODO i want to notice it!
    TurnType.LEFT        -> Color.ORANGE,
    TurnType.RIGHT       -> Color.GREEN,
    TurnType.UTURN       -> Color.MAGENTA
  )

  def draw_intersection(g2d: Graphics2D, e: Edge) = {
    def draw_turn(turn: Turn) = {
      val pt1 = e.end_pt
      val pt2 = e.to.location
      val pt3 = turn.to.start_pt
      val curve = new CubicCurve2D.Double(
        pt1.x, pt1.y, pt2.x, pt2.y, pt2.x, pt2.y, pt3.x, pt3.y
      )
      g2d.setColor(turn_colors(turn.turn_type))
      // draw the directed turn
      g2d.draw(curve)
      g2d.fill(
        GeomFactory.draw_arrow(new Line(pt2.x, pt2.y, pt3.x, pt3.y), 3)
      )
    }

    // TODO cycling through and showing conflicts
    for (turn <- e.next_turns) {
      draw_turn(turn)
    }
  }

  // the radius of a small epsilon circle for the cursor
  def eps = 5.0 / zoom

  def mouseover_edge(x: Double, y: Double): Option[EdgeLine] = {
    val window = viewing_window
    val cursor_bubble = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)
    // do a search at low granularity first
    for (big_line <- bg_lines if big_line.line.intersects(window)) {
      for (e <- big_line.road.all_lanes) {
        for (l <- edge2lines(e) if l.line.intersects(cursor_bubble)) {
          return Some(l)
        }
      }
    }
    // no hit
    return None
  }

  def handle_ev(ev: UI_Event) = {
    ev match {
      case EV_Mouse_Moved(x, y) => {
        // Are we mouse-overing something? ("mousing over"?)
        if (zoom > zoom_threshold) {
          current_edge = mouseover_edge(x, y) match {
            case Some(l) => Some(l.edge)
            case None    => None
          }
        }
        // TODO always?
        repaint
      }
    }
  }
}

sealed trait ScreenLine {
  val line: Line2D.Double
}
final case class RoadLine(a: Coordinate, b: Coordinate, road: Road) extends ScreenLine {
  val line = new Line2D.Double(a.x, a.y, b.x, b.y)
}
final case class EdgeLine(l: Line, edge: Edge) extends ScreenLine {
  val line = new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  val arrow = GeomFactory.draw_arrow(l, 1)  // TODO 3? THREE?! さん！？
}

// TODO what else belongs?
object GeomFactory {
  def draw_arrow(line: Line, size: Int): Shape = {
    // TODO enum for size
    // width = how far out is the tip
    val width = size match {
      case 3 => 0.25
      case 2 => 0.15
      case _ => 0.1
    }
    // height = how tall is the arrow
    val height = size match {
      case 3 => 1.0
      case 2 => 0.75
      case _ => 0.5
    }

    val mid = line.midpt
    val theta = line.broken_angle
    val x = mid.x + (height * math.cos(theta))
    val y = mid.y + (height * math.sin(theta))

    // Perpendiculous!
    val theta_perp1 = theta + (math.Pi / 2)
    val cos_perp1 = width * math.cos(theta_perp1)
    val sin_perp1 = width * math.sin(theta_perp1)

    val theta_perp2 = theta - (math.Pi / 2)
    val cos_perp2 = width * math.cos(theta_perp2)
    val sin_perp2 = width * math.sin(theta_perp2)

    val arrow = new Path2D.Double()
    arrow.moveTo(x, y)
    arrow.lineTo(mid.x + cos_perp1, mid.y + sin_perp1)
    arrow.lineTo(mid.x + cos_perp2, mid.y + sin_perp2)
    return arrow
  }
}
