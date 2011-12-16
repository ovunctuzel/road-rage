package ui

import scala.collection.mutable.ListBuffer
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import java.awt.geom._

import map._  // TODO yeah getting lazy.

// TODO maybe adaptor class for our map geometry? stick in a 'render road' bit

class MapCanvas(g: Graph) extends ScrollingCanvas {
  def canvas_width = g.width.toInt
  def canvas_height = g.height.toInt

  // pre-render base roads.
  // TODO this was too ridiculous a sequence comprehension, so use a ListBuffer,
  // which is supposed to be O(1) append and conversion to list.
  println("Pre-rendering road geometry...")
  val bg_lines = build_bg_lines

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
                   
  // pre-render lanes
  val fg_lines = for (e <- g.edges; l <- e.lines)
                 yield new EdgeLine(l, e)

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
  def render_canvas(g2d: Graphics2D, x1: Double, y1: Double,
                    x2: Double, y2: Double, zoom: Double) =
  {
    // a window of our logical bounds
    val window = new Rectangle2D.Double(x1, y1, x2 - x1, y2 - y1)

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

  // TODO callback mechanism of some sort.
}

sealed trait ScreenLine {
  val line: Line2D.Double
}
final case class RoadLine(a: Coordinate, b: Coordinate, road: Road) extends ScreenLine {
  val line = new Line2D.Double(a.x, a.y, b.x, b.y)
}
final case class EdgeLine(l: Line, edge: Edge) extends ScreenLine {
  val line = new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  val arrow = GeomFactory.draw_arrow(l, 3)  // TODO 3? THREE?! さん！？
}

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
