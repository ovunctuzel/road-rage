package utexas.ui

import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Set => MutableSet}
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import java.awt.geom._
import swing.event.Key

import utexas.map._  // TODO yeah getting lazy.
import utexas.sim.{Simulation, Queue_of_Agents, LanePosition, VoidPosition,
                   Agent, Simulation_Listener}

import utexas.cfg
import utexas.Util.{log, log_push, log_pop}

// TODO maybe adaptor class for our map geometry? stick in a 'render road' bit

class MapCanvas(sim: Simulation) extends ScrollingCanvas {
  // listen!
  sim.listeners += new Simulation_Listener() {
    def ev_step() = {
      handle_ev(EV_Action("step"))
    }
  }

  // state
  private var current_edge: Option[Edge] = None
  private var highlight_type: Option[String] = None
  private var current_turn = -1  // for cycling through turns from an edge

  // this is like static config, except it's a function of the map and rng seed
  private val special_ward_color = Color.BLACK
  private val ward_colors = List(
    Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW, Color.CYAN, Color.ORANGE,
    Color.PINK, Color.MAGENTA
  ) // TODO cfg
  // TODO do this better by shuffling once and then having a lazy cyclic
  // infinite list
  private val ward_colorings = sim.wards.map(
    w => (w, scala.util.Random.shuffle(ward_colors).head)
  ).toMap ++ List((sim.special_ward, special_ward_color)).toMap

  def canvas_width = sim.width.toInt
  def canvas_height = sim.height.toInt

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
    for (r <- sim.roads; (from, to) <- r.pairs_of_points) {
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
    for (e <- sim.edges) {
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

  // TODO cfg
  /*private val center_stroke = new BasicStroke(
    0.1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array(1.0f), 0.0f
  )*/
  private val center_stroke = new BasicStroke(0.1f)
  private val lane_stroke = new BasicStroke(0.05f)
  private val lane_width = 0.6f

  // pre-compute; we don't have more than max_lanes
  private val strokes = (0 until cfg.max_lanes).map(n => new BasicStroke(lane_width * n.toFloat))

  // TODO colors for everything belong in cfg.

  // Coordinates passed in are logical/map
  def render_canvas(g2d: Graphics2D) = {
    // a window of our logical bounds
    val window = viewing_window

    // remember these so we can draw center lines more efficiently
    val roads_seen = new ListBuffer[RoadLine]

    // Draw the first layer (roads)
    for (l <- bg_lines if l.line.intersects(window)) {
      g2d.setColor(color_road(l.road))
      g2d.setStroke(strokes(l.road.num_lanes))
      g2d.draw(l.line)
      roads_seen += l
    }

    // don't show tiny details when it doesn't matter (and when it's expensive
    // to render them all)
    if (zoom > cfg.zoom_threshold) {
      // then the second layer (lanes)
      g2d.setStroke(lane_stroke)
      // TODO if it's an edge of a road whose line has been seen?
      for (l <- fg_lines if l.line.intersects(window)) {
        draw_edge(g2d, l)
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
    } else {
      // Draw agents anyway?
      sim.agents.foreach(a => draw_agent(g2d, a))
    }
  }

  def draw_edge(g2d: Graphics2D, l: EdgeLine) = {
    g2d.setColor(color_edge(l.edge))
    g2d.draw(l.line)
    g2d.setColor(Color.BLUE)
    g2d.fill(l.arrow)

    // and any agents
    l.edge.agents.foreach(a => draw_agent(g2d, a))
  }

  def draw_agent(g2d: Graphics2D, a: Agent) = {
    g2d.setColor(Color.RED) // TODO depending on their state, later
    // TODO how can we encode the invariant that edge.agents only contains
    // agents with at=LanePosition?
    a.at match {
      case LanePosition(e, dist) => {
        // TODO again, i think we have the assumption that they'll validly be
        // on this edge.
        val loc = e.location(dist)
        loc match {
          case Some(pt) => g2d.fill(new Ellipse2D.Double(pt.x - eps, pt.y - eps, eps * 2, eps * 2))
          case None     => {}
        }
      }
      case VoidPosition() => {}
    }
  }

  def color_road(r: Road): Color = {
    // Test wards
    return ward_colorings(sim.ward(r))

    // Test parallel/perpendicular roads.
    /*current_edge match {
      case Some(e) => {
        //val special = e.to.parallel_roads(e.road)
        val special = e.to.perp_roads(e.road)
        if (special(r)) {
          return Color.CYAN
        }
      }
      case _ => {}
    }*/

    // The normal handling
    return highlight_type match
    {
      case (Some(x)) if x == r.road_type => Color.GREEN
      case _                             => Color.BLACK
    }
  }

  def color_edge(e: Edge): Color = {
    // Test counter-clockwise ordering
    /*current_edge match {
      case Some(cur) => {
        cur.next_counterclockwise_to match {
          case (Some(ccw)) if e == ccw => return Color.CYAN
          case _ => {}
        }
      }
      case _ => {}
    }*/

    return Color.WHITE
  }

  val turn_colors = Map( // cfg
    TurnType.CROSS       -> Color.WHITE,
    TurnType.CROSS_MERGE -> Color.RED,   // TODO i want to notice it!
    TurnType.LEFT        -> Color.ORANGE,
    TurnType.RIGHT       -> Color.GREEN,
    TurnType.UTURN       -> Color.MAGENTA
  )

  def draw_turn(g2d: Graphics2D, turn: Turn, color: Color) = {
    val pt1 = turn.from.end_pt
    val pt2 = turn.from.to.location
    val pt3 = turn.to.start_pt
    val curve = new CubicCurve2D.Double(
      pt1.x, pt1.y, pt2.x, pt2.y, pt2.x, pt2.y, pt3.x, pt3.y
    )
    g2d.setColor(color)
    // draw the directed turn
    g2d.draw(curve)
    g2d.fill(
      GeomFactory.draw_arrow(new Line(pt2.x, pt2.y, pt3.x, pt3.y), 3)
    )
  }

  def draw_intersection(g2d: Graphics2D, e: Edge) = {
    if (current_turn == -1) {
      // show all turns
      for (turn <- e.next_turns) {
        draw_turn(g2d, turn, turn_colors(turn.turn_type))
      }
    } else {
      // show one turn and its conflicts
      val turn = e.next_turns(current_turn)
      draw_turn(g2d, turn, turn_colors(turn.turn_type))
      // TODO this looks buggy!
      for (conflict <- turn.conflicts) {
        draw_turn(g2d, conflict, Color.RED)
      }
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
        // always reset this
        current_turn = -1

        // Are we mouse-overing something? ("mousing over"?)
        if (zoom > cfg.zoom_threshold) {
          current_edge = mouseover_edge(x, y) match {
            case Some(l) => Some(l.edge)
            case None    => None
          }
          status.location.text = current_edge match {
            case Some(e) => "" + e
            case None    => "Nowhere"
          }
        }
        // TODO always?
        repaint
      }
      case EV_Param_Set("highlight", value) => {
        highlight_type = value
        repaint
      }
      case EV_Key_Press(key) => {
        key match {
          // TODO this'll be tab someday, i vow!
          case Key.Control => {
            // cycle through turns
            current_edge match {
              case Some(e) => {
                current_turn += 1
                if (current_turn >= e.next_turns.size) {
                  current_turn = 0
                }
                repaint
              }
              case None => {}
            }
          }
          case Key.P => {
            handle_ev(EV_Action("toggle-running"))
          }
          case _ => {}
        }
      }
      case EV_Action("spawn-army") => {
        // TODO cfg
        for (_ <- 0 until 10) {
          sim.add_agent(sim.random_edge_except(Set()))
        }
        status.agents.text = "" + sim.agents.length
        repaint
      }
      case EV_Action("step") => {
        status.time.text = "%.1f".format(sim.tick)
        // agents have maybe moved, so...
        repaint
      }
      case EV_Action("toggle-running") => {
        if (sim.running) {
          sim.pause
          status.time.text = "%.1f [Paused]".format(sim.tick)
        } else {
          sim.resume
        }
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
final case class EdgeLine(l: Line, edge: Edge with Queue_of_Agents) extends ScreenLine {
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
