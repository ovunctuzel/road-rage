package utexas.ui

import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Set => MutableSet}
import java.awt.{Graphics2D, Shape, BasicStroke, Color}
import java.awt.geom._
import swing.event.Key
import swing.Dialog

import utexas.map._  // TODO yeah getting lazy.
import utexas.sim.{Simulation, Agent, Simulation_Listener}

import utexas.cfg
import utexas.Util

object Mode extends Enumeration {
  type Mode = Value
  val EXPLORE = Value("Explore")  // just chilling
  val PICK_1st = Value("Pick 1st edge") // for now, only client is pathfinding
  val PICK_2nd = Value("Pick 2nd edge")
}

// TODO maybe adaptor class for our map geometry? stick in a 'render road' bit

class MapCanvas(sim: Simulation) extends ScrollingCanvas {
  // listen!
  sim.listeners += new Simulation_Listener() {
    def ev_step() = handle_ev(EV_Action("step"))
  }

  // start the simulation
  sim.start_timer

  // state
  private var current_edge: Option[Edge] = None
  private var highlight_type: Option[String] = None
  private var show_ward_colors = false
  private var current_turn = -1  // for cycling through turns from an edge
  private var show_wards = false
  private var current_ward: Option[Ward] = None
  private var mode = Mode.EXPLORE
  private var chosen_edge1: Option[Edge] = None
  private var chosen_edge2: Option[Edge] = None
  private var route_members = Set[Edge]()
  private var current_agent: Option[Agent] = None
  private var polygon_roads: Set[Road] = Set()

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
  private val agent_colors = HashMap[Agent, Color]()

  def canvas_width = sim.width.toInt
  def canvas_height = sim.height.toInt

  // pre-render base roads.
  // TODO this was too ridiculous a sequence comprehension, so use a ListBuffer,
  // which is supposed to be O(1) append and conversion to list.
  Util.log("Pre-rendering road geometry...")
  Util.log_push

  val road2lines = new HashMap[Road, MutableSet[RoadLine]] with MultiMap[Road, RoadLine]
  Util.log("Road lines...")
  val bg_lines = build_bg_lines

  // this is only used for finer granularity searching...
  val edge2lines = new HashMap[Edge, MutableSet[EdgeLine]] with MultiMap[Edge, EdgeLine]
  // pre-render lanes
  Util.log("Edge lines...")
  val fg_lines = build_fg_lines

  // pre-render ward bubbles
  Util.log("Ward bubbles...")
  val ward_bubbles = sim.wards.map(new WardBubble(_))
  Util.log_pop

  // just used during construction.
  // Note ListBuffer takes O(n) to access the last element, so only write to it,
  // don't read from it.
  private def build_bg_lines(): List[RoadLine] = {
    val list = new ListBuffer[RoadLine]()
    for (r <- sim.roads; (from, to) <- r.pairs_of_points) {
      val line = new RoadLine(from, to, r)
      road2lines.addBinding(r, line)
      list += line
    }
    return list.toList
  }
  private def build_fg_lines(): List[EdgeLine] = {
    val list = new ListBuffer[EdgeLine]()
    for (e <- sim.edges; l <- e.lines) {
      val line = new EdgeLine(l, e)
      edge2lines.addBinding(e, line)
      list += line
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
  private val center_stroke     = new BasicStroke(0.1f)
  private val route_lane_stroke = new BasicStroke(0.45f)
  private val lane_stroke       = new BasicStroke(0.05f)
  private val lane_width = 0.6f
  // TODO make this look cooler.
  private val drawing_stroke = new BasicStroke(
    5.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array(1.0f), 0.0f
  )

  // pre-compute; we don't have more than max_lanes
  private val strokes = (0 until cfg.max_lanes).map(n => new BasicStroke(lane_width * n.toFloat))

  // TODO colors for everything belong in cfg.

  def render_canvas(g2d: Graphics2D) = {
    // a window of our logical bounds
    val window = viewing_window

    // remember these so we can draw center lines more efficiently
    val roads_seen = new ListBuffer[RoadLine]

    // Draw the first layer (roads) - either all or just major ones
    for (l <- bg_lines if (l.line.intersects(window) && (!show_wards || sim.special_ward.roads(l.road))))
    {
      draw_road(g2d, l)
      roads_seen += l
    }

    // Draw wards?
    if (show_wards) {
      for (w <- ward_bubbles if w.bubble.intersects(window)) {
        roads_seen ++= draw_ward(g2d, w)
      }
    }

    // don't show tiny details when it doesn't matter (and when it's expensive
    // to render them all)
    if (zoom > cfg.zoom_threshold) {
      // then the second layer (lanes)
      // TODO this is ugly and maybe inefficient?
      //for (l <- fg_lines if l.line.intersects(window))
      for (r <- roads_seen; l <- r.road.all_lanes.flatMap(edge2lines(_)) if l.line.intersects(window))
      {
        draw_edge(g2d, l)
      }

      // and the third layer (dashed center lines)
      g2d.setColor(Color.YELLOW)
      g2d.setStroke(center_stroke)
      for (l <- roads_seen if !l.road.is_oneway) {
        g2d.draw(l.line)
      }

      // draw turns?
      current_edge match {
        case Some(e) => draw_intersection(g2d, e)
        case None    => {}
      }
    }

    // When an agent is doing a turn, it's not any edge's agent queue. Because
    // of that and because they're seemingly so cheap to draw anyway, just
    // always...
    sim.agents.foreach(a => draw_agent(g2d, a))

    // Finally, if the user is free-handing a region, show their work.
    g2d.setColor(Color.RED)
    g2d.setStroke(drawing_stroke)
    g2d.draw(polygon)
  }

  // we return any new roads seen
  def draw_ward(g2d: Graphics2D, w: WardBubble): Set[RoadLine] = {
    current_ward match {
      // Show the roads of the highlighted ward
      case (Some(ward)) if w.ward == ward => {
        val lines = ward.roads.flatMap(road2lines(_))
        lines.foreach(l => draw_road(g2d, l))
        return lines
      }
      case _ => {
        // Show the center of the ward and all external connections
        g2d.setColor(ward_colorings(w.ward))
        g2d.fill(w.bubble)
        g2d.setStroke(strokes(1)) // TODO
        for (v <- w.ward.frontier) {
          g2d.draw(new Line2D.Double(
            w.bubble.getCenterX, w.bubble.getCenterY, v.location.x, v.location.y
          ))
        }
        return Set()
      }
    }
  }

  def draw_road(g2d: Graphics2D, l: RoadLine) = {
    g2d.setColor(color_road(l.road))
    g2d.setStroke(strokes(l.road.num_lanes))
    g2d.draw(l.line)
  }

  def draw_edge(g2d: Graphics2D, l: EdgeLine) = {
    if (route_members(l.edge)) {
      g2d.setStroke(route_lane_stroke)
    } else {
      g2d.setStroke(lane_stroke)
    }
    g2d.setColor(color_edge(l.edge))
    g2d.draw(l.line)
    g2d.setColor(Color.BLUE)
    g2d.fill(l.arrow)

    // (draw all agents)
    //// and any agents
    //sim.queues(l.edge).agents.foreach(a => draw_agent(g2d, a))
  }

  def agent_bubble(a: Agent): Ellipse2D = {
    val loc = a.at.location
    return new Ellipse2D.Double(loc.x - eps, loc.y - eps, eps * 2, eps * 2)
  }

  def draw_agent(g2d: Graphics2D, a: Agent) = {
    if (!agent_colors.contains(a)) {
      agent_colors(a) = GeomFactory.rand_color
    }
    g2d.setColor(agent_colors(a))
    g2d.fill(agent_bubble(a))
  }

  def color_road(r: Road): Color = {
    if (polygon_roads(r)) {
      return Color.YELLOW
    } else if (show_ward_colors) {
      return ward_colorings(r.ward)
    } else {
      // The normal handling
      return highlight_type match
      {
        case (Some(x)) if x == r.road_type => Color.GREEN
        case _                             => Color.BLACK
      }
    }
  }

  def color_edge(e: Edge): Color = {
    // TODO cfg
    if (chosen_edge1.isDefined && chosen_edge1.get == e) {
      return Color.BLUE
    } else if (chosen_edge2.isDefined && chosen_edge2.get == e) {
      return Color.RED
    } else if (route_members(e)) {
      return Color.GREEN
    } else if (e.doomed) {
      // TODO configurable.
      return Color.RED
    } else {
      return Color.WHITE
    }
  }

  val turn_colors = Map( // cfg
    TurnType.CROSS       -> Color.WHITE,
    TurnType.CROSS_MERGE -> Color.RED,   // TODO i want to notice it!
    TurnType.LEFT        -> Color.ORANGE,
    TurnType.RIGHT       -> Color.GREEN,
    TurnType.UTURN       -> Color.MAGENTA
  )

  def draw_turn(g2d: Graphics2D, turn: Turn, color: Color) = {
    val pt1 = turn.from.shifted_end_pt
    val pt2 = turn.from.to.location
    val pt3 = turn.to.shifted_start_pt
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

  def mouseover_edge(x: Double, y: Double): Option[Edge] = {
    val window = viewing_window
    val cursor_bubble = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)
    // do a search at low granularity first
    // TODO does this actually help us?
    /*for (big_line <- bg_lines if big_line.line.intersects(window)) {
      for (e <- big_line.road.all_lanes) {
        for (l <- edge2lines(e) if l.line.intersects(cursor_bubble)) {
          return Some(l.edge)
        }
      }
    }*/

    return fg_lines.find(l => l.line.intersects(cursor_bubble)) match {
      case Some(l) => Some(l.edge)
      case None    => None
    }
  }

  def mouseover_ward(x: Double, y: Double): Option[Ward] = {
    val cursor_bubble = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)
    return ward_bubbles.find(w => w.bubble.intersects(cursor_bubble)) match {
      case Some(b) => Some(b.ward)
      case None    => None
    }
  }

  def mouseover_agent(x: Double, y: Double): Option[Agent] = {
    val cursor_bubble = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)
    return sim.agents.find(a => agent_bubble(a).intersects(cursor_bubble))
  }

  def handle_ev(ev: UI_Event) = {
    ev match {
      case EV_Mouse_Moved(x, y) => {
        // always reset this
        current_turn = -1

        // Are we mouse-overing something? ("mousing over")
        current_agent = mouseover_agent(x, y)

        current_agent match {
          case Some(a) => {
            status.location.text = "" + a
            current_ward = None
            current_edge = None
          }
          case None => {
            if (show_wards) {
              current_ward = mouseover_ward(x, y)
              // TODO fall back to looking for edges if None?
              status.location.text = current_ward match {
                case Some(w) => "" + w
                case None    => "Nowhere"
              }
              current_edge = None
              current_agent = None
            } else if (zoom > cfg.zoom_threshold) {
              current_edge = mouseover_edge(x, y)
              status.location.text = current_edge match {
                case Some(e) => "" + e
                case None    => "Nowhere"
              }
              current_ward = None
              current_agent = None
            } else {
              status.location.text = "Nowhere"
            }
          }
        }

        // TODO only on changes?
        repaint
      }
      case EV_Param_Set("highlight", value) => {
        highlight_type = value
        repaint
      }
      // TODO this'll be tab someday, i vow!
      case EV_Key_Press(Key.Control) => {
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
      case EV_Key_Press(Key.P) => {
        handle_ev(EV_Action("toggle-running"))
      }
      case EV_Key_Press(Key.W) => {
        handle_ev(EV_Action("toggle-wards"))
      }
      case EV_Action("spawn-army") => {
        val num = 100   // TODO cfg
        sim.spawn_army(1, num)
        status.agents.text = "" + sim.agents.size
        repaint
      }
      case EV_Action("step") => {
        status.time.text = "%.1f".format(sim.tick)
        // agents have maybe moved, so...
        status.agents.text = "" + sim.agents.size
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
      case EV_Action("toggle-wards") => {
        show_wards = !show_wards
        // TODO this doesnt quite seem to match until we actually move...
        handle_ev(EV_Mouse_Moved(mouse_at_x, mouse_at_y))
        repaint
      }
      case EV_Action("pathfind") => {
        show_ward_colors = false  // it's really hard to see otherwise
        switch_mode(Mode.PICK_1st)
        chosen_edge1 = None
        chosen_edge2 = None
        route_members = Set[Edge]()
        repaint
      }
      case EV_Action("clear-route") => {
        switch_mode(Mode.EXPLORE)
        chosen_edge1 = None
        chosen_edge2 = None
        route_members = Set[Edge]()
        repaint
      }
      case EV_Action("teleport") => {
        Dialog.showInput(
          message = "What edge ID do you seek?", initial = ""
        ) match {
          case Some(id) => {
            try {
              val e = sim.edges(id.toInt)
              // TODO center on some part of the edge and zoom in, rather than just
              // vaguely moving that way
              Util.log("Here's " + e)
              x_off = e.lines.head.x1 * zoom
              y_off = e.lines.head.y1 * zoom
              chosen_edge2 = Some(e)  // just kind of use this to highlight it
              repaint
            } catch {
              case _ => Util.log("Bad edge ID " + id)
            }
          }
          case _ =>
        }
      }
      case EV_Key_Press(Key.C) if current_edge.isDefined => {
        mode match {
          case Mode.PICK_1st => {
            chosen_edge1 = current_edge
            switch_mode(Mode.PICK_2nd)
            repaint
          }
          case Mode.PICK_2nd => {
            chosen_edge2 = current_edge
            // TODO later, let this inform any client
            show_pathfinding
            switch_mode(Mode.EXPLORE)
          }
          case _ =>
        }
      }
      case EV_Key_Press(Key.T) => {
        // toggle road-coloring mode
        show_ward_colors = !show_ward_colors
        repaint
      }
      case EV_Key_Press(Key.OpenBracket) => {
        sim.slow_down
        status.time_speed.text = "%.1fx".format(sim.time_speed)
      }
      case EV_Key_Press(Key.CloseBracket) => {
        sim.speed_up
        status.time_speed.text = "%.1fx".format(sim.time_speed)
      }
      case EV_Key_Press(Key.A) if current_agent.isDefined => {
        current_agent.get.dump_info
      }
      case EV_Key_Press(Key.D) if current_edge.isDefined => {
        val r = current_edge.get.road
        Util.log(r + " is a " + r.road_type + " of length " +
                 current_edge.get.length + " meters")
      }
      case EV_Select_Polygon() => {
        // Let's find all vertices inside the polygon.
        polygon_roads = sim.vertices.filter(v => polygon.contains(v.location.x, v.location.y)).flatMap(v => v.roads).toSet
        Util.log("Matched " + polygon_roads.size + " roads")
      }
      case EV_Key_Press(_) => // Ignore the rest
    }
  }

  def switch_mode(m: Mode.Mode) = {
    mode = m
    status.mode.text = "" + mode
  }

  def show_pathfinding() = {
    // contract: must be called when current_edge1 and 2 are set
    val r = sim.pathfind_astar(chosen_edge1.get, chosen_edge2.get)
    // Filter and just remember the edges; the UI doesn't want to highlight
    // turns.
    route_members = r.map(s => s match {
      case e: Edge => Some(e)
      case _       => None
    }).flatten.toSet
    repaint
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
  val arrow = GeomFactory.draw_arrow(l, 1)  // TODO 3? THREE?!
}
// and, separately...
class WardBubble(val ward: Ward) {
  private val r = 2.0 + (0.1 * ward.roads.size) // TODO cfg
  val bubble = new Ellipse2D.Double(ward.center.x - r, ward.center.y - r, r * 2, r * 2)
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

  def rand_color() = new Color(
    Util.rand_double(0.0, 1.0).toFloat,
    Util.rand_double(0.0, 1.0).toFloat,
    Util.rand_double(0.0, 1.0).toFloat
  )
}
