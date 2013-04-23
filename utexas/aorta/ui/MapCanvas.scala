// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import java.awt.{Graphics2D, Shape, BasicStroke, Color, Polygon}
import java.awt.geom._
import swing.event.Key
import swing.Dialog
import scala.language.implicitConversions
import java.io.File

import utexas.aorta.map._  // TODO yeah getting lazy.
import utexas.aorta.map.analysis.CongestionRouter
import utexas.aorta.sim.{Simulation, Agent, Sim_Event, EV_Signal_Change,
                         IntersectionType, RouteType, Route_Event,
                         EV_Transition, EV_Reroute, EV_Heartbeat}
import utexas.aorta.sim.PathRoute

import utexas.aorta.{Util, RNG, Common, cfg}

object Mode extends Enumeration {
  type Mode = Value
  val EXPLORE = Value("Explore")  // just chilling
  val PICK_1st = Value("Pick 1st edge") // for now, only client is pathfinding
  val PICK_2nd = Value("Pick 2nd edge")
}

class MapCanvas(sim: Simulation, headless: Boolean = false) extends ScrollingCanvas {
  // Headless mode might be controlling us...
  if (!headless) {
    // fire steps every now and then
    new Thread {
      override def run(): Unit = {
        while (true) {
          val start = System.currentTimeMillis
          // we should fire about 10x/second. optimal/useful rate is going to be
          // related to desired_sim_speed and cfg.dt_s   TODO
          Thread.sleep(10)
          if (running) {
            sim.step((System.currentTimeMillis - start).toDouble / 1000.0)
            camera_agent match {
              case Some(a) => {
                if (sim.has_agent(a)) {
                  center_on(a.at.location)
                } else {
                  Util.log(a + " is done; the camera won't stalk them anymore")
                  camera_agent = None
                  route_members = Set[Road]()
                }
              }
              case None =>
            }
            // always render
            handle_ev(EV_Action("step"))
          }
        }
      }
    }.start
  }

  sim.listen("statusbar", (ev: Sim_Event) => { ev match {
    case EV_Heartbeat(info) => {
      status.agents.text = info.describe
      status.time.text = Util.time_num(info.tick)
    }
    case _ =>
  }})

  // but we can also pause
  var running = false

  // state
  private var current_obj: Option[Renderable] = None
  private var highlight_type: Option[String] = None
  private var current_turn = -1  // for cycling through turns from an edge
  private var mode = Mode.EXPLORE
  private var chosen_edge1: Option[Edge] = None
  private var chosen_edge2: Option[Edge] = None
  private var chosen_road: Option[Road] = None
  private var chosen_pos: Option[Position] = None
  private var route_members = Set[Road]()
  private var polygon_roads1: Set[Road] = Set()
  private var polygon_roads2: Set[Road] = Set()
  private var camera_agent: Option[Agent] = None
  private val green_turns = new HashMap[Turn, Shape]()
  private var show_green = false
  private var show_tooltips = true
  private val policy_colors = Map(
    IntersectionType.StopSign -> Color.RED,
    IntersectionType.Signal -> Color.YELLOW,
    IntersectionType.Reservation -> Color.GREEN,
    IntersectionType.CommonCase -> Color.BLUE
  )

  implicit def line2awt(l: Line): Line2D.Double = new Line2D.Double(l.x1, l.y1, l.x2, l.y2)

  def current_edge: Option[Edge] = current_obj match {
    case Some(pos: Position) => Some(pos.on.asInstanceOf[Edge])
    case _ => None
  }
  def current_agent: Option[Agent] = current_obj match {
    case Some(a: Agent) => Some(a)
    case _ => None
  }

  private val agent_colors = HashMap[Agent, Color]()

  def canvas_width = sim.graph.width.toInt
  def canvas_height = sim.graph.height.toInt

  // pre-render base roads.
  // TODO this was too ridiculous a sequence comprehension, so use a ListBuffer,
  // which is supposed to be O(1) append and conversion to list.
  Util.log("Pre-rendering road geometry...")

  val road2lines = new HashMap[Road, MutableSet[RoadLine]]
    with MultiMap[Road, RoadLine]
  val bg_lines = build_bg_lines

  // this is only used for finer granularity searching...
  val edge2lines = new HashMap[Edge, MutableSet[EdgeLine]]
    with MultiMap[Edge, EdgeLine]
  // pre-render lanes
  val fg_lines = build_fg_lines

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
      // draw the lines on the borders of lanes, not in the middle
      val line = new EdgeLine(l.perp_shift(0.5), e)
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
  // TODO make this look cooler.
  private val drawing_stroke = new BasicStroke(
    5.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, Array(1.0f), 0.0f
  )

  // pre-compute; we don't have more than max_lanes
  private val lane_line_width = 0.6f  // TODO cfg
  private val strokes = (0 until cfg.max_lanes).map(
    n => new BasicStroke(lane_line_width * n.toFloat)
  )

  def zoomed_in = zoom > cfg.zoom_threshold

  // Register to hear events
  sim.listen("UI", (ev: Sim_Event) => { ev match {
    case EV_Signal_Change(greens) => {
      green_turns.clear
      for (t <- greens) {
        green_turns(t) = GeomFactory.turn_geom(t)
      }
    }
    case _ =>
  } })

  // At this point, signal policies have already fired up and sent the first
  // round of greens. We missed it, so compute manually the first time.
  // TODO better solution
  for (v <- sim.vertices) {
    for (t <- v.intersection.policy.current_greens) {
      green_turns(t) = GeomFactory.turn_geom(t)
    }
  }

  // TODO colors for everything belong in cfg.

  def render_canvas(g2d: Graphics2D, window: Rectangle2D.Double): List[Tooltip] = {
    // remember these so we can draw center lines more efficiently
    val roads_seen = new ListBuffer[RoadLine]

    val tooltips = new ListBuffer[Tooltip]()

    // Draw the first layer (roads) - either all or just major ones
    for (l <- bg_lines if l.line.intersects(window)) {
      draw_road(g2d, l)
      roads_seen += l
    }

    // don't show tiny details when it doesn't matter (and when it's expensive
    // to render them all)
    if (zoomed_in) {
      // then the second layer (lanes)
      // TODO this is ugly and maybe inefficient?
      //for (l <- fg_lines if l.line.intersects(window))
      for (r <- roads_seen; l <- r.road.all_lanes.flatMap(edge2lines(_))
           if l.line.intersects(window))
      {
        draw_edge(g2d, l)
      }

      // and the third layer (dashed center lines)
      g2d.setColor(Color.YELLOW)
      g2d.setStroke(center_stroke)
      for (l <- roads_seen) {
        g2d.draw(l.line)
      }

      current_obj match {
        case Some(pos: Position) => draw_intersection(g2d, pos.on.asInstanceOf[Edge])
        case Some(v: Vertex) => {
          for (t <- v.intersection.policy.current_greens) {
            draw_turn(g2d, t, Color.GREEN)
          }
        }
        case _ =>
      }

      // Show traffic signal stuff
      if (show_green) {
        g2d.setStroke(center_stroke)
        g2d.setColor(Color.GREEN)
        /*green_turns.values.foreach(
          t => if (t.intersects(window)) { g2d.draw(t) }
        )*/
        green_turns.foreach(t => if (t._2.intersects(window)) {
          draw_turn(g2d, t._1, Color.GREEN)
        })
      }

      // Illustrate the intersection policies
      for (v <- sim.vertices) {
        val bub = bubble(v.location)
        if (bub.intersects(window)) {
          g2d.setColor(policy_colors(v.intersection.policy.policy_type))
          g2d.draw(bub)
        }
      }
    }

    // When an agent is doing a turn, it's not any edge's agent queue. Because
    // of that and because they're seemingly so cheap to draw anyway, just
    // always...
    sim.agents.foreach(a => {
      // Do a cheap intersection test before potentially expensive rendering
      // work
      if (agent_bubble(a).intersects(window)) {
        draw_agent(g2d, a)
        if (zoomed_in && show_tooltips) {
          tooltips += Tooltip(
            a.at.location.x, a.at.location.y, a.wallet.tooltip,
            a.wallet.dark_tooltip
          )
        }
      }
    })

    // Finally, if the user is free-handing a region, show their work.
    g2d.setColor(Color.RED)
    g2d.setStroke(drawing_stroke)
    g2d.draw(polygon)

    // What tooltips do we want?
    current_obj match {
      case Some(thing) => {
        tooltips += Tooltip(
          screen_to_map_x(mouse_at_x), screen_to_map_y(mouse_at_y),
          thing.tooltip, false
        )
      }
      case None =>
    }
    return tooltips.toList
  }

  def draw_road(g2d: Graphics2D, l: RoadLine) = {
    g2d.setColor(color_road(l.road))
    if (route_members(l.road) && !zoomed_in) {
      g2d.setStroke(strokes(l.road.num_lanes * 2))
    } else {
      g2d.setStroke(strokes(l.road.num_lanes))
    }
    g2d.draw(l.bg_line)
  }

  def draw_edge(g2d: Graphics2D, l: EdgeLine) = {
    g2d.setStroke(lane_stroke)
    g2d.setColor(color_edge(l.edge))
    g2d.draw(l.line)
    g2d.setColor(Color.BLUE)
    g2d.fill(l.arrow)

    // (draw all agents)
    //// and any agents
    //sim.queues(l.edge).agents.foreach(a => draw_agent(g2d, a))
  }

  def draw_agent(g2d: Graphics2D, a: Agent) = {
    g2d.setColor(color_agent(a))
    if (zoomed_in) {
      // TODO cfg. just tweak these by sight.
      val vehicle_length = 0.2  // along the edge
      val vehicle_width = 0.15  // perpendicular

      var (line, front_dist) = a.at.on.current_pos(a.at.dist)
      a.old_lane match {
        case Some(l) => {
          val (line2, more_dist) = l.current_pos(a.at.dist)
          // TODO I'd think 1 - progress should work, but by visual inspection,
          // apparently not.
          val progress = (a.lanechange_dist_left / cfg.lanechange_dist)
          line = line.add_scaled(line2, progress)
        }
        case None =>
      }
      val front_pt = line.point_on(front_dist)

      // the front center of the vehicle is where the location is. ascii
      // diagrams are hard, but line up width-wise
      val rect = new Rectangle2D.Double(
        front_pt.x - vehicle_length, front_pt.y - (vehicle_width / 2),
        vehicle_length, vehicle_width
      )
      // play rotation tricks
      // TODO val g2d_rot = g2d.create
      // rotate about the front center point, aka, keep that point fixed/pivot
      g2d.rotate(-line.angle, front_pt.x, front_pt.y)
      g2d.fill(rect)
      // TODO undoing it this way is dangerous... try to make a new g2d context
      // and dispose of it
      g2d.rotate(line.angle, front_pt.x, front_pt.y)
    } else {
      g2d.fill(agent_bubble(a))
    }
  }

  def color_road(r: Road): Color =
    if (chosen_road.isDefined && chosen_road.get == r)
      Color.RED
    else if (route_members(r))
      Color.GREEN
    else if (polygon_roads1(r))
      Color.RED
    else if (polygon_roads2(r))
      Color.GREEN
    else if (r.doomed)
      Color.RED
    else
      // The normal handling
      highlight_type match {
        case (Some(x)) if x == r.road_type => Color.GREEN
        case _                             => Color.BLACK
      }

  def color_edge(e: Edge): Color =
    // TODO cfg
    if (chosen_edge1.isDefined && chosen_edge1.get == e)
      Color.BLUE
    else if (chosen_edge2.isDefined && chosen_edge2.get == e)
      Color.RED
    else if (e.doomed)
      // TODO configurable.
      Color.RED
    else
      Color.WHITE

  def color_agent(a: Agent): Color = current_obj match {
    case Some(v: Vertex) => a.all_tickets(v.intersection).toList match {
      case Nil => Color.GRAY
      case ls if ls.find(_.is_approved).isDefined => Color.GREEN
      case ls if ls.find(_.is_interruption).isDefined => Color.YELLOW
      case _ => Color.RED
    }
    case _ => camera_agent match {
      case Some(agent) if a == agent => Color.WHITE
      case _ => {
        // try to avoid flashing red, this feature is used to visually spot true
        // clumps
        if (a.how_long_idle >= 30.0) {
          Color.RED
        } else if (!zoomed_in) {
          Color.GRAY
        } else {
          if (!agent_colors.contains(a)) {
            agent_colors(a) = GeomFactory.rand_color
          }
          agent_colors(a)
        }
      }
    }
  }

  def draw_turn(g2d: Graphics2D, turn: Turn, color: Color) = {
    val line = GeomFactory.turn_geom(turn)
    g2d.setColor(color)
    g2d.draw(line)
    g2d.fill(GeomFactory.draw_arrow(line, line.shift_back(0.75), 3))
  }

  def draw_intersection(g2d: Graphics2D, e: Edge) = {
    if (current_turn == -1) {
      // show all turns
      for (turn <- e.next_turns) {
        draw_turn(g2d, turn, Color.GREEN)
      }
    } else {
      // show one turn and its conflicts
      val turn = e.next_turns(current_turn)
      draw_turn(g2d, turn, Color.GREEN)

      for (conflict <- turn.conflicts) {
        draw_turn(g2d, conflict, Color.RED)
      }
    }
  }

  def redo_mouseover(x: Double, y: Double): Unit = {
    current_obj = None
    current_turn = -1

    if (!zoomed_in) {
      return
    }

    // TODO determine if a low-granularity search to narrow down results helps.

    val cursor = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2)

    def hit(shape: Shape) = shape.intersects(cursor)

    // Order of search: agents, vertices, edges, roads
    // TODO ideally, center agent bubble where the vehicle center is drawn.

    // TODO this is _kind_ of ugly.
    current_obj = sim.agents.find(a => hit(agent_bubble(a))) match {
      case None => sim.vertices.find(v => hit(bubble(v.location))) match {
        case None => fg_lines.find(l => hit(l.line)) match {
          case None => bg_lines.find(l => hit(l.line)) match {
            case None => None
            case Some(l) => Some(l.road)
          }
          case Some(l) => Some(Position(l.edge, l.edge.approx_dist(new Coordinate(x, y), 1.0)))
        }
        case Some(v) => Some(v)
      }
      case Some(a) => Some(a)
    }
  }

  // the radius of a small epsilon circle for the cursor
  def eps = 5.0 / zoom
  def bubble(pt: Coordinate) = new Ellipse2D.Double(
    pt.x - eps, pt.y - eps, eps * 2, eps * 2
  )
  def agent_bubble(a: Agent) = bubble(a.at.location)

  def handle_ev(ev: UI_Event): Unit = ev match {
    case EV_Action(action) => handle_ev_action(action)
    case EV_Key_Press(key) => handle_ev_keypress(key)
    case EV_Mouse_Moved(x, y) => {
      redo_mouseover(x, y)
      repaint
    }
    case EV_Param_Set("highlight", value) => {
      highlight_type = value
      repaint
    }
    case EV_Select_Polygon_For_Army() => {
      // TODO continuation style would make this reasonable:
      // 1) dont keep all that ugly state in the object
      //    (but how to clear it?)
      // 2) code reads like a simple flow

      // Let's find all vertices inside the polygon.
      val rds = sim.vertices.filter(
        v => polygon.contains(v.location.x, v.location.y)).flatMap(v => v.roads
      ).toSet
      Util.log("Matched " + rds.size + " roads")
      if (rds.isEmpty) {
        Util.log("Try that again.")
      } else {
        if (polygon_roads1.isEmpty) {
          polygon_roads1 = rds
          Util.log("Now select a second set of roads")
        } else {
          polygon_roads2 = rds

          prompt_generator(
            polygon_roads1.toList.flatMap(_.all_lanes),
            polygon_roads2.toList.flatMap(_.all_lanes)
          )

          // Make the keyboard work again
          grab_focus

          polygon_roads1 = Set()
          polygon_roads2 = Set()
        }
      }
    }
    case EV_Select_Polygon_For_Policy() => {
      // Let's find all vertices inside the polygon.
      val intersections = sim.vertices.filter(
        v => polygon.contains(v.location.x, v.location.y)
      ).map(_.intersection)
      Util.log("Matched " + intersections.size + " intersections")
      Dialog.showInput(
        message = "What policy should govern these intersections?",
        initial = "",
        entries = IntersectionType.values.toList
      ) match {
        case Some(name) => {
          // TODO make a new scenario...
          /*val builder = Simulation.policy_builder(IntersectionType.withName(name.toString))
          intersections.foreach(i => i.policy = builder(i))*/
        }
        case None =>
      }
    }
    case EV_Select_Polygon_For_Serialization() => {
      val dir = s"maps/area_${sim.graph.name}"
      (new File(dir)).mkdir
      Dialog.showInput(message = "Name this area", initial = "") match {
        case Some(name) => {
          val edges = sim.vertices.filter(
            v => polygon.contains(v.location.x, v.location.y)).flatMap(v => v.edges
          ).map(_.id).toArray
          Util.serialize(edges, s"${dir}/${name}")
          Util.log(s"Area saved to ${dir}/${name}")
        }
        case None =>
      }
    }
    case _ =>
  }

  def handle_ev_action(ev: String): Unit = ev match {
    case "spawn-army" => {
      prompt_generator(sim.edges, sim.edges)
    }
    case "step" => {
      val note = if (running)
                   ""
                 else
                   " [Paused]"
      status.update_speed(sim)
      repaint
    }
    case "toggle-running" => {
      if (running) {
        running = false
      } else {
        running = true
      }
    }
    case "pathfind" => {
      switch_mode(Mode.PICK_1st)
      chosen_edge1 = None
      chosen_edge2 = None
      route_members = Set[Road]()
      repaint
    }
    case "clear-route" => {
      switch_mode(Mode.EXPLORE)
      chosen_edge1 = None
      chosen_edge2 = None
      route_members = Set[Road]()
      repaint
    }
    // TODO refactor the 4 teleports?
    case "teleport-edge" => {
      prompt_int("What edge ID do you seek?") match {
        case Some(id) => {
          try {
            val e = sim.edges(id.toInt)
            // TODO center on some part of the edge and zoom in, rather than
            // just vaguely moving that way
            Util.log("Here's " + e)
            center_on(e.lines.head.start)
            chosen_edge2 = Some(e)  // just kind of use this to highlight it
            repaint
          } catch {
            case _: NumberFormatException => Util.log("Bad edge ID " + id)
          }
        }
        case _ =>
      }
      grab_focus
    }
    case "teleport-road" => {
      prompt_int("What road ID do you seek?") match {
        case Some(id) => {
          try {
            val r = sim.roads(id.toInt)
            // TODO center on some part of the road and zoom in, rather than
            // just vaguely moving that way
            Util.log("Here's " + r)
            center_on(r.all_lanes.head.lines.head.start)
            chosen_road = Some(r)
            repaint
          } catch {
            case _: NumberFormatException => Util.log("Bad edge ID " + id)
          }
        }
        case _ =>
      }
      grab_focus
    }
    case "teleport-agent" => {
      prompt_int("What agent ID do you seek?") match {
        case Some(id) => {
          try {
            sim.get_agent(id.toInt) match {
              case Some(a) => {
                Util.log("Here's " + a)
                current_obj = Some(a)
                handle_ev_keypress(Key.F)
                repaint
              }
              case _ => Util.log("Didn't find " + id)
            }
          } catch {
            case _: NumberFormatException => Util.log("Bad agent ID " + id)
          }
        }
        case _ =>
      }
      grab_focus
    }
    case "teleport-vertex" => {
      prompt_int("What vertex ID do you seek?") match {
        case Some(id) => {
          try {
            val v = sim.vertices(id.toInt)
            Util.log("Here's " + v)
            center_on(v.location)
            repaint
          } catch {
            case _: NumberFormatException => Util.log("Bad agent ID " + id)
          }
        }
        case _ =>
      }
      grab_focus
    }
  }

  def handle_ev_keypress(key: Any): Unit = key match {
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
        case None =>
      }
    }
    case Key.P => {
      handle_ev(EV_Action("toggle-running"))
    }
    case Key.C if current_edge.isDefined => {
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
    case Key.M if current_edge.isDefined => {
      mode match {
        case Mode.EXPLORE => {
          chosen_edge1 = current_edge
          chosen_pos = current_obj.asInstanceOf[Option[Position]]
          switch_mode(Mode.PICK_2nd)
          repaint
        }
        case Mode.PICK_2nd => {
          chosen_edge2 = current_edge
          // TODO make one agent from chosen_pos to current_edge
          chosen_edge1 = None
          chosen_edge2 = None
          chosen_pos = None
          switch_mode(Mode.EXPLORE)
          repaint
        }
        case _ =>
      }
    }
    case Key.OpenBracket => {
      sim.slow_down()
      status.update_speed(sim)
    }
    case Key.CloseBracket => {
      sim.speed_up()
      status.update_speed(sim)
    }
    case Key.Minus => {
      sim.slow_down(5)
      status.update_speed(sim)
    }
    case Key.Equals => {
      sim.speed_up(5)
      status.update_speed(sim)
    }
    case Key.D => current_obj match {
      case Some(thing) => thing.debug
      case None =>
    }
    case Key.F => {
      // Unregister old listener
      camera_agent match {
        case Some(a) => a.route.unlisten("UI")
        case None =>
      }

      camera_agent = current_agent
      camera_agent match {
        case Some(a) => a.route match {
          case r: PathRoute => {
            route_members = r.path.map(_.road).toSet
            r.listen("UI", (ev: Route_Event) => { ev match {
              case EV_Reroute(path) => {
                route_members = path.map(_.road).toSet
              }
              case EV_Transition(from, to) => from match {
                case e: Edge => {
                  route_members -= e.road
                }
                case _ =>
              }
            } })
          }
          case _ =>
        }
        case None => {
          route_members = Set[Road]()
        }
      }
    }
    case Key.X if current_agent.isDefined => {
      if (running) {
        Util.log("Cannot nuke agents while simulation is running!")
      } else {
        val a = current_agent.get
        current_obj = None
        Util.log("WARNING: Nuking " + a)
        //a.terminate
        // TODO remove the agent from the list
      }
    }
    case Key.G => {
      show_green = !show_green
    }
    case Key.T => {
      show_tooltips = !show_tooltips
    }
    case Key.Q => {
      // Run some sort of debuggy thing
      sim.ui_debug
    }
    case _ => // Ignore the rest
  }

  def prompt_generator(src: Seq[Edge], dst: Seq[Edge]): Unit = {
    Util.log("Creating a new generator...")

    // Ask: what type of behavior and route strategy
    // TODO plumb behavior through, too, once there's reason to
    val route_type = Dialog.showInput(
      message = "How should the agents route to their destination?",
      initial = "",
      entries = RouteType.values.toList
    ) match {
      case Some(name) => RouteType.withName(name.toString)
      case None => return
    }

    // Ask: fixed (how many) or continuous (how many per what time)
    // TODO improve the UI.
    Dialog.showOptions(
      message = "Want a fixed, one-time burst or a continuous generator?",
      optionType = Dialog.Options.YesNoCancel, initial = 0,
      entries = Seq("Constant", "Continuous")
    ) match {
      case Dialog.Result.Yes => {
        // Fixed
        prompt_int("How many agents?") match {
          case Some(num) => {
            // TODO make em
          }
          case _ =>
        }
      }
      case Dialog.Result.No => {
        // Continuous
        prompt_double(
          "How often (in simulation-time seconds) do you want one new agent?"
        ) match {
          case Some(time) => {
            // TODO make em
          }
          case _ =>
        }
      }
    }
  }

  def switch_mode(m: Mode.Mode) = {
    mode = m
  }

  def show_pathfinding() = {
    // contract: must be called when current_edge1 and 2 are set
    val from = chosen_edge1.get.directed_road
    val to = chosen_edge2.get.directed_road

    //val route = sim.graph.router.path(from, to)
    val timer = Common.timer("A*")
    val route = (new CongestionRouter(sim.graph)).path(from, to)
    route.foreach(step => println("  - " + step))
    timer.stop()

    // Filter and just remember the edges; the UI doesn't want to highlight
    // turns.
    // TODO pathfinding is by directed road now, not edge. just pick some edge
    // in each group.
    route_members = route.map(_.road).toSet
    repaint
  }
}

sealed trait ScreenLine {
  val line: Line2D.Double
}
final case class RoadLine(a: Coordinate, b: Coordinate, road: Road)
  extends ScreenLine
{
  // center
  val line = new Line2D.Double(a.x, a.y, b.x, b.y)
  // special for one-ways
  val bg_line = if (road.is_oneway) {
                      val l = new Line(a, b).perp_shift(road.num_lanes / 2.0)
                      new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
                    } else {
                      line
                    }
}
final case class EdgeLine(l: Line, edge: Edge) extends ScreenLine {
  val line = new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  val arrow = GeomFactory.draw_arrow(l, l.midpt, 1)  // TODO cfg
}

// TODO what else belongs?
object GeomFactory {
  private val rng = new RNG()

  def draw_arrow(line: Line, base: Coordinate, size: Int): Shape = {
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

    val theta = line.broken_angle
    val x = base.x + (height * math.cos(theta))
    val y = base.y + (height * math.sin(theta))

    // Perpendiculous!
    val theta_perp1 = theta + (math.Pi / 2)
    val cos_perp1 = width * math.cos(theta_perp1)
    val sin_perp1 = width * math.sin(theta_perp1)

    val theta_perp2 = theta - (math.Pi / 2)
    val cos_perp2 = width * math.cos(theta_perp2)
    val sin_perp2 = width * math.sin(theta_perp2)

    val arrow = new Path2D.Double()
    arrow.moveTo(x, y)
    arrow.lineTo(base.x + cos_perp1, base.y + sin_perp1)
    arrow.lineTo(base.x + cos_perp2, base.y + sin_perp2)
    return arrow
  }

  def turn_geom(turn: Turn): Line = {
    // We don't use the conflict_line, since that doesn't draw very
    // informatively, unless lane lines are trimmed back well.
    // Shift the lines to match the EdgeLines we draw.
    val pt1 = turn.from.lines.last.perp_shift(0.5).shift_back()
    val pt2 = turn.to.lines.head.perp_shift(0.5).shift_fwd()
    return new Line(pt1, pt2)
  }

  def rand_color() = new Color(
    rng.double(0.0, 1.0).toFloat,
    rng.double(0.0, 1.0).toFloat,
    rng.double(0.0, 1.0).toFloat
  )
}
