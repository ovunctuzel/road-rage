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

import utexas.aorta.map._  // TODO yeah getting lazy.
import utexas.aorta.sim.{Simulation, Agent, FixedSizeGenerator,
                         ContinuousGenerator, SpecificGenerator, Sim_Event,
                         EV_Signal_Change}
import utexas.aorta.sim.policies.{GreenFlood, Cycle}

import utexas.aorta.{Util, cfg}

object Mode extends Enumeration {
  type Mode = Value
  val EXPLORE = Value("Explore")  // just chilling
  val PICK_1st = Value("Pick 1st edge") // for now, only client is pathfinding
  val PICK_2nd = Value("Pick 2nd edge")
}

// TODO maybe adaptor class for our map geometry? stick in a 'render road' bit

class MapCanvas(sim: Simulation) extends ScrollingCanvas {
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
              if (sim.agents.contains(a)) {
                center_on(a.at.location)
              } else {
                Util.log(a + " is done; the camera won't stalk them anymore")
                camera_agent = None
              }
            }
            case None =>
          }
          // always render
          handle_ev(EV_Action("step"))
        } else {
          // poll the generators
          if (sim.pre_step) {
            // only render when a generator did something
            handle_ev(EV_Action("step"))
          }
        }
      }
    }
  }.start
  // but we can also pause
  var running = false

  // state
  private var current_obj: Option[Renderable] = None
  private var highlight_type: Option[String] = None
  private var show_ward_colors = false
  private var current_turn = -1  // for cycling through turns from an edge
  private var show_wards = false
  private var mode = Mode.EXPLORE
  private var chosen_edge1: Option[Edge] = None
  private var chosen_edge2: Option[Edge] = None
  private var chosen_road: Option[Road] = None
  private var chosen_pos: Option[Position] = None
  private var route_members = Set[Edge]()
  private var route_members_road = Set[Road]()
  private var polygon_roads1: Set[Road] = Set()
  private var polygon_roads2: Set[Road] = Set()
  private var camera_agent: Option[Agent] = None
  private val green_turns = new HashMap[Turn, Shape]()
  private var show_green = false

  def current_edge: Option[Edge] = current_obj match {
    case Some(pos: Position) => Some(pos.on.asInstanceOf[Edge])
    case _ => None
  }
  def current_agent: Option[Agent] = current_obj match {
    case Some(a: Agent) => Some(a)
    case _ => None
  }

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

  def canvas_width = Graph.width.toInt
  def canvas_height = Graph.height.toInt

  // pre-render base roads.
  // TODO this was too ridiculous a sequence comprehension, so use a ListBuffer,
  // which is supposed to be O(1) append and conversion to list.
  Util.log("Pre-rendering road geometry...")
  Util.log_push

  val road2lines = new HashMap[Road, MutableSet[RoadLine]]
    with MultiMap[Road, RoadLine]
  Util.log("Road lines...")
  val bg_lines = build_bg_lines

  // this is only used for finer granularity searching...
  val edge2lines = new HashMap[Edge, MutableSet[EdgeLine]]
    with MultiMap[Edge, EdgeLine]
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
      // draw the lines on the borders of lanes, not in the middle
      val line = new EdgeLine(l.shift_line(0.5), e)
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
  sim.listeners :+= ((ev: Sim_Event) => { ev match {
    case EV_Signal_Change(reds, greens) => {
      green_turns --= reds
      for (t <- greens) {
        green_turns(t) = GeomFactory.curved_turn(t)
      }
    }
  } })

  // At this point, signal policies have already fired up and sent the first
  // round of greens. We missed it, so compute manually the first time.
  // TODO better solution
  for (v <- sim.vertices) {
    for (t <- v.intersection.policy.current_greens) {
      green_turns(t) = GeomFactory.curved_turn(t)
    }
  }

  // TODO colors for everything belong in cfg.

  def render_canvas(g2d: Graphics2D, window: Rectangle2D.Double): Option[String] = {
    // remember these so we can draw center lines more efficiently
    val roads_seen = new ListBuffer[RoadLine]

    // Draw the first layer (roads) - either all or just major ones
    for (l <- bg_lines if (l.line.intersects(window) &&
                           (!show_wards || sim.special_ward.roads(l.road))))
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

      // TODO tmp -- trying out intersection merging
      g2d.setColor(Color.CYAN)
      for (v <- sim.vertices) {
        val bub = bubble(v.location)
        if (bub.intersects(window)) {
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
      }
    })

    // Finally, if the user is free-handing a region, show their work.
    g2d.setColor(Color.RED)
    g2d.setStroke(drawing_stroke)
    g2d.draw(polygon)

    // What tooltip do we want?
    return current_obj match {
      case Some(pos: Position) => Some(f"${pos.on} at ${pos.dist}%.2f")
      case Some(thing) => Some(thing.toString)
      case None => None
    }
  }

  // we return any new roads seen
  def draw_ward(g2d: Graphics2D, w: WardBubble): Set[RoadLine] =
    current_obj match {
      // Show the roads of the highlighted ward
      case (Some(ward: Ward)) if w.ward == ward => {
        val lines = ward.roads.flatMap(road2lines(_))
        lines.foreach(l => draw_road(g2d, l))
        lines
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
        Set()
      }
    }

  def draw_road(g2d: Graphics2D, l: RoadLine) = {
    g2d.setColor(color_road(l.road))
    if (route_members_road(l.road)) {
      g2d.setStroke(strokes(l.road.num_lanes * 2))
    } else {
      g2d.setStroke(strokes(l.road.num_lanes))
    }
    g2d.draw(l.bg_line)
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

  def draw_agent(g2d: Graphics2D, a: Agent) = {
    // try to avoid flashing red, this feature is used to visually spot true
    // clumps
    if (a.speed == 0.0 && a.how_long_idle >= 5.0) {
      g2d.setColor(Color.RED)
    } else {
      if (!agent_colors.contains(a)) {
        agent_colors(a) = GeomFactory.rand_color
      }
      g2d.setColor(agent_colors(a))
    }
    if (zoomed_in) {
      // TODO cfg. just tweak these by sight.
      val vehicle_length = 0.5  // along the edge
      val vehicle_width = 0.25  // perpendicular

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
    else if (route_members_road(r))
      Color.GREEN
    else if (polygon_roads1(r))
      Color.RED
    else if (polygon_roads2(r))
      Color.GREEN
    else if (show_ward_colors)
      ward_colorings(r.ward)
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
    else if (route_members(e))
      Color.GREEN
    else if (e.doomed)
      // TODO configurable.
      Color.RED
    else
      Color.WHITE

  def draw_turn(g2d: Graphics2D, turn: Turn, color: Color) = {
    val curve = GeomFactory.curved_turn(turn)
    g2d.setColor(color)
    g2d.draw(curve)
    // This is math copied from curved_turn :P
    val pt2 = turn.from.to.location
    val pt3 = turn.to.shifted_start_pt(0.5)
    g2d.fill(
      GeomFactory.draw_arrow(new Line(pt2.x, pt2.y, pt3.x, pt3.y), 2)
    )
  }

  def draw_intersection(g2d: Graphics2D, e: Edge) = {
    if (current_turn == -1) {
      // show all turns
      for (turn <- e.next_turns) {
        draw_turn(g2d, turn, Color.WHITE)
      }
    } else {
      // show one turn and its conflicts
      val turn = e.next_turns(current_turn)
      draw_turn(g2d, turn, Color.WHITE)

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

    // Order of search: agents, vertices, edges, roads, wards
    // TODO ideally, center agent bubble where the vehicle center is drawn.

    // TODO this is _kind_ of ugly.
    current_obj = sim.agents.find(a => hit(agent_bubble(a))) match {
      case None => sim.vertices.find(v => hit(bubble(v.location))) match {
        case None => fg_lines.find(l => hit(l.line)) match {
          case None => bg_lines.find(l => hit(l.line)) match {
            case None if show_wards => ward_bubbles.find(b => hit(b.bubble)) match {
              case None => None
              case Some(b) => Some(b.ward)
            }
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
    case EV_Mouse_Moved(x, y) => {
      redo_mouseover(x, y)
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
        case None =>
      }
    }
    case EV_Key_Press(Key.P) => {
      handle_ev(EV_Action("toggle-running"))
    }
    case EV_Key_Press(Key.W) => {
      handle_ev(EV_Action("toggle-wards"))
    }
    case EV_Action("spawn-army") => {
      prompt_generator(sim.edges, sim.edges)
    }
    case EV_Action("step") => {
      val note = if (running)
                   ""
                 else
                   " [Paused]"
      status.time.text = "%.1f %s".format(sim.tick, note)
      status.update_speed(sim)
      // agents have maybe moved, so...
      status.agents.text = sim.describe_agents
      update_plot
      repaint
    }
    case EV_Action("toggle-running") => {
      if (running) {
        running = false
      } else {
        running = true
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
      route_members_road = Set[Road]()
      repaint
    }
    case EV_Action("clear-route") => {
      switch_mode(Mode.EXPLORE)
      chosen_edge1 = None
      chosen_edge2 = None
      route_members = Set[Edge]()
      route_members_road = Set[Road]()
      repaint
    }
    case EV_Action("teleport-edge") => {
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
    case EV_Action("teleport-road") => {
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
    case EV_Action("teleport-agent") => {
      prompt_int("What agent ID do you seek?") match {
        case Some(id) => {
          try {
            sim.agents.find(a => a.id == id.toInt) match {
              case Some(a) => {
                Util.log("Here's " + a)
                center_on(a.at.location)
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
    case EV_Action("teleport-vertex") => {
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
    case EV_Key_Press(Key.M) if current_edge.isDefined => {
      mode match {
        case Mode.EXPLORE => {
          chosen_edge1 = current_edge
          chosen_pos = current_obj.asInstanceOf[Option[Position]]
          switch_mode(Mode.PICK_2nd)
          repaint
        }
        case Mode.PICK_2nd => {
          chosen_edge2 = current_edge
          sim.add_gen(new SpecificGenerator(
            sim, "Static A*",
            // TODO avoid some nasty casting if we specialize an EdgePosition
            List((chosen_pos.get.on.asInstanceOf[Edge], current_edge.get, chosen_pos.get.dist))
          ))
          chosen_edge1 = None
          chosen_edge2 = None
          chosen_pos = None
          switch_mode(Mode.EXPLORE)
          repaint
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
      sim.slow_down()
      status.update_speed(sim)
    }
    case EV_Key_Press(Key.CloseBracket) => {
      sim.speed_up()
      status.update_speed(sim)
    }
    case EV_Key_Press(Key.Minus) => {
      sim.slow_down(5)
      status.update_speed(sim)
    }
    case EV_Key_Press(Key.Equals) => {
      sim.speed_up(5)
      status.update_speed(sim)
    }
    // TODO move the debug string to the renderable trait
    case EV_Key_Press(Key.D) => current_obj match {
      case Some(a: Agent) => {
        a.debug
        sim.debug_agent = Some(a)
      }
      case Some(thing) => thing.debug
      case None =>
    }
    case EV_Key_Press(Key.F) => { camera_agent = current_agent }
    case EV_Key_Press(Key.X) if current_agent.isDefined => {
      if (running) {
        Util.log("Cannot nuke agents while simulation is running!")
      } else {
        val a = current_agent.get
        current_obj = None
        Util.log("WARNING: Nuking " + a)
        // simulate being done
        a.exit(a.at.on)
        a.upcoming_intersections.foreach(i => i.unregister(a))
        a.upcoming_intersections = Set()
        sim.agents -= a
      }
    }
    case EV_Key_Press(Key.G) => {
      show_green = !show_green
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
        // TODO populate seq from what sim uses
        entries = Seq("Never Go", "Stop Sign", "Signal Cycle", "Reservation")
      ) match {
        case Some(name) => {
          val builder = Simulation.policy_builder(name.toString)
          intersections.foreach(i => i.policy = builder(i))
        }
        case None =>
      }
    }
    case EV_Key_Press(_) => // Ignore the rest
    case _ =>
  }

  // TODO ew, even refactored, these are a bit ugly.
  def prompt_int(msg: String): Option[Int] = Dialog.showInput(
    message = msg, initial = ""
  ) match {
    case Some(num) => {
      try {
        Some(num.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    }
    case _ => None
  }

  def prompt_double(msg: String): Option[Double] = Dialog.showInput(
    message = msg, initial = ""
  ) match {
    case Some(num) => {
      try {
        Some(num.toDouble)
      } catch {
        case _: NumberFormatException => None
      }
    }
    case _ => None
  }

  def prompt_generator(src: Seq[Edge], dst: Seq[Edge]): Unit = {
    Util.log("Creating a new generator...")

    // Ask: what type of behavior and route strategy
    // TODO plumb behavior through, too, once there's reason to
    val route_type = Dialog.showInput(
      message = "How should the agents route to their destination?",
      initial = "",
      // TODO populate seq from what sim uses
      entries = Seq("Static A*", "Drunken", "Directional Drunk",
                    "Drunken Explorer")
    ) match {
      case Some(name) => name.toString
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
            sim.add_gen(new FixedSizeGenerator(
              sim, src, dst, num, route_type
            ))
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
            sim.add_gen(new ContinuousGenerator(
              sim, src, dst, time, route_type
            ))
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
    val r = sim.pathfind_astar(chosen_edge1.get, chosen_edge2.get)
    // Filter and just remember the edges; the UI doesn't want to highlight
    // turns.
    // TODO pathfinding is by directed road now, not edge. just pick some edge
    // in each group.
    route_members = r.map(_.edges.head).toSet
    route_members_road = route_members.map(_.road)
    repaint
  }

  def update_plot() = {
    // TODO plot multiple things, or let the user select, etc.
    // TODO and we should own the chart, not viewer.
    Viewer.chart_data.addPoint(sim.tick, sim.agents.size)
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
                      val l = new Line(a, b).shift_line(road.num_lanes / 2.0)
                      new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
                    } else {
                      line
                    }
}
final case class EdgeLine(l: Line, edge: Edge) extends ScreenLine {
  val line = new Line2D.Double(l.x1, l.y1, l.x2, l.y2)
  val arrow = GeomFactory.draw_arrow(l, 1)  // TODO cfg
}
// and, separately...
class WardBubble(val ward: Ward) {
  private val r = 2.0 + (0.1 * ward.roads.size) // TODO cfg
  val bubble = new Ellipse2D.Double(
    ward.center.x - r, ward.center.y - r, r * 2, r * 2
  )
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

  def curved_turn(turn: Turn): Shape = {
    // We don't use the conflict_line, since that doesn't draw very
    // informatively, unless lane lines are trimmed back well.
    val pt1 = turn.from.shifted_end_pt(0.5)
    val pt2 = turn.from.to.location
    val pt3 = turn.to.shifted_start_pt(0.5)
    return new CubicCurve2D.Double(
      pt1.x, pt1.y, pt2.x, pt2.y, pt2.x, pt2.y, pt3.x, pt3.y
    )

    /*val line = turn.conflict_line
    return new Line2D.Double(line.start.x, line.start.y, line.end.x, line.end.y)*/

    /*val pt1 = turn.conflict_line.start
    val pt2 = turn.from.to.location
    val pt3 = turn.conflict_line.end
    return new CubicCurve2D.Double(
      pt1.x, pt1.y, pt2.x, pt2.y, pt2.x, pt2.y, pt3.x, pt3.y
    )*/
  }

  def rand_color() = new Color(
    Util.rand_double(0.0, 1.0, Util.util_rng).toFloat,
    Util.rand_double(0.0, 1.0, Util.util_rng).toFloat,
    Util.rand_double(0.0, 1.0, Util.util_rng).toFloat
  )
}
