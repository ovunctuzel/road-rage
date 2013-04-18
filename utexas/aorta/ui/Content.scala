// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import javafx.scene.Group
import javafx.scene.paint.Color
import javafx.scene.layout.StackPane
import javafx.scene.shape.{Rectangle, Line}
import javafx.scene.text.{Text, Font}
import javafx.scene.transform.Rotate

import scala.collection.JavaConversions.asJavaCollection
import scala.collection.mutable

import utexas.aorta.map.Road
import utexas.aorta.sim.{Simulation, Agent, Sim_Event, EV_Agent_Start,
                         EV_Agent_Stop}
import utexas.aorta.cfg

// TODO possibly have callbacks for everything! when agent moves, asap translate
// it?

// TODO this is really config, but...
object Params {
  val road_width_per_lane = 0.6
  val center_line_width = 0.1
  val center_line_dash = 3.0
  val lane_line_width = 0.05

  // Perpendicular to where the car is going
  val vehicle_width = 0.15
  val vehicle_length = 0.2
  val font_size = 4
}

object Content {
  var content: Content = null

  def setup(sim: Simulation) = {
    println("Rendering scene...")
    content = new Content(sim)
    println("Rendering done") // TODO timer
  }

  def geometry_root = content.root
}

class Content(val sim: Simulation) {
  val root = new Group()

  // TODO keep around mappings in some useful way
  val roads = sim.graph.roads.map(r => new DrawRoad(r))
  roads.foreach(r => root.getChildren.addAll(r.render))
  roads.foreach(r => r.set_zorder)

  val agents = new mutable.HashMap[Agent, DrawDriver]()

  sim.listen("gui", (ev: Sim_Event) => { ev match {
    case EV_Agent_Start(a) => {
      agents(a) = new DrawDriver(a)
      root.getChildren.add(agents(a).render)
    }
    case EV_Agent_Stop(a) => {
      // TODO more efficiently by batching and/or making a group just for
      // agents?
      root.getChildren.remove(agents(a).render)
      agents -= a
    }
    case _ =>
  }})

  def step() = {
    println(s"at ${sim.tick}")
    sim.step()

    // Process updated things
    sim.agents.foreach(a => agents(a).redraw())
  }
}

class DrawRoad(road: Road) {
  val bg_lines = road.pairs_of_points.map(
    pair => new Line(pair._1.x, pair._1.y, pair._2.x, pair._2.y)
  ).toList
  for (line <- bg_lines) {
    line.setStrokeWidth(road.num_lanes * Params.road_width_per_lane)
    line.setStroke(Color.BLACK)
    line.setSmooth(false)
  }

  val center_lines = road.pairs_of_points.map(
    pair => new Line(pair._1.x, pair._1.y, pair._2.x, pair._2.y)
  ).toList
  for (line <- center_lines) {
    line.setStrokeWidth(Params.center_line_width)
    line.setStroke(Color.YELLOW)
    line.setSmooth(false)
    //line.getStrokeDashArray.add(Params.center_line_dash)
  }

  val fg_lines = road.all_lanes.flatMap(e => e.lines.map(_.perp_shift(0.5)).map(
    line => new Line(line.x1, line.y1, line.x2, line.y2)
  ))
  for (line <- fg_lines) {
    line.setStrokeWidth(Params.lane_line_width)
    line.setStroke(Color.WHITE)
    line.setSmooth(false)
  }

  def render = bg_lines ++ center_lines ++ fg_lines
  def set_zorder() = {
    for (line <- bg_lines) {
      line.toBack()
    }
  }
}

class DrawDriver(a: Agent) {
  val pane = new StackPane()
  val rotate = new Rotate()
  pane.getTransforms.add(rotate)

  val car = new Rectangle(0, 0, Params.vehicle_length, Params.vehicle_width)
  car.setFill(Color.BLUE) // TODO remember random color

  val label = new Text(s"A${a.id}")
  label.setFont(Font.font(Font.getDefault.getFamily, Params.font_size))
  pane.getChildren.addAll(car)//, label)

  def render = pane

  def redraw() = {
    label.setText(s"A${a.id}\n${a.wallet.budget}")

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
    pane.setTranslateX(front_pt.x - Params.vehicle_length)
    pane.setTranslateY(front_pt.y - (Params.vehicle_width / 2.0))
    rotate.setAngle(-line.angle)
    rotate.setPivotX(front_pt.x)
    rotate.setPivotY(front_pt.y)
  }
}

// TODO where's this belong?
trait Renderable {
  def debug(): Unit
  def tooltip(): List[String] = List(toString)

  // TODO someday, right-click context menus!
}
