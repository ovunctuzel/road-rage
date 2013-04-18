// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import javafx.scene.Group
import javafx.scene.paint.Color
import javafx.scene.layout.StackPane
import javafx.scene.shape.{Rectangle, Line}
import javafx.scene.text.{Text, Font}

import scala.collection.JavaConversions.asJavaCollection

import utexas.aorta.map.Road
import utexas.aorta.sim.Simulation

// TODO gotta listen to simulation... when new agent or when one disappears,
// mess with it.

// TODO this is really config, but...
object Params {
  val road_width_per_lane = 0.6
  val center_line_width = 0.1
  val center_line_dash = 3.0
  val lane_line_width = 0.05

  // Perpendicular to where the car is going
  val vehicle_width = 0.2
  val vehicle_length = 0.15
  val font_size = 4
}

object Content {
  private var content: Content = null

  def setup(sim: Simulation) = {
    println("Rendering scene...")
    content = new Content(sim)
    println("Rendering done") // TODO timer
  }

  def geometry_root = content.root
}

class Content(sim: Simulation) {
  val root = new Group()

  // TODO keep around mappings in some useful way
  val roads = sim.graph.roads.map(r => new DrawRoad(r))
  roads.foreach(r => root.getChildren.addAll(r.render))
  roads.foreach(r => r.set_zorder)
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

class DrawDriver() {
  val pane = new StackPane()

  val car = new Rectangle(0, 0, Params.vehicle_width, Params.vehicle_length)
  car.setFill(Color.BLUE)

  val label = new Text("A42\n$.50")
  label.setFont(Font.font(Font.getDefault.getFamily, Params.font_size))
  pane.getChildren.addAll(car, label)

  def render = List(pane)
}

// TODO where's this belong?
trait Renderable {
  def debug(): Unit
  def tooltip(): List[String] = List(toString)

  // TODO someday, right-click context menus!
}
