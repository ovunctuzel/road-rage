// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import com.sun.j3d.utils.behaviors.vp.{ViewPlatformBehavior, OrbitBehavior}
import com.sun.j3d.utils.behaviors.keyboard.KeyNavigatorBehavior
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.universe.SimpleUniverse
import javax.media.j3d.{Canvas3D, BranchGroup, BoundingSphere, AmbientLight,
                        Shape3D, QuadArray, GeometryArray, Appearance,
                        LineArray, Background, LineAttributes, Transform3D,
                        PolygonAttributes, WakeupOnAWTEvent}
import javax.vecmath.{Point3d, Color3f, Point3f, Vector3d}
import java.awt.AWTEvent
import java.awt.event.KeyEvent

import utexas.aorta.sim.Simulation
import utexas.aorta.map.{Graph, Road}

import utexas.aorta.{Util, cfg}

class MapCanvas3D(sim: Simulation) {
  val canvas = setup_canvas
  // TODO something about sharing colors is screwing up
  def white = new Color3f(1.0f, 1.0f, 1.0f)
  def black = new Color3f(0.0f, 0.0f, 0.0f)
  def grey = new Color3f(0.5f, 0.5f, 0.5f)
  def green = new Color3f(0.0f, 0.5f, 0.1f)
  def blue = new Color3f(0.0f, 0.1f, 0.4f)
  def red = new Color3f(0.7f, 0.1f, 0.2f)

  def setup_canvas(): Canvas3D = {
    val c = new Canvas3D(SimpleUniverse.getPreferredConfiguration)
    c.setSize(600, 400) // TODO resizing?
    val universe = new SimpleUniverse(c)
    val scene = create_scene_graph
    universe.addBranchGraph(scene)

    // Set up the initial view
    val camera = universe.getViewingPlatform.getViewPlatformTransform
    val t3d = new Transform3D()
    camera.getTransform(t3d)
    t3d.setTranslation(new Vector3d(-1, 1, -1))
    camera.setTransform(t3d)

    // Set up controls
    val behav = new KeyBehavior()
    behav.setSchedulingBounds(
      new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100000.0)
    )
    universe.getViewingPlatform.setViewPlatformBehavior(behav)

    return c
  }

  def create_scene_graph(): BranchGroup = {
    val scene = new BranchGroup()
    val bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0), 1000.0);

    // Background.
    val bg = new Background(black)
    bg.setApplicationBounds(bounds)
    scene.addChild(bg)

    // TODO light up better
    val light = new AmbientLight(white)
    light.setInfluencingBounds(bounds)
    scene.addChild(light)

    // TODO axes?

    scene.addChild(new Sphere(0.3f))

    // draw some tiles and stuff
    scene.addChild(new FlatRect(0f, 0f, 1f, 1f, green))
    scene.addChild(new FlatRect(1f, 2f, 2f, 2f, blue))
    scene.addChild(new FlatRect(0f, 1f, 1f, 2f, red))

    /*// Draw roads
    for (r <- sim.roads) {
      scene.addChild(new FlatRoad(r))
    }*/

    //scene.compile
    return scene
  }
}

class FlatRect(x1: Float, z1: Float, x2: Float, z2: Float, color: Color3f) extends Shape3D {
  setup

  def setup() = {
    val plane = new QuadArray(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
    val points = List(
      new Point3f(x1, 0f, z1),
      new Point3f(x1, 0f, z2),
      new Point3f(x2, 0f, z2),
      new Point3f(x2, 0f, z1)
    )
    val colors = List(color, color, color, color)
    plane.setCoordinates(0, points.toArray)
    plane.setColors(0, colors.toArray)
    setGeometry(plane)

    val app = new Appearance()
    val attribs = new PolygonAttributes()
    // visible from both sides
    attribs.setCullFace(PolygonAttributes.CULL_NONE)
    app.setPolygonAttributes(attribs)
    setAppearance(app)
  }
}

class FlatRoad(r: Road) extends Shape3D {
  setup
  
  def setup() = {
    val lines = new LineArray(r.points.size, GeometryArray.COORDINATES | GeometryArray.COLOR_3) 
    val color = new Color3f(1.0f, 0.2f, 0.2f) // TODO black?
    for ((pt, i) <- r.points.zipWithIndex) {
      Util.log(s"$r has $i = $pt")
      // float above backdrop?
      lines.setCoordinate(0, new Point3f(pt.x.toFloat, 0.1f, pt.y.toFloat))
      lines.setColor(i, color)
    }
    setGeometry(lines)

    val road_appearance = new Appearance()
    val road_thickness = new LineAttributes()
    road_thickness.setLineWidth(10.0f)
    road_appearance.setLineAttributes(road_thickness)
    setAppearance(road_appearance)
  }
}

// From http://fivedots.coe.psu.ac.th/~ad/jg2/ch07/index.html
class KeyBehavior() extends ViewPlatformBehavior {
  val rot_amount = math.Pi / 36.0 // 5 degrees
  val move_step = 0.2

  val fwd = new Vector3d(0, 0, -move_step)
  val back = new Vector3d(0, 0, move_step)
  val left = new Vector3d(-move_step, 0, 0)
  val right = new Vector3d(move_step, 0, 0)
  val up = new Vector3d(0, move_step, 0)
  val down = new Vector3d(0, -move_step, 0)

  val fwd_key = KeyEvent.VK_UP
  val back_key = KeyEvent.VK_DOWN
  val left_key = KeyEvent.VK_LEFT
  val right_key = KeyEvent.VK_RIGHT

  var key_press = new WakeupOnAWTEvent(KeyEvent.KEY_PRESSED)
  var t3d = new Transform3D()
  var to_move = new Transform3D()
  var to_rot = new Transform3D()

  var up_moves = 0

  override def initialize() {
    wakeupOn(key_press)
  }

  override def processStimulus(criteria: java.util.Enumeration[_]) {
    while (criteria.hasMoreElements()) {
      val wakeup = criteria.nextElement
      wakeup match {
        case w: WakeupOnAWTEvent => {
          val event = w.getAWTEvent
          for (ev <- event if ev.getID == KeyEvent.KEY_PRESSED) {
            processKeyEvent(ev.asInstanceOf[KeyEvent])
          }
        }
      }
    }
    wakeupOn(key_press)
  }

  def processKeyEvent(ev: KeyEvent) {
    val key = ev.getKeyCode
    if (ev.isAltDown) {
      key match {
        case `fwd_key` => {
          up_moves += 1
          doMove(up)
        }
        case `back_key` => {
          if (up_moves > 0) {
            up_moves -= 1
            doMove(down)
          }
        }
        case `left_key` => doMove(left)
        case `right_key` => doMove(right)
        case _ =>
      }
    } else {
      key match {
        case `fwd_key` => doMove(fwd)
        case `back_key` => doMove(back)
        case `left_key` => rotateY(rot_amount)
        case `right_key` => rotateY(-rot_amount)
        case _ =>
      }
    }
  }

  def rotateY(radians: Double) = {
    targetTG.getTransform(t3d)
    to_rot.rotY(radians)
    t3d.mul(to_rot)
    targetTG.setTransform(t3d)
    println("rot " + to_rot)
  }

  def doMove(move: Vector3d) = {
    targetTG.getTransform(t3d)
    to_move.setTranslation(move)
    t3d.mul(to_move)
    targetTG.setTransform(t3d)
    println("translate " + to_move)
  }
}
