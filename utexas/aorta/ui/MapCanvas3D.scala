// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import com.sun.j3d.utils.behaviors.keyboard.KeyNavigatorBehavior
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.universe.SimpleUniverse
import javax.media.j3d.{Canvas3D, BranchGroup, BoundingSphere, AmbientLight,
                        Shape3D, QuadArray, GeometryArray, Appearance,
                        LineArray, Background, LineAttributes, Transform3D}
import javax.vecmath.{Point3d, Color3f, Point3f, Vector3d}

import utexas.aorta.sim.Simulation
import utexas.aorta.map.{Graph, Road}

import utexas.aorta.{Util, cfg}

class MapCanvas3D(sim: Simulation) {
  val canvas = setup_canvas
  // TODO something about sharing colors is screwing up
  def white = new Color3f(1.0f, 1.0f, 1.0f)
  def grey = new Color3f(0.5f, 0.5f, 0.5f)

  def setup_canvas(): Canvas3D = {
    val c = new Canvas3D(SimpleUniverse.getPreferredConfiguration)
    c.setSize(600, 400) // TODO resizing?
    val universe = new SimpleUniverse(c)
    val scene = create_scene_graph

    val key = new KeyNavigatorBehavior(universe.getViewingPlatform.getViewPlatformTransform)
    key.setSchedulingBounds(new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100000.0))
    scene.addChild(key)

    universe.addBranchGraph(scene)

    universe.getViewingPlatform.setNominalViewingTransform
    /*val camera = universe.getViewingPlatform.getViewPlatformTransform
    val t3d = new Transform3D()
    camera.getTransform(t3d)
    t3d.lookAt(
      new Point3d(0, 5, 0),  // eye is at
      new Point3d(1, 0, 1),  // look at
      new Vector3d(0, 1, 0)   // up
    )
    t3d.invert
    camera.setTransform(t3d)*/
    val orbit = new OrbitBehavior(c)
    val bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100000.0)
    orbit.setSchedulingBounds(bounds)
    universe.getViewingPlatform.setViewPlatformBehavior(orbit)

    return c
  }

  def create_scene_graph(): BranchGroup = {
    val scene = new BranchGroup()

    // White background.
    scene.addChild(new Background(white))

    // Draw a gray backdrop.
    scene.addChild(
      new FlatRect(0f, 0f, Graph.width.toFloat, Graph.height.toFloat, grey)
    )

    // TODO axes?

    // Draw roads
    for (r <- sim.roads) {
      scene.addChild(new FlatRoad(r))
    }

    // TODO light up better
    val light = new AmbientLight(white)
    val bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0), 1000.0);
    light.setInfluencingBounds(bounds)
    scene.addChild(light)

    //scene.compile // TODO does this help?
    return scene
  }
}

class FlatRect(x1: Float, z1: Float, x2: Float, z2: Float, color: Color3f) extends Shape3D {
  setup

  def setup() = {
    val plane = new QuadArray(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
    val points = List(
      new Point3f(x1, 0f, z1),
      new Point3f(x2, 0f, z1),
      new Point3f(x2, 0f, z2),
      new Point3f(x1, 0f, z2)
    )
    val colors = List(color, color, color, color)
    plane.setCoordinates(0, points.toArray)
    plane.setColors(0, colors.toArray)
    setGeometry(plane)
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
