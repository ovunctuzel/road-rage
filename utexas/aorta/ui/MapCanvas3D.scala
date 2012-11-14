// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.universe.SimpleUniverse
import com.sun.j3d.exp.swing.JCanvas3D
import javax.media.j3d.{Canvas3D, BranchGroup, BoundingSphere, AmbientLight,
                        Shape3D, QuadArray, GeometryArray}
import javax.vecmath.{Point3d, Color3f, Point3f}
import java.awt.Dimension

import utexas.aorta.sim.Simulation

import utexas.aorta.{Util, cfg}

class MapCanvas3D(sim: Simulation) {
  val canvas = new JCanvas3D()
  canvas.setPreferredSize(new Dimension(600, 400))
  //canvas.setSize(600, 400)  <-- sets a fixed size!

  def setup() = {
    val use_canvas = canvas.getOffscreenCanvas3D
    val universe = new SimpleUniverse(use_canvas)
    universe.addBranchGraph(create_scene_graph)
    universe.getViewingPlatform.setNominalViewingTransform
    val orbit = new OrbitBehavior(use_canvas)
    val bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100000.0)
    orbit.setSchedulingBounds(bounds)
    universe.getViewingPlatform.setViewPlatformBehavior(orbit)
  }

  def create_scene_graph(): BranchGroup = {
    val group = new BranchGroup()

    group.addChild(new Sphere(0.3f))

    group.addChild(
      new FlatRect(-30f, -10f, -5f, -5f, new Color3f(1.0f, 0f, 0f))
    )

    // TODO light up better
    val light = new AmbientLight(new Color3f(1.0f, 1.4f, .1f))
    val bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0), 1000.0);
    light.setInfluencingBounds(bounds)
    group.addChild(light)

    return group
  }
}

class FlatRect(x1: Float, y1: Float, x2: Float, y2: Float, color: Color3f) extends Shape3D {
  setup

  def setup() = {
    val plane = new QuadArray(4, GeometryArray.COORDINATES | GeometryArray.COLOR_3)
    val points = List(
      new Point3f(x1, y1, 0f),
      new Point3f(x2, y1, 0f),
      new Point3f(x2, y2, 0f),
      new Point3f(x1, y2, 0f)
    )
    val colors = List(color, color, color, color)
    plane.setCoordinates(0, points.toArray)
    plane.setColors(0, colors.toArray)
    setGeometry(plane)
  }
}

// TODO the resize issue. 
// http://www.java.net/node/660790
// problem seems to be offscreen buffer doesnt get created
// 1) just use the scala canvas stuff?
// 2) disable layout manager, where?
// 3) jcanvas 3d can work, but it doesnt resize, and it also breaks mouse orbit
// behavior.
