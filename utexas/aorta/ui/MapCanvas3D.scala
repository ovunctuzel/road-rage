// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.universe.SimpleUniverse
import javax.media.j3d.{Canvas3D, BranchGroup, BoundingSphere, AmbientLight}
import javax.vecmath.{Point3d, Color3f}

import utexas.aorta.sim.Simulation

import utexas.aorta.{Util, cfg}

class MapCanvas3D(sim: Simulation) {
  val canvas = setup_canvas

  def setup_canvas(): Canvas3D = {
    val c = new Canvas3D(SimpleUniverse.getPreferredConfiguration)
    c.setSize(600, 400) // TODO resizing?
    val universe = new SimpleUniverse(c)
    universe.addBranchGraph(create_scene_graph)
    universe.getViewingPlatform.setNominalViewingTransform
    val orbit = new OrbitBehavior(c)
    val bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100000.0)
    orbit.setSchedulingBounds(bounds)
    universe.getViewingPlatform.setViewPlatformBehavior(orbit)
    return c
  }

  def create_scene_graph(): BranchGroup = {
    val group = new BranchGroup()

    group.addChild(new Sphere(0.3f))

    // TODO light up more
    val light = new AmbientLight(new Color3f(1.0f, 1.4f, .1f))
    val bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0), 1000.0);
    light.setInfluencingBounds(bounds)
    group.addChild(light)

    return group
  }
}
