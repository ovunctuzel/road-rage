// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.awt.geom.Rectangle2D
import javax.imageio.ImageIO
import java.io.File

import utexas.aorta.map.Graph
import utexas.aorta.sim.Agent

// Take a screenshot of the current simulation and dump to file
object Snapshot {
  var ui: MapCanvas = null  // cache this

  def take(fn: String, zoom: Double = 1.0) = {
    if (ui == null) {
      ui = new MapCanvas(Agent.sim)
    }

    // TODO problem is memory... maps can get huge.
    val w = (Graph.width * zoom).toInt
    val h = (Graph.height * zoom).toInt
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val g2d = img.createGraphics

    // set background
    g2d.setColor(Color.LIGHT_GRAY)
    g2d.fillRect(0, 0, w, h)

    // zoom
    g2d.scale(zoom, zoom)
    ui.zoom = zoom

    // yes, always take the time to draw pretty pictures.
    // ew, clunky old java crap.
    val antialiasing = new java.util.HashMap[Any,Any]()
    antialiasing.put(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )
    g2d.setRenderingHints(antialiasing)

    ui.render_canvas(g2d, new Rectangle2D.Double(0, 0, w, h), g2d.getTransform,
                     g2d.getTransform)
    ImageIO.write(img, "PNG", new File(fn))
  }
}
