// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import java.io.FileWriter

import utexas.aorta.sim.Simulation
import utexas.aorta.map.Graph
import utexas.aorta.{Util, cfg}

// Dump an SVG of the map.
object SVG {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args, true, false)
    val out = new FileWriter("output.svg")
    dump(sim, out)
    out.close
  }

  def dump(sim: Simulation, out: FileWriter) = {
    out.write(
      // TODO string interp + escaping quotes?
      "<svg width=\"" + Graph.width + "\" height=\"" + Graph.height + "\">\n"
    )

    // TODO scale everything up?

    // Roads!
    for (r <- sim.roads) {
      // TODO special case for one-ways
      // TODO leading comma/space cause problems?
      out.write("  <polyline points=\"")
      for (pt <- r.points) {
        out.write(" " + pt.x + " " + pt.y)
      }
      out.write("\" style=\"stroke: grey; fill: none; stroke-width: ")
      out.write((10 * r.num_lanes) + ";\"/>\n")
    }
    // Then the center line...
    for (r <- sim.roads) {
      out.write("  <polyline points=\"")
      for (pt <- r.points) {
        out.write(" " + pt.x + " " + pt.y)
      }
      out.write("\" style=\"stroke: yellow; fill: none; stroke-width: 2;\"/>\n")
    }

    // Edges!
    for (e <- sim.edges) {
      out.write("  <polyline points=\"")
      // Avoid duplicate points
      for (line <- e.lines) {
        out.write(" " + line.x1 + " " + line.y1)
      }
      // Catch the last
      val last = e.lines.last
      out.write(" " + last.x2 + " " + last.y2)
      out.write("\" style=\"stroke: black; fill: none; stroke-width: 1;\"/>\n")
    }

    out.write("</svg>\n")
  }
}
