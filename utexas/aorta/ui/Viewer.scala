// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing._  // TODO figure out exactly what
import java.awt.{Color, Component}
import swing.Dialog

import utexas.aorta.sim.{Simulation, Headless}
import utexas.aorta.Util

object Status_Bar {
  val zoom       = new Label("1.0") // TODO from cfg
  val agents     = new Label("0 / 0 / 0 (0 generators)")
  val time       = new Label("0.0 [Paused]")
  val time_speed = new Label("1.0x")
  val mode       = new Label("" + Mode.EXPLORE)
  val location   = new Label("Nowhere")

  // TODO could put methods here to set text!

  val panel = new GridBagPanel {
    // TODO config for all the sizings...
    maximumSize = new Dimension(Int.MaxValue, 10)
    border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)

    // TODO generate these?

    // all of this to prevent the rightmost 'At' column from spazzing out when the text
    // changes length
    // row 1: labels
    val c = new Constraints
    c.gridx = 0
    c.gridy = 0
    c.ipadx = 50
    layout(new Label("Zoom")) = c
    c.gridx = 1
    layout(new Label("Agents Active/Ready/Routing")) = c
    c.gridx = 2
    layout(new Label("Time")) = c
    c.gridx = 3
    layout(new Label("Sim Speed")) = c
    c.gridx = 4
    layout(new Label("Mode")) = c // TODO remove?
    c.gridx = 5
    c.weightx = 1.0
    c.ipadx = 0
    layout(new Label("Location")) = c

    // row 2: columns
    c.weightx = 0.0
    c.ipadx = 50
    c.gridx = 0
    c.gridy = 1
    layout(Status_Bar.zoom) = c
    c.gridx = 1
    layout(Status_Bar.agents) = c
    c.gridx = 2
    layout(Status_Bar.time) = c
    c.gridx = 3
    layout(Status_Bar.time_speed) = c
    c.gridx = 4
    layout(Status_Bar.mode) = c
    c.gridx = 5
    c.weightx = 1.0
    c.ipadx = 0
    layout(Status_Bar.location) = c
  }
}

// TODO SwingApplication has a startup, quit, shutdown...
object Viewer extends SimpleSwingApplication {
  val road_types = List(
    "null", "residential", "unclassified", "secondary",
    "motorway_link", "motorway", "trunk_link", "secondary_link", "primary_link",
    "tertiary", "primary", "service", "doomed"
  )
  // null just because it's parametric from argv
  var canvas: MapCanvas = null

  val helper = new BoxPanel(Orientation.Vertical) {
    border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)
    yLayoutAlignment = java.awt.Component.TOP_ALIGNMENT
    // TODO These're fixed now, but the idea is to tie them to configuration and
    // add/remove some context-sensitively. And also organize better.

    // Simulation controls
    contents += new Label("p   pause/resume")
    contents += new Label("[   slow down time")
    contents += new Label("]   speed up time")
    contents += new Label("-   slown down time faster")
    contents += new Label("=   speed up time faster")

    // Actions
    contents += new Label("c   choose edge for pathfinding")
    contents += new Label("d   object-sensitive debug")
    contents += new Label("f   follow agent")
    contents += new Label("x   delete agent (may crash!)")

    // Polygons
    contents += new Label("Shift+Left click   draw a polygon")
    contents += new Label("Shift+s   begin/end agents in polygon")
    contents += new Label("Shift+p   change intersection policies")

    // View
    contents += new Label("r   reset view")
    contents += new Label("w   toggle wards display")
    contents += new Label("t   toggle ward colors")
    contents += new Label("g   toggle greenflood colors")
    contents += new Label("CTRL   cycle through turns")
    contents += new Label("arrow keys pan")

    // TODO expand to fill the whole column, or otherwise work on aesthetics
    // TODO and option to hide the panel
  }

  override def main(args: Array[String]) = {
    canvas = new MapCanvas(Headless.process_args(args))
    super.main(args)
  }

  def top = new MainFrame {
    title = "AORTA"
    preferredSize = new Dimension(800, 600)
    
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Configuration") {
          popup_config
        })
        contents += new MenuItem(Action("Save scenario for later resimulation") {
          val fn = "resim_log"
          Simulation.save_log(fn)
          Dialog.showMessage(message = "Scenario saved in '" + fn + "'")
        })
        contents += new Separator
        contents += new MenuItem(Action("Quit") {
          System.exit(0)
        })
      }

      contents += new Menu("View") {
        contents += new Menu("Highlight type of road") {
          contents ++= road_types.map(t => new MenuItem(t) {
            canvas.handle_ev(EV_Param_Set("highlight", Some(t)))
          })
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          canvas.handle_ev(EV_Param_Set("highlight", None))
        })
        contents += new MenuItem(Action("Toggle wards display") {
          canvas.handle_ev(EV_Action("toggle-wards"))
        })
      }

      contents += new Menu("Query") {
        contents += new MenuItem(Action("Teleport to Edge") {
          canvas.handle_ev(EV_Action("teleport-edge"))
        })
        contents += new MenuItem(Action("Teleport to Agent") {
          canvas.handle_ev(EV_Action("teleport-agent"))
        })
        contents += new MenuItem(Action("Teleport to Vertex") {
          canvas.handle_ev(EV_Action("teleport-vertex"))
        })
        
        // TODO these are kind of toggleable...
        contents += new MenuItem(Action("Pathfind") {
          canvas.handle_ev(EV_Action("pathfind"))
        })
        contents += new MenuItem(Action("Clear Route") {
          canvas.handle_ev(EV_Action("clear-route"))
        })
      }

      contents += new Menu("Simulate") {
        //contents += new MenuItem("Spawn Agent") // TODO
        contents += new MenuItem(Action("Spawn Army") {
          canvas.handle_ev(EV_Action("spawn-army"))
        })
        contents += new MenuItem(Action("Play/Pause") {
          canvas.handle_ev(EV_Action("toggle-running"))
        })
      }
    }

    contents = new BoxPanel(Orientation.Vertical) {
      background = Color.LIGHT_GRAY
      contents += Status_Bar.panel

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += canvas
        contents += helper
      }
      border = Swing.MatteBorder(2, 2, 2, 2, Color.RED)
    }
  }

  def popup_config() = {
    // TODO tabbed pane by category?

  }
}
