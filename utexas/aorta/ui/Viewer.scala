// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.ui

import swing._  // TODO figure out exactly what
import java.awt.{Color, Component}
import swing.Dialog
import info.monitorenter.gui.chart.Chart2D
import info.monitorenter.gui.chart.traces.Trace2DLtd
import javax.media.j3d.Canvas3D

import utexas.aorta.sim.Simulation
import utexas.aorta.{Util, cfg}

object Status_Bar {
  val zoom       = new Label("1.0") // TODO from cfg
  val agents     = new Label("0 / 0 / 0 (0 generators)")
  val time       = new Label("0.0 [Paused]")
  val sim_speed = new Label("1.0x / 1.0x")

  def update_speed(sim: Simulation) = {
    sim_speed.text = "%.1fx / %.1fx".format(
      sim.actual_sim_speed, sim.desired_sim_speed
    )
  }

  val panel = new GridBagPanel {
    // TODO config for all the sizings...
    maximumSize = new Dimension(Int.MaxValue, 10)
    border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)

    // TODO generate these?

    // all of this to prevent the rightmost 'At' column from spazzing out when
    // the text changes length
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
    layout(new Label("Sim Speed (Actual/Desired)")) = c

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
    layout(Status_Bar.sim_speed) = c
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
  var canvas_2d: MapCanvas = null
  var canvas_3d: MapCanvas3D = null

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
    contents += new Label("m   make new agent on current edge")
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

  val chart = new Chart2D
  // Remember enough points for the last 2 minutes
  // TODO tweak more chart stuff
  val chart_data = new Trace2DLtd((120.0 / cfg.dt_s).toInt)
  chart_data.setColor(Color.RED)
  chart.addTrace(chart_data)
  chart.setMinPaintLatency(1000)  // only one redraw per second
  chart.getAxisX.getAxisTitle.setTitle("Time (s)")
  chart.getAxisY.getAxisTitle.setTitle("Number of agents")

  override def main(args: Array[String]) = {
    val sim = Util.process_args(args)
    canvas_2d = new MapCanvas(sim)
    canvas_3d = new MapCanvas3D(sim)
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
        contents += new MenuItem(
          Action("Save scenario for later resimulation")
        {
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
            canvas_2d.handle_ev(EV_Param_Set("highlight", Some(t)))
          })
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          canvas_2d.handle_ev(EV_Param_Set("highlight", None))
        })
        contents += new MenuItem(Action("Toggle wards display") {
          canvas_2d.handle_ev(EV_Action("toggle-wards"))
        })
      }

      contents += new Menu("Query") {
        contents += new MenuItem(Action("Teleport to Edge") {
          canvas_2d.handle_ev(EV_Action("teleport-edge"))
        })
        contents += new MenuItem(Action("Teleport to Road") {
          canvas_2d.handle_ev(EV_Action("teleport-road"))
        })
        contents += new MenuItem(Action("Teleport to Agent") {
          canvas_2d.handle_ev(EV_Action("teleport-agent"))
        })
        contents += new MenuItem(Action("Teleport to Vertex") {
          canvas_2d.handle_ev(EV_Action("teleport-vertex"))
        })
        
        // TODO these are kind of toggleable...
        contents += new MenuItem(Action("Pathfind") {
          canvas_2d.handle_ev(EV_Action("pathfind"))
        })
        contents += new MenuItem(Action("Clear Route") {
          canvas_2d.handle_ev(EV_Action("clear-route"))
        })
      }

      contents += new Menu("Simulate") {
        //contents += new MenuItem("Spawn Agent") // TODO
        contents += new MenuItem(Action("Spawn Army") {
          canvas_2d.handle_ev(EV_Action("spawn-army"))
        })
        contents += new MenuItem(Action("Play/Pause") {
          canvas_2d.handle_ev(EV_Action("toggle-running"))
        })
      }
    }

    // TODO toggle between helper and other stuff in right pane
    val main_content = new SplitPane(
      Orientation.Vertical,
      new scala.swing.Component {
        override lazy val peer = new javax.swing.JComponent {
          add(canvas_3d.canvas)
        }
      },
      //canvas_2d,
      new scala.swing.Component {
        override lazy val peer = chart
      }
    )

    contents = new BorderPanel {
      background = Color.LIGHT_GRAY
      border = Swing.MatteBorder(2, 2, 2, 2, Color.RED)

      add(Status_Bar.panel, BorderPanel.Position.North)
      add(main_content, BorderPanel.Position.Center)
    }
  }

  def popup_config() = {
    // TODO tabbed pane by category?
  }
}
