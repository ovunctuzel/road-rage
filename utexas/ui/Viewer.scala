package utexas.ui

import swing._  // TODO figure out exactly what
import java.awt.Color

import utexas.sim.Simulation
import utexas.Util

object Status_Bar {
  val zoom       = new Label("1.0") // TODO from cfg
  val agents     = new Label("0")
  val time       = new Label("0.0 [Paused]")
  val time_speed = new Label("1.0x")
  val location   = new Label("Nowhere")
  val mode       = new Label("" + Mode.EXPLORE)

  // TODO could put methods here to set text!
}

// TODO SwingApplication has a startup, quit, shutdown...
object Viewer extends SimpleSwingApplication {
  val road_types = List(
    "null", "residential", "unclassified", "secondary",
    "motorway_link", "motorway", "trunk_link", "secondary_link", "primary_link",
    "tertiary", "primary", "service", "doomed"
  )
  // TODO parametric from args
  val canvas = new MapCanvas(Simulation.load("dat/test.map"))

  override def main(args: Array[String]) = {
    if (args.size == 1) {
      Util.init_rng(args.head.toLong)
    } else {
      Util.init_rng(System.currentTimeMillis)
    }
    super.main(args)
  }

  def top = new MainFrame {
    title = "Road Rage Map Viewer"
    preferredSize = new Dimension(800, 600)
    
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Configuration") {
          popup_config
        })
        contents += new Separator
        contents += new MenuItem(Action("Quit") {
          System.exit(0)
        })
      }

      contents += new Menu("View") {
        contents += new Menu("Highlight type of road") {
          // TODO write this more functionally
          for (road_type <- road_types) {
            contents += new MenuItem(Action(road_type) {
              canvas.handle_ev(EV_Param_Set("highlight", Some(road_type)))
            })
          }
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          canvas.handle_ev(EV_Param_Set("highlight", None))
        })
        contents += new MenuItem(Action("Toggle wards display") {
          canvas.handle_ev(EV_Action("toggle-wards"))
        })
      }

      contents += new Menu("Query") {
        contents += new MenuItem("Teleport")  // TODO
        
        // TODO these are kind of toggleable...
        contents += new MenuItem(Action("Pathfind") {
          canvas.handle_ev(EV_Action("pathfind"))
        })
        contents += new MenuItem(Action("Clear Route") {
          canvas.handle_ev(EV_Action("clear-route"))
        })

        contents += new MenuItem("Mark Agent")  // TODO
        contents += new MenuItem("Unmark Agent")
      }

      contents += new Menu("Simulate") {
        contents += new MenuItem("Spawn Agent") // TODO
        contents += new MenuItem(Action("Spawn Army") {
          canvas.handle_ev(EV_Action("spawn-army"))
        })
        contents += new MenuItem("Sacrifice to RNG God")  // TODO
        contents += new MenuItem(Action("Play/Pause") {
          canvas.handle_ev(EV_Action("toggle-running"))
        })
      }
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new GridBagPanel {
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
        layout(new Label("Agents")) = c
        c.gridx = 2
        layout(new Label("Time")) = c
        c.gridx = 3
        layout(new Label("Sim Speed")) = c
        c.gridx = 4
        layout(new Label("Mode")) = c
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
      contents += canvas
      border = Swing.MatteBorder(2, 2, 2, 2, Color.RED)
    }
  }

  def popup_config() = {
    // TODO tabbed pane by category?

  }
}
