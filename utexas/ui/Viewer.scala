package utexas.ui

import swing._  // TODO figure out exactly what
import java.awt.Color

import utexas.map.Graph

object Status_Bar {
  val zoom     = new Label("1.0") // TODO from cfg
  val agents   = new Label("0")
  val time     = new Label("0")
  val location = new Label("Nowhere")

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
  val canvas = new MapCanvas(Graph.load("dat/test.map"))

  def top = new MainFrame {
    title = "Road Rage Map Viewer"
    preferredSize = new Dimension(800, 600)
    
    menuBar = new MenuBar {
      contents += new Menu("File") {
        // It's not actually that useful to be able to do this.
        /*val osm_chooser = new FileChooser {
          fileFilter = new javax.swing.filechooser.FileNameExtensionFilter("OSM", "osm")
        }
        contents += new MenuItem(Action("Process OSM Map") {
          // TODO maybe not the best way to do this
          if (osm_chooser.showOpenDialog(this) == FileChooser.Result.Approve)
          {
            val fn = osm_chooser.selectedFile
            // TODO exec ./run a $fn, echoing STDOUT
            // then 'use source' of graph.load
          }
        })*/

        contents += new MenuItem(Action("Configuration") {
          // TODO ui_config()
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
              canvas.handle_ev(new EV_Param_Set("highlight", Some(road_type)))
            })
          }
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          canvas.handle_ev(new EV_Param_Set("highlight", None))
        })
      }

      contents += new Menu("Query") {
        contents += new MenuItem("Teleport")
        
        // TODO these are kind of toggleable...
        contents += new MenuItem("Pathfind")
        contents += new MenuItem("Clear Route")

        contents += new MenuItem("Mark Agent")
        contents += new MenuItem("Unmark Agent")
      }

      contents += new Menu("Simulate") {
        contents += new MenuItem("Spawn Agent")
        contents += new MenuItem("Spawn Army")
        contents += new MenuItem("Sacrifice to RNG God")
        contents += new MenuItem("Play/Pause")
      }
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new GridPanel(2, 4) {
        // TODO config for all the sizings...
        maximumSize = new Dimension(800, 10)
        border = Swing.MatteBorder(5, 5, 5, 5, Color.BLACK)
        contents += new Label("Zoom: ")
        contents += new Label("Agents: ")
        contents += new Label("Time: ")
        contents += new Label("At: ")
        contents += Status_Bar.zoom
        contents += Status_Bar.agents
        contents += Status_Bar.time
        contents += Status_Bar.location
      }
      contents += canvas
      border = Swing.MatteBorder(2, 2, 2, 2, Color.RED)
    }
  }
}
