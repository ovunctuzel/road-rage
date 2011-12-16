package utexas.ui

import swing._  // TODO figure out exactly what

import utexas.map.Graph

// TODO SwingApplication has a startup, quit, shutdown...
object Viewer extends SimpleSwingApplication {
  val road_types = List(
    "null", "residential", "unclassified", "secondary",
    "motorway_link", "motorway", "trunk_link", "secondary_link", "primary_link",
    "tertiary", "primary", "service"
  )

  def top = new MainFrame {
    title = "Road Rage Map Viewer"
    preferredSize = new Dimension(800, 600)
    
    menuBar = new MenuBar {
      contents += new Menu("File") {
        val osm_chooser = new FileChooser {
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
        })
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
              // highlight stuff
            })
          }
        }
        contents += new MenuItem(Action("Clear all highlighting") {
          // TODO clear stuff
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
      // TODO parametric from args
      contents += new MapCanvas(Graph.load("dat/test.map"))
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
  }
}
