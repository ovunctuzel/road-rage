// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import java.io.File

import utexas.aorta.ui.{MapCanvas, Viewer, EV_Action}

import utexas.aorta.{Util, Common, cfg}

object Headless {
  def main(args: Array[String]): Unit = {
    val sim = Util.process_args(args)

    // When this file exists, launch a GUI for sudden interactive watching.
    val gui_signal = new File(".headless_gui")
    var gui: Option[MapCanvas] = None

    // Print an update every second
    var last_tick = sim.tick
    sim.listen("headless", (ev: Sim_Event) => { ev match {
      case EV_Heartbeat(info) => {
        Util.log("At t=%s (%.1fx): %s {%s moves, %s CH, %s A*}".format(
          Util.time_num(info.tick), info.tick - last_tick, info.describe,
          Util.comma_num(info.agent_steps, pad = true),
          Util.comma_num(info.ch_paths), Util.comma_num(info.astar_paths)
        ))
        last_tick = info.tick

        if (gui_signal.exists) {
          gui_signal.delete
          gui match {
            case Some(ui) => {
              if (Viewer.closed) {
                println("Resuming the GUI...")
                Viewer.top.open
                Viewer.closed = false
              }
            }
            case None => {
              println("Launching the GUI...")
              gui = Some(new MapCanvas(sim, headless = true))
              Viewer.launch_from_headless(gui.get)
            }
          }
        }
        gui match {
          case Some(ui) if !Viewer.closed => ui.handle_ev(EV_Action("step"))
          case _ =>
        }
      }
      case _ =>
    } })

    Util.log("Starting simulation with time-steps of " + cfg.dt_s + "s")
    val t = Common.timer("headless simulation")
    while (!sim.done) {
      sim.step(cfg.dt_s)
    }
    t.stop
    sys.exit
  }
}
