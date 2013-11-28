// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.{Simulation, EV_Heartbeat}
import utexas.aorta.common.IO

class SimSpeedMonitor(sim: Simulation, fn: String) {
  private val freq_ms = 1000

  // Don't have to explicitly close the file when simulation finishes
  // TODO fn library. denote what we're running on.
  private val file = new IO(None).output_file("sim_speed_" + fn)
  file.println("realtime_sec sim_tick")
  private val start = System.currentTimeMillis
  private var last_record = start

  sim.listen("sim_speed", _ match {
    case EV_Heartbeat(info) => {
      val now = System.currentTimeMillis
      if (now - last_record >= freq_ms) {
        record((now - start) / 1000, info.tick)
        last_record = now
      }
    }
    case _ =>
  })

  private def record(realtime: Long, tick: Double) {
    file.println(s"$realtime $tick")
  }
}
