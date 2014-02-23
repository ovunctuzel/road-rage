// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import javax.script.{ScriptEngineManager, ScriptException}
import java.io.File
import jline.console.ConsoleReader

import utexas.aorta.sim.{Simulation, EV_Heartbeat}

class REPL(sim: Simulation) {
  private val e = new ScriptEngineManager().getEngineByName("scala")
  private val reader = new ConsoleReader()
  reader.setPrompt("> ")
  private var first_time = true

  def run() {
    if (first_time) {
      e.put("raw_sim_obj", sim)
      e.eval("val sim = raw_sim_obj.asInstanceOf[utexas.aorta.sim.Simulation]")
      first_time = false
    }
    println("Type 'quit' to exit the REPL. The main simulation object is bound to 'sim'.")

    while (true) {
      val input = reader.readLine()
      if (input == "quit") {
        return
      }
      try {
        val result = e.eval(input)
        if (result != null) {
          println(result)
        }
      } catch {
        case e: ScriptException => // the error message already gets printed
      }
    }
  }
}

class REPLDebugger(sim: Simulation) {
  // When this file exists, launch the REPL
  private val signal = new File(".headless_repl")
  private val repl = new REPL(sim)

  sim.listen(classOf[EV_Heartbeat], _ match { case e: EV_Heartbeat => {
    if (signal.exists) {
      signal.delete()
      repl.run()
    }
  }})
}
