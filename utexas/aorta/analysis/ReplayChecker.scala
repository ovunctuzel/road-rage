// AORTA is copyright (C) 2013 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.Simulation
import utexas.aorta.{StateReader, StateWriter, Util}

import java.io.File

// TODO better name
// Record what every agent is doing at every timestep, so that when we
// re-simulate something with slight tweaks, we can detect exactly who and what
// starts deviating.
abstract class ReplayChecker(sim: Simulation) {
  def difference(id: Int, expect: Double, actual: Double)

  // What're we doing?
  private var reader: StateReader = null
  private var writer: StateWriter = null
  private val fn = sim.scenario.name.replace("scenarios/", "scenarios/replay_")
  if (new File(fn).exists) {
    reader = Util.reader(fn)
  } else {
    writer = Util.writer(fn)
  }

  private val agents = sim.scenario.agents.size

  def handle_tick() {
    if (reader != null) {
      replay_tick()
    } else {
      record_tick()
    }
  }

  // TODO writing an extra, i think...
  def record_tick() {
    // We get agents in sorted order, so we can detect the ones who aren't
    // around yet or have gone.
    var expect_id = 0
    for (a <- sim.agents) {
      // [expect_id, a.id)
      for (id <- Range(expect_id, a.id)) {
        writer.double(ReplayChecker.MISSING)
      }
      writer.double(a.characterize_choice)
      expect_id = a.id + 1
    }
    // TODO theres a range with inclusive upper bounds...
    for (id <- Range(expect_id, agents + 1)) {
      writer.double(ReplayChecker.MISSING)
    }
  }

  def replay_tick() {
    // TODO fire another callback when we're simulating past where we stopped
    // last time
    var expect_id = 0
    for (a <- sim.agents) {
      // [expect_id, a.id)
      for (id <- Range(expect_id, a.id)) {
        val expect = reader.double
        if (expect != ReplayChecker.MISSING) {
          difference(id, expect, ReplayChecker.MISSING)
        }
      }
      val expect = reader.double
      expect_id = a.id + 1
      if (expect != a.characterize_choice) {
        difference(a.id, expect, a.characterize_choice)
      }
    }
    for (id <- Range(expect_id, agents + 1)) {
      val expect = reader.double
      if (expect != ReplayChecker.MISSING) {
        difference(id, expect, ReplayChecker.MISSING)
      }
    }
  }

  def done() {
    if (writer != null) {
      writer.done()
    }
  }
}

object ReplayChecker {
  val MISSING = -1.0
}
