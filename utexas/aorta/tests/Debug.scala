// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation
import utexas.aorta.sim.policies.SignalPolicy

import utexas.aorta.common.{Util, Common, cfg}

// Misc, temporary stuff. Maybe they'll become real tests someday.
object Debug {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args)

    get_optimal_times(args.head)
    serialize_sanity(sim)
    find_crazy_signals(sim)
    calc_capacity(sim)
  }

  private def get_optimal_times(fn: String) {
    val scenario = utexas.aorta.sim.make.Scenario.load(fn)
    val t = Common.timer("all optimal times")
    //scenario.compute_optimal_times_analytically()
    t.stop()
  }

  private def serialize_sanity(orig_sim: Simulation) = {
    // 1) Run for some time, save original
    Util.log("Running for 60 seconds...")
    orig_sim.multi_step(60.0)
    val w1 = new utexas.aorta.common.BinaryStateWriter("orig_snapshot")
    orig_sim.serialize(w1)
    w1.done

    // 2) Continue the original track
    Util.log("Continuing original sim...")
    Console.withOut(new java.io.FileOutputStream("log_orig")) {
      orig_sim.multi_step(30.0)
      val w2 = new utexas.aorta.common.BinaryStateWriter("snapshot1")
      orig_sim.serialize(w2)
      w2.done
    }

    // 4) Load the serialized one and continue it for a bit
    Util.log("Loading serialized original, continuing that...")
    val new_sim = Simulation.unserialize(new utexas.aorta.common.BinaryStateReader("orig_snapshot"))
    Console.withOut(new java.io.FileOutputStream("log_new")) {
      new_sim.multi_step(30.0)
      val w3 = new utexas.aorta.common.BinaryStateWriter("snapshot2")
      new_sim.serialize(w3)
      w3.done
    }
  }

  private def find_crazy_signals(sim: Simulation) = {
    for (v <- sim.graph.vertices) {
      v.intersection.policy match {
        case p: SignalPolicy => {
          if (p.phase_order.size > 4) {
            Util.log(s"$v has ${p.phase_order.size} phases!")
          }
        }
        case _ =>
      }
    }
  }

  private def calc_capacity(sim: Simulation) = {
    var capacity = 0
    for (r <- sim.graph.roads) {
      List(r.pos_lanes.headOption, r.neg_lanes.headOption).flatten.foreach(
        e => capacity += e.queue.capacity
      )
    }
    Util.log(s"Lower bound on total capacity: $capacity")
  }
}
