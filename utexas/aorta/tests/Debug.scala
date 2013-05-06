// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.Simulation
import utexas.aorta.sim.policies.SignalPolicy

import utexas.aorta.{Util, Common, cfg}

// Misc, temporary stuff
// TODO slowly it's stealing from tests.MapSuite! :P
object Debug {
  def main(args: Array[String]) = {
    val sim = Util.process_args(args)

    serialize_sanity(sim)
    /*find_short_edges(sim)
    find_disconnected_verts(sim)
    find_crazy_signals(sim)
    calc_capacity(sim)
    degenerate_verts(sim)
    doomed_stuff(sim)
    disconnected_directed_roads(sim)
    stress_test_pathfind(sim)*/
  }

  private def serialize_sanity(orig_sim: Simulation) = {
    // 1) Run for some time
    Util.log("Running for 60 seconds...")
    orig_sim.step(60.0)
    // 2) Serialize
    Util.log("Serializing original, continuing it...")
    val w1 = new utexas.aorta.BinaryStateWriter("orig_snapshot")
    orig_sim.serialize(w1)
    w1.done
    // 3) Continue the orig for a bit
    orig_sim.step(30.0)
    // 4) Load the serialized one and continue it for a bit
    // TODO make sure common.sim fine!
    Util.log("Loading original, continuing that...")
    val new_sim = Simulation.unserialize(new utexas.aorta.BinaryStateReader("orig_snapshot"))
    new_sim.step(30.0)
    // 5) Serialize both again, then compare.
    val w2 = new utexas.aorta.BinaryStateWriter("snapshot1")
    orig_sim.serialize(w2)
    w2.done
    val w3 = new utexas.aorta.BinaryStateWriter("snapshot2")
    new_sim.serialize(w3)
    w3.done
  }

  private def find_short_edges(sim: Simulation) = {
    for (e <- sim.graph.edges) {
      if (e.length <= 1.0 || e.length.isNaN) {
        Util.log(s"$e has length ${e.length}. road ${e.road.length}")
      }
    }
  }

  private def find_disconnected_verts(sim: Simulation) = {
    for (v <- sim.graph.vertices) {
      // Make sure every road is reachable from every other, except one-ways
      var missing = false
      for (r1 <- v.roads if (!r1.is_oneway || r1.v2 == v)) {
        for (r2 <- v.roads if r1 != r2 && (!r2.is_oneway || r2.v1 == v)) {
          if (!missing) {
            if (!v.turns.find(t => t.from.road == r1 && t.to.road == r2).isDefined) {
              Util.log(s"Roads at $v aren't fully connected... $r1 -> $r2 not there!")
              missing = true
            }
          }
        }
      }
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

  private def degenerate_verts(sim: Simulation) = {
    val silly = sim.vertices.filter(v => v.roads.size == 2)
    silly.foreach(v => println("Degenerate vertex: " + v))
    println("")
    println(silly.size + " total")
  }

  private def doomed_stuff(sim: Simulation) = {
    println(sim.roads.filter(_.doomed).size + " doomed roads")
    println(sim.edges.filter(_.doomed).size + " doomed edges")
  }

  private def disconnected_directed_roads(sim: Simulation) = {
    for (r <- sim.graph.directed_roads) {
      if (r.succs.isEmpty || r.preds.isEmpty) {
        println(s"$r has succs ${r.succs} and preds ${r.preds}. Big road oneway? ${r.road.is_oneway}")
      }
    }
  }

  private def stress_test_pathfind(sim: Simulation) = {
    val rng = new utexas.aorta.RNG()
    // CH (if available), or Dijkstra's
    val router = sim.graph.router
    val t = Common.timer("routing")

    for (i <- 1 until 500000) {
      if (i % 1000 == 0) {
        println(s"round $i")
      }
      val from = rng.choose(sim.graph.directed_roads)
      val to = rng.choose(sim.graph.directed_roads)
      try {
        router.path(from, to)
      } catch {
        case e: Throwable => {
          println(s"Problem on round $i going $from -> $to: $e")
        }
      }
      // TODO verify path
    }
    t.stop
  }
}
