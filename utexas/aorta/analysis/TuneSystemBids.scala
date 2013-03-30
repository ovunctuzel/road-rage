// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.sim.{Scenario, SystemWalletConfig}
import utexas.aorta.map.Graph

import utexas.aorta.{Util, Common, cfg}

// Use hillclimbing to tune the rates of system bids.
object TuneSystemBids {
  type State = (Int, Int, Int, Int, Int)

  // Or stop when we can't do any better
  val iters = 100

  // Cut off after how many hours?
  val run_for = 1 * 3600

  var graph: Graph = null
  var base: Scenario = null

  // Feed it the scenario to use as a metric
  def main(args: Array[String]): Unit = {
    base = Util.unserialize(args.head).asInstanceOf[Scenario]
    graph = Graph.load(base.map_fn)

    // The parameters of the SystemWalletConfig
    // (Leave avail_capacity_threshold fixed)
    var current = (1, 1, 1, 1, 1)
    var iter = 1
    var continue = true
    while (continue) {
      println(s"Iteration $iter: base $current with score ${score(current)}")
      val next = generate_neighbors(current).maxBy(state => score(state))

      iter += 1
      if (iter == iters || score(next) <= score(current)) {
        continue = false
      } else {
        current = next
      }
    }

    println(s"\nBest is $current with ${score(current)}")
  }

  private def generate_neighbors(state: State) =
    state.copy(_1 = bound(state._1 - 1)) :: state.copy(_1 = bound(state._1 + 1)) ::
    state.copy(_2 = bound(state._2 - 1)) :: state.copy(_2 = bound(state._2 + 1)) ::
    state.copy(_3 = bound(state._3 - 1)) :: state.copy(_3 = bound(state._3 + 1)) ::
    state.copy(_4 = bound(state._4 - 1)) :: state.copy(_4 = bound(state._4 + 1)) ::
    state.copy(_5 = bound(state._5 - 1)) :: state.copy(_5 = bound(state._5 + 1)) ::
    Nil
  private def bound(i: Int) = math.max(1, math.min(10, i))

  val scores = new MutableMap[State, Int]()
  private def score(state: State): Int = {
    if (scores.contains(state)) {
      return scores(state)
    } else {
      val mod = base.copy(system_wallet = SystemWalletConfig(
        thruput_bonus = state._1,
        capacity_bonus = state._2,
        dependency_rate = state._3,
        waiting_rate = state._4,
        ready_bonus = state._5
      ))
      val sim = mod.make_sim(graph)
      Common.scenario = mod
      Common.sim = sim

      // Run the simulation until its cut-off and rank it.
      var finished = 0
      try {
        println(s"  Testing $state")
        var last_time = System.currentTimeMillis
        while (sim.done == false && sim.tick < run_for) {
          sim.step(cfg.dt_s)
          val now = System.currentTimeMillis
          if (now - last_time > 1000 * 15) {
            println(s"    Currently it's ${Util.time_num(sim.tick)}")
            last_time = now
          }
        }

        // The more agents that completed, the better.
        finished = mod.agents.size - sim.agents.size - sim.ready_to_spawn.size - sim.events.size
      } catch {
        case e: Throwable => {
          println(s"  !!! Simulation of $state broke... $e")
        }
      }

      println(s"  $state had $finished agents finish")
      scores(state) = finished
      return finished
    }
  }
}
