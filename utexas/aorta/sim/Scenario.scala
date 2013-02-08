// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Graph, Edge}
import utexas.aorta.map.make.PlaintextReader

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream}
import scala.collection.mutable.ArrayBuffer

import utexas.aorta.{Util, cfg}


case class Scenario(map: String, agents: Array[MkAgent],
                    intersections: Array[MkIntersection])
{
  def write(fn: String) = {
    val out = new ObjectOutputStream(new FileOutputStream(fn))
    out.writeObject(this)
    out.close
  }
}

object Scenario {
  def load(fn: String): Scenario = {
    val in = new ObjectInputStream(new FileInputStream(fn))
    val scenario = in.readObject
    in.close
    return scenario.asInstanceOf[Scenario]
  }
}

// The "Mk" prefix means "Make". These're small serializable classes to make
// agents/intersections/etc.

case class MkAgent(id: Int, birth_tick: Double, start_edge: Int,
                   start_dist: Double, route: MkRoute, wallet: MkWallet)

// TODO maybe a rng seed?
case class MkRoute(strategy: RouteStrategy.Value, goal: Integer)

// TODO other params?
case class MkWallet(policy: WalletType.Value, budget: Double)

case class MkIntersection(id: Integer, policy: IntersectionPolicy.Value,
                          ordering: IntersectionOrderingEnum.Value)

// TODO how to organize stuff like this?
object ScenarioMaker {
  // TODO separate agent creation and intersection assignment a bit
  // TODO agent distribution... time, O/D distribution, wallet params

  def default_scenario(graph: Graph, map_fn: String): Scenario = {
    Util.init_rng(System.currentTimeMillis)
    graph.edges.foreach(e => e.queue = new Queue(e))
    val start_candidates = graph.edges.filter(e => e.queue.ok_to_spawn).toArray

    // Just spawn some agents all at the start with a fixed budget
    val agents = new ArrayBuffer[MkAgent]()
    val budget = 1000.0
    for (id <- (0 until cfg.army_size)) {
      val start = Util.choose_rand[Edge](start_candidates)
      val end = Util.choose_rand[Edge](graph.edges)
      agents += MkAgent(
        id, 0.0, start.id, start.queue.safe_spawn_dist,
        MkRoute(RouteStrategy.Drunken, end.id),
        MkWallet(WalletType.Random, budget)
      )
    }

    // Assign every intersection the same policy and ordering
    val intersections = new ArrayBuffer[MkIntersection]()
    val policy = IntersectionPolicy.withName(cfg.policy)
    val ordering = IntersectionOrderingEnum.withName(cfg.ordering)
    for (v <- graph.vertices) {
      intersections += MkIntersection(v.id, policy, ordering)
    }

    return Scenario(map_fn, agents.toArray, intersections.toArray)
  }
}

object ScenarioTest {
  def main(args: Array[String]) = {
    val fn = args.head
    val map = (new PlaintextReader(fn, false)).load_map
    val scenario = ScenarioMaker.default_scenario(map, fn)
    scenario.write("tmp")
    val copy = Scenario.load("tmp")
  }
}
