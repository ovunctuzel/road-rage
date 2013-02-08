// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Graph, Edge, Vertex, DirectedRoad}
import utexas.aorta.map.make.PlaintextReader
import utexas.aorta.sim.policies._
import utexas.aorta.sim.market._

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream}
import scala.collection.mutable.ArrayBuffer

import utexas.aorta.{Util, cfg}

@SerialVersionUID(420001)
case class Scenario(map: String, agents: Array[MkAgent],
                    intersections: Array[MkIntersection])
{
  def write(fn: String) = {
    val out = new ObjectOutputStream(new FileOutputStream(fn))
    out.writeObject(this)
    out.close
  }

  def make_sim(with_geo: Boolean) =
    (new PlaintextReader(map, with_geo)).load_simulation(this)
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

@SerialVersionUID(420002)
case class MkAgent(id: Int, birth_tick: Double, start_edge: Int,
                   start_dist: Double, route: MkRoute, wallet: MkWallet)
{
  def make() = {
    // TODO worry about order...
    Agent.sim.schedule(birth_tick, make_agent)
  }

  def make_agent(): Unit = {
    val sim = Agent.sim
    val a = new Agent(id, route.make(sim), wallet)
    sim.ready_to_spawn += new SpawnAgent(a, sim.edges(start_edge), start_dist)
  }
}

// TODO maybe a rng seed?
@SerialVersionUID(420003)
case class MkRoute(strategy: RouteType.Value, goal: Integer) {
  def make(sim: Simulation) =
    Factory.make_route(strategy, sim.edges(goal).directed_road)
}

// TODO other params?
@SerialVersionUID(420004)
case class MkWallet(policy: WalletType.Value, budget: Double) {
  def make(a: Agent) = Factory.make_wallet(a, policy, budget)
}

@SerialVersionUID(420005)
case class MkIntersection(id: Integer, policy: IntersectionType.Value,
                          ordering: OrderingType.Value)
{
  def make(v: Vertex) = new Intersection(v, policy, ordering)
}

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
        MkRoute(RouteType.Drunken, end.id),
        MkWallet(WalletType.Random, budget)
      )
    }

    // Assign every intersection the same policy and ordering
    val intersections = new ArrayBuffer[MkIntersection]()
    val policy = IntersectionType.withName(cfg.policy)
    val ordering = OrderingType.withName(cfg.ordering)
    for (v <- graph.vertices) {
      intersections += MkIntersection(v.id, policy, ordering)
    }

    return Scenario(map_fn, agents.toArray, intersections.toArray)
  }
}

// TODO rm
object ScenarioTest {
  def main(args: Array[String]) = {
    val fn = args.head
    val map = (new PlaintextReader(fn, false)).load_map
    val scenario = ScenarioMaker.default_scenario(map, fn)
    scenario.write("tmp")
    val copy = Scenario.load("tmp")
  }
}

object IntersectionType extends Enumeration {
  type IntersectionType = Value
  val NeverGo, StopSign, Signal, Reservation = Value
}

object RouteType extends Enumeration {
  type RouteType = Value
  val StaticAstar, Drunken, DirectionalDrunk, DrunkenExplorer = Value
}

object OrderingType extends Enumeration {
  type OrderingType = Value
  val FIFO, Auction = Value
}

object WalletType extends Enumeration {
  type WalletType = Value
  val Random, Emergency, Freerider = Value
}                                                                         

object Factory {
  def make_policy(i: Intersection, policy: IntersectionType.Value,
                  ordering: OrderingType.Value) = policy match
  { 
    case IntersectionType.NeverGo =>
      new NeverGoPolicy(i)
    case IntersectionType.StopSign =>
      new StopSignPolicy(i, make_intersection_ordering[Ticket](ordering))
    case IntersectionType.Signal =>
      new SignalPolicy(i, make_intersection_ordering[Phase](ordering))
    case IntersectionType.Reservation =>
      new ReservationPolicy(i, make_intersection_ordering[TurnBatch](ordering))
  }
  
  def make_route(enum: RouteType.Value, goal: DirectedRoad) = enum match {
    case RouteType.StaticAstar => new StaticRoute(goal)
    case RouteType.Drunken => new DrunkenRoute(goal)
    case RouteType.DirectionalDrunk => new DirectionalDrunkRoute(goal)
    case RouteType.DrunkenExplorer => new DrunkenExplorerRoute(goal)
  }
  
  def make_intersection_ordering[T](enum: OrderingType.Value) = enum match
  { 
    case OrderingType.FIFO => new FIFO_Ordering[T]()            
    case OrderingType.Auction => new AuctionOrdering[T]()       
  }

  def make_wallet(a: Agent, enum: WalletType.Value, budget: Double) = enum match {
    case WalletType.Random => new RandomWallet(a, budget)
    // TODO budget is misnomer for emergency
    case WalletType.Emergency => new EmergencyVehicleWallet(a, budget)    
    case WalletType.Freerider => new FreeriderWallet(a)
  }
}
