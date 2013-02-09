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

import utexas.aorta.{Util, RNG, cfg}

@SerialVersionUID(1)
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
// TODO associate directly with their corresponding class?

@SerialVersionUID(1)
case class MkAgent(id: Int, birth_tick: Double, seed: Long, start_edge: Int,
                   start_dist: Double, route: MkRoute, wallet: MkWallet)
{
  def make() = {
    // TODO worry about order...
    Agent.sim.schedule(birth_tick, make_agent)
  }

  def make_agent(): Unit = {
    val sim = Agent.sim
    val a = new Agent(id, route.make(sim), new RNG(seed), wallet)
    sim.ready_to_spawn += new SpawnAgent(a, sim.edges(start_edge), start_dist)
  }
}

@SerialVersionUID(1)
case class MkRoute(strategy: RouteType.Value, goal: Integer, seed: Long) {
  def make(sim: Simulation) =
    Factory.make_route(strategy, sim.edges(goal).directed_road, new RNG(seed))
}

// TODO other params?
@SerialVersionUID(1)
case class MkWallet(policy: WalletType.Value, budget: Double) {
  def make(a: Agent) = Factory.make_wallet(a, policy, budget)
}

@SerialVersionUID(1)
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
    val rng = new RNG()

    graph.edges.foreach(e => e.queue = new Queue(e))
    val start_candidates = graph.edges.filter(e => e.queue.ok_to_spawn).toArray

    // Just spawn some agents all at the start with a fixed budget
    val agents = new ArrayBuffer[MkAgent]()
    val budget = 1000.0
    for (id <- (0 until cfg.army_size)) {
      val start = rng.choose_rand[Edge](start_candidates)
      val end = rng.choose_rand[Edge](graph.edges)
      agents += MkAgent(
        id, 0.0, rng.new_seed, start.id, start.queue.safe_spawn_dist(rng),
        MkRoute(RouteType.Drunken, end.id, rng.new_seed),
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
  
  def make_route(enum: RouteType.Value, goal: DirectedRoad, rng: RNG) = enum match {
    case RouteType.StaticAstar => new StaticRoute(goal, rng)
    case RouteType.Drunken => new DrunkenRoute(goal, rng)
    case RouteType.DirectionalDrunk => new DirectionalDrunkRoute(goal, rng)
    case RouteType.DrunkenExplorer => new DrunkenExplorerRoute(goal, rng)
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
