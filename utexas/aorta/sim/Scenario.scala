// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Graph, Edge, Vertex, DirectedRoad}
import utexas.aorta.sim.policies._
import utexas.aorta.sim.market._

import java.io.File
import scala.collection.mutable.ArrayBuffer

import utexas.aorta.{Util, RNG, Common, cfg}

@SerialVersionUID(1)
case class Scenario(map_fn: String, agents: Array[MkAgent],
               intersections: Array[MkIntersection])
{
  def make_sim(graph: Graph = Graph.load(map_fn)) = new Simulation(graph, this)
  def save(fn: String) = Util.serialize(this, fn)

  def make_intersection(v: Vertex) = intersections(v.id).make(v)
  def make_agents() = agents.foreach(a => a.make)
}

object Scenario {
  def load(fn: String) = Util.unserialize(fn).asInstanceOf[Scenario]

  def default(map_fn: String, graph: Graph): Scenario = {
    val s = Scenario(
      map_fn,
      AgentDistribution.default(graph),
      IntersectionDistribution.default(graph)
    )
    // Always save it, so resimulation is easy.
    (new File("./scenarios")).mkdir
    s.save(s"scenarios/default_${graph.name}")
    return s
  }
}

// The "Mk" prefix means "Make". These're small serializable classes to make
// agents/intersections/etc.
// TODO associate directly with their corresponding class?

abstract class MkAgent() {
  def make(): Unit
}

@SerialVersionUID(1)
case class MkSingleAgent(id: Int, birth_tick: Double, seed: Long,
                         start_edge: Int, start_dist: Double, route: MkRoute,
                         wallet: MkWallet)
  extends MkAgent
{
  def make() = {
    // TODO worry about order...
    Common.sim.schedule(birth_tick, make_agent)
  }

  private def make_agent(): Unit = {
    val sim = Common.sim
    val a = new Agent(id, route.make(sim), new RNG(seed), wallet)
    sim.ready_to_spawn += new SpawnAgent(a, sim.edges(start_edge), start_dist)
  }
}

// Spawns some distribution of agents every frequency seconds.
/*@SerialVersionUID(1)
case class MkAgentSpawner(frequency: Double, expires: Double, seed: Long)
  extends MkAgent
{
  def make() = {
    spawn
  }

  private def spawn(): Unit = {
    val sim = Common.sim
    if (sim.tick < expires) {
      // Re-schedule ourselves
      sim.schedule(sim.tick + frequency, spawn)
    }

    // TODO do something interesting
  }
}*/

@SerialVersionUID(1)
case class MkRoute(strategy: RouteType.Value, goal: Integer, seed: Long) {
  def make(sim: Simulation) =
    Factory.make_route(strategy, sim.edges(goal).directed_road, new RNG(seed))
}

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

object IntersectionDistribution {
  private val rng = new RNG()

  val all_policies = IntersectionType.values.toArray
  val all_orderings = OrderingType.values.toArray
  lazy val default_policy = IntersectionType.withName(cfg.policy)
  lazy val default_ordering = OrderingType.withName(cfg.ordering)

  // TODO specify "80% x, 20% y" for stuff...
  def uniform(graph: Graph, policies: Array[IntersectionType.Value],
              orderings: Array[OrderingType.Value]) =
    graph.vertices.map(v => MkIntersection(
      v.id, rng.choose(policies), rng.choose(orderings)
    ))

  def default(graph: Graph) = uniform(
    graph, Array(default_policy), Array(default_ordering)
  )

  // TODO realistic assignment, with signs at small crossings, and signals for
  // heavy direction of big crossings
}

object AgentDistribution {
  // TODO share RNGs so the cmdline tool can be parametrized by one random
  // thing?
  private val rng = new RNG()

  val all_routes = RouteType.values.toArray
  val all_wallets = WalletType.values.toArray
  lazy val default_route = RouteType.withName(cfg.route)
  lazy val default_wallet = WalletType.withName(cfg.wallet)

  def filter_candidates(starts: Array[Edge]) = starts.filter(_.ok_to_spawn)

  // TODO specify "80% x, 20% y" for stuff...
  def uniform(ids: Range, starts: Array[Edge], ends: Array[Edge],
              times: (Double, Double), routes: Array[RouteType.Value], 
              wallets: Array[WalletType.Value],
              budgets: (Double, Double)): Array[MkAgent] =
  {
    val actual_starts = filter_candidates(starts)
    return ids.map(id => {
      val start = rng.choose(actual_starts)
      MkSingleAgent(
        id, rng.double(times._1, times._2), rng.new_seed, start.id,
        start.safe_spawn_dist(rng),
        MkRoute(rng.choose(routes), rng.choose(ends).id, rng.new_seed),
        MkWallet(rng.choose(wallets), rng.double(budgets._1, budgets._2))
      )
    }).toArray
  }

  def default(graph: Graph) = uniform(
    Range(0, cfg.army_size), graph.edges, graph.edges, (0.0, 60.0),
    Array(default_route), Array(default_wallet), (100.0, 1000.0)
  )
}

// Enumeration stuff

object IntersectionType extends Enumeration {
  type IntersectionType = Value
  val NeverGo, StopSign, Signal, Reservation = Value
}

object RouteType extends Enumeration {
  type RouteType = Value
  val Dijkstra, Path, Drunken, DirectionalDrunk, DrunkenExplorer = Value
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
    case RouteType.Dijkstra => new DijkstraRoute(goal, rng)
    case RouteType.Path => new PathRoute(goal, rng)
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
