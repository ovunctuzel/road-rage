// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.make

import utexas.aorta.map._
import utexas.aorta.sim._
import utexas.aorta.sim.drivers._
import utexas.aorta.sim.routes._
import utexas.aorta.sim.intersections._

import Function.tupled
import scala.collection.mutable

import utexas.aorta.common.{Util, StateWriter, StateReader, AgentID, VertexID, RoadID, cfg}

// Array index and agent/intersection ID must correspond. Creator's
// responsibility.
case class Scenario(
  name: String, map_fn: String, agents: Array[MkAgent], intersections: Array[MkIntersection],
  system_wallet: SystemWalletConfig, auditor: CongestionType.Value
) {
  def graph = Graph.load(map_fn)
  def make_sim() = new Simulation(this)
  def save() {
    val w = Util.writer(name)
    serialize(w)
    w.done()
  }

  def make_intersection(v: Vertex, sim: Simulation) = intersections(v.id.int).make(v, sim)
  def make_auditor(r: Road, sim: Simulation) = Factory.make_auditor(auditor, r, sim)

  // Although the massive numbers of agents were probably created with a
  // distribution function originally, we just see the individual list in the
  // end. So compute basic stats about it.
  def summarize() {
    Util.log(s"Scenario $name for $map_fn\n")
    // TODO breakdown combos of policy/ordering, and wallet/budget
    Util.log("Intersection policies:")
    percentages(intersections.map(_.policy))
    Util.log("Intersection orderings:")
    percentages(intersections.map(_.ordering))
    Util.log(s"Link auditor: $auditor")
    Util.log("")

    Util.log(s"${agents.size} agents total")
    // TODO where are agents starting/going?
    if (agents.nonEmpty) {
      Util.log_push
      Util.log("Spawning time (s): " + basic_stats(agents.map(_.birth_tick)))
      Util.log("Routes:")
      percentages(agents.map(_.route.strategy))
      Util.log("Orig routers:")
      percentages(agents.map(_.route.orig_router))
      Util.log("Rerouters:")
      percentages(agents.map(_.route.rerouter))
      Util.log("Wallets:")
      percentages(agents.map(_.wallet.policy))
      Util.log("Budget ($): " + basic_stats_int(agents.map(_.wallet.budget)))
      Util.log("Priority: " + basic_stats_int(agents.map(_.wallet.priority)))
      Util.log_pop
    }

    Util.log("System wallet rates:")
    Util.log(system_wallet.toString)
  }

  // Describe the percentages of each thing
  private def percentages[T](things: Iterable[T]) = {
    val count = new mutable.HashMap[T, Int]().withDefaultValue(0)
    things.foreach(t => count(t) += 1)
    val total = things.size
    Util.log_push
    for (key <- count.keys) {
      val percent = count(key).toDouble / total * 100.0
      Util.log(f"$key: ${percent}%.2f%%")
    }
    Util.log_pop
  }

  // Min, max, average
  private def basic_stats(nums: Iterable[Double]) =
    f"${nums.min}%.2f - ${nums.max}%.2f (average ${nums.sum / nums.size}%.2f)"
  private def basic_stats_int(nums: Iterable[Int]) =
    f"${nums.min} - ${nums.max} (average ${nums.sum / nums.size}%.2f)"

  def diff(other: Scenario): Unit = {
    if (map_fn != other.map_fn) {
      Util.log(s"Scenarios are for different maps: $map_fn and ${other.map_fn}")
      return
    }
    Util.diff(auditor, other.auditor, "link auditor") match {
      case Some(d) => Util.log(d)
      case None =>
    }
    intersections.zip(other.intersections).foreach(tupled((i1, i2) => i1.diff(i2)))
    agents.zip(other.agents).foreach(tupled((a1, a2) => a1.diff(a2)))
    if (agents.size != other.agents.size) {
      Util.log(s"Scenarios have different numbers of agents: ${agents.size} and ${other.agents.size}")
    }

    // TODO diff SystemWalletConfig
  }

  def serialize(w: StateWriter) {
    w.string(name)
    w.string(map_fn)
    w.int(agents.size)
    agents.foreach(a => a.serialize(w))
    w.int(intersections.size)
    intersections.foreach(i => i.serialize(w))
    system_wallet.serialize(w)
    w.int(auditor.id)
  }

  def num_agents = agents.size
}

object Scenario {
  def unserialize(r: StateReader) = Scenario(
    r.string, r.string,
    Range(0, r.int).map(_ => MkAgent.unserialize(r)).toArray,
    Range(0, r.int).map(_ => MkIntersection.unserialize(r)).toArray,
    SystemWalletConfig.unserialize(r), CongestionType(r.int)
  )

  def load(fn: String) = unserialize(Util.reader(fn))

  def default(map_fn: String): Scenario = {
    val graph = Graph.load(map_fn)
    val s = Scenario(
      s"scenarios/default_${graph.name}",
      map_fn,
      AgentDistribution.default(graph),
      IntersectionDistribution.default(graph),
      SystemWalletConfig(),
      CongestionType.withName(cfg.auditor)
    )
    // Always save it, so resimulation is easy.
    Util.mkdir("scenarios")
    s.save()
    return s
  }
}

// The "Mk" prefix means "Make". These're small serializable classes to make
// agents/intersections/etc.
// TODO associate directly with their corresponding class?

case class MkAgent(id: AgentID, birth_tick: Double, start: RoadID, start_dist: Double,
                   route: MkRoute, wallet: MkWallet) extends Ordered[MkAgent]
{
  // break ties by ID
  def compare(other: MkAgent) =
    implicitly[Ordering[Tuple2[Double, Integer]]].compare(
      (other.birth_tick, other.id.int), (birth_tick, id.int)
    )

  def make(sim: Simulation) = new Agent(id, route.make(sim), wallet.make, sim)

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.double(birth_tick)
    w.int(start.int)
    w.double(start_dist)
    route.serialize(w)
    wallet.serialize(w)
  }

  def diff(other: MkAgent) = {
    Util.assert_eq(id, other.id)
    val d = List(
      Util.diff(birth_tick, other.birth_tick, "spawn time"),
      Util.diff(start, other.start, "start"),
      Util.diff(start_dist, other.start_dist, "start distance"),
      Util.diff(route.goal, other.route.goal, "end"),
      Util.diff(route.strategy, other.route.strategy, "route"),
      Util.diff(route.orig_router, other.route.orig_router, "orig_router"),
      Util.diff(route.rerouter, other.route.rerouter, "rerouter"),
      Util.diff(wallet.policy, other.wallet.policy, "wallet"),
      Util.diff(wallet.budget, other.wallet.budget, "budget"),
      Util.diff(wallet.priority, other.wallet.priority, "priority")
    ).flatten.mkString(", ")
    if (d.nonEmpty) {
      Util.log(s"Agent $id different: $d")
    }
  }
}

object MkAgent {
  def unserialize(r: StateReader) = MkAgent(
    new AgentID(r.int), r.double, new RoadID(r.int), r.double,
    MkRoute.unserialize(r), MkWallet.unserialize(r)
  )
}

// orig_router, rerouter, and initial_path are only for strategy = Path
case class MkRoute(
  strategy: RouteType.Value, orig_router: RouterType.Value, rerouter: RouterType.Value,
  initial_path: List[RoadID], goal: RoadID
) {
  def make(sim: Simulation) = Factory.make_route(
    strategy, sim.graph, orig_router, rerouter, sim.graph.get_r(goal),
    initial_path.map(id => sim.graph.get_r(id))
  )

  def serialize(w: StateWriter) {
    w.int(strategy.id)
    w.int(orig_router.id)
    w.int(rerouter.id)
    w.int(initial_path.size)
    initial_path.foreach(id => w.int(id.int))
    w.int(goal.int)
  }
}

object MkRoute {
  def unserialize(r: StateReader) = MkRoute(
    RouteType(r.int), RouterType(r.int), RouterType(r.int),
    Range(0, r.int).map(_ => new RoadID(r.int)).toList, new RoadID(r.int)
  )
}

case class MkWallet(policy: WalletType.Value, budget: Int, priority: Int, bid_ahead: Boolean) {
  def make() = Factory.make_wallet(policy, budget, priority, bid_ahead)

  def serialize(w: StateWriter) {
    w.int(policy.id)
    w.int(budget)
    w.int(priority)
    w.bool(bid_ahead)
  }
}

object MkWallet {
  def unserialize(r: StateReader) = MkWallet(WalletType(r.int), r.int, r.int, r.bool)
}

case class MkIntersection(id: VertexID, policy: IntersectionType.Value,
                          ordering: OrderingType.Value)
{
  def make(v: Vertex, sim: Simulation) = new Intersection(v, policy, ordering, sim)

  def diff(other: MkIntersection) = {
    Util.assert_eq(id, other.id)
    val d = List(
      Util.diff(policy, other.policy, "policy"),
      Util.diff(ordering, other.ordering, "ordering")
    ).flatten.mkString(", ")
    if (d.nonEmpty) {
      Util.log(s"Intersection $id different: $d")
    }
  }

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.int(policy.id)
    w.int(ordering.id)
  }
}

object MkIntersection {
  def unserialize(r: StateReader) = MkIntersection(
    new VertexID(r.int), IntersectionType(r.int), OrderingType(r.int)
  )
}

case class SystemWalletConfig(
  thruput_bonus: Int            = 7,
  avail_capacity_threshold: Int = 25,
  capacity_bonus: Int           = 5,
  // This one easily dominates decisions
  dependency_rate: Int          = 2,
  waiting_rate: Int             = 1,
  ready_bonus: Int              = 5
) {
  override def toString =
    s"SystemWalletConfig(thruput_bonus = $thruput_bonus, avail_capacity_threshold = $avail_capacity_threshold, capacity_bonus = $capacity_bonus, dependency_rate = $dependency_rate, waiting_rate = $waiting_rate, ready_bonus = $ready_bonus)"

  def serialize(w: StateWriter) {
    w.int(thruput_bonus)
    w.int(avail_capacity_threshold)
    w.int(capacity_bonus)
    w.int(dependency_rate)
    w.int(waiting_rate)
    w.int(ready_bonus)
  }
}

object SystemWalletConfig {
  def unserialize(r: StateReader) = SystemWalletConfig(
    r.int, r.int, r.int, r.int, r.int, r.int
  )

  def blank = SystemWalletConfig(0, 0, 0, 0, 0, 0)
}

// Enumeration stuff

object IntersectionType extends Enumeration {
  type IntersectionType = Value
  val NeverGo, StopSign, Signal, Reservation, Yield, AIM = Value
}

object RouteType extends Enumeration {
  type RouteType = Value
  val Path = Value
}

object RouterType extends Enumeration {
  type RouterType = Value
  // Agents don't use Unusable; it's just for manually-invoked routers.
  val Congestion, Zone, Fixed, Unusable, DumbToll, TollThreshold, SumToll = Value
}

object OrderingType extends Enumeration {
  type OrderingType = Value
  val FIFO, Auction = Value
}

object WalletType extends Enumeration {
  type WalletType = Value
  val Static, Freerider, Fair, System = Value
}

object CongestionType extends Enumeration {
  type CongestionType = Value
  val Current, Sticky, MovingWindow = Value
}

object Factory {
  def make_policy(i: Intersection, policy: IntersectionType.Value,
                  ordering: OrderingType.Value, sim: Simulation) = policy match
  {
    case IntersectionType.NeverGo =>
      new NeverGoPolicy(i)
    case IntersectionType.StopSign =>
      new StopSignPolicy(i, make_intersection_ordering[Ticket](ordering))
    case IntersectionType.Signal =>
      new SignalPolicy(i, make_intersection_ordering[Phase](ordering), sim)
    case IntersectionType.Reservation =>
      new ReservationPolicy(i, make_intersection_ordering[Ticket](ordering))
    case IntersectionType.Yield =>
      new YieldPolicy(i, make_intersection_ordering[Ticket](ordering))
    case IntersectionType.AIM =>
      new AIMPolicy(i, make_intersection_ordering[Ticket](ordering))
  }

  def make_route(
    enum: RouteType.Value, graph: Graph, orig_router: RouterType.Value, rerouter: RouterType.Value,
    goal: Road, initial_path: List[Road]
  ) = enum match {
    case RouteType.Path => new PathRoute(
      goal, make_router(orig_router, graph, initial_path), make_router(rerouter, graph, Nil)
    )
  }

  def make_router(enum: RouterType.Value, graph: Graph, initial_path: List[Road])
  = enum match {
    case RouterType.Congestion => new CongestionRouter(graph)
    case RouterType.Zone => new ZoneRouter(graph)
    case RouterType.Fixed => new FixedRouter(graph, initial_path)
    case RouterType.DumbToll => new DumbTollRouter(graph)
    case RouterType.TollThreshold => new TollThresholdRouter(graph)
    case RouterType.SumToll => new SumTollRouter(graph)
  }

  def make_intersection_ordering[T <: Ordered[T]](enum: OrderingType.Value) = enum match
  {
    case OrderingType.FIFO => new FIFO_Ordering[T]()
    case OrderingType.Auction => new AuctionOrdering[T]()
  }

  def make_wallet(enum: WalletType.Value, budget: Int, priority: Int, bid_ahead: Boolean)
  = enum match {
    case WalletType.Static => new StaticWallet(budget, priority)
    case WalletType.Freerider => new FreeriderWallet(priority)
    case WalletType.Fair => new FairWallet(budget, priority, bid_ahead)
  }

  def make_auditor(enum: CongestionType.Value, r: Road, sim: Simulation) = enum match {
    case CongestionType.Current => new CurrentCongestion(r, sim)
    case CongestionType.Sticky => new StickyCongestion(r, sim)
    case CongestionType.MovingWindow => new MovingWindowCongestion(r, sim)
  }
}

// TODO Most of this file is ripe for a java beans-esque generalization.
