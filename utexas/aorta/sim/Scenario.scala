// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Graph, Edge, Vertex, DirectedRoad}
import utexas.aorta.map.analysis.{FixedRouter, CHRouter, CongestionRouter, Router}
import utexas.aorta.sim.policies._
import utexas.aorta.sim.market._

import scala.collection.mutable

import utexas.aorta.common.{Util, RNG, Common, cfg, StateWriter, StateReader,
                            AgentID, VertexID, EdgeID, DirectedRoadID}

// Array index and agent/intersection ID must correspond. Creator's
// responsibility.
case class Scenario(name: String, map_fn: String, agents: Array[MkAgent],
                    intersections: Array[MkIntersection],
                    system_wallet: SystemWalletConfig)
{
  def make_sim(graph: Graph = Graph.load(map_fn)) = new Simulation(graph, this)
  def save() {
    val w = Util.writer(name)
    serialize(w)
    w.done()
  }

  def make_intersection(v: Vertex) = intersections(v.id.int).make(v)

  // Although the massive numbers of agents were probably created with a
  // distribution function originally, we just see the individual list in the
  // end. So compute basic stats about it.
  def summarize() = {
    Util.log(s"Scenario $name for $map_fn\n")
    // TODO breakdown combos of policy/ordering, and wallet/budget
    Util.log("Intersection policies:")
    percentages(intersections.map(_.policy))
    Util.log("Intersection orderings:")
    percentages(intersections.map(_.ordering))
    Util.log("")

    Util.log(s"${agents.size} agents total")
    // TODO where are agents starting/going?
    if (agents.nonEmpty) {
      Util.log_push
      Util.log("Spawning time (s): " + basic_stats(agents.map(_.birth_tick)))
      Util.log("Routes:")
      percentages(agents.map(_.route.strategy))
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
    intersections.zip(other.intersections).foreach(pair => pair._1.diff(pair._2))
    agents.zip(other.agents).foreach(pair => pair._1.diff(pair._2))
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
  }

  // Spawn each agent in an empty simulation, then figure out how long it takes
  // each to complete their trip with nobody else around. Returns a map from
  // agent ID to that time.
  def compute_optimal_times_by_simulation(): Map[AgentID, Double] = {
    val graph = Graph.load(map_fn)
    val times = new mutable.HashMap[AgentID, Double]()
    var cnt = 0
    for (a <- agents) {
      cnt += 1
      Util.log(s"Computing optimal time for agent $cnt/${agents.size}")
      val solo_scenario = this.copy(agents = Array(a))
      val sim = solo_scenario.make_sim(graph)
      // TODO by simulation is way too slow. do it analytically.
      sim.setup()
      while (!sim.done) {
        sim.step()
      }
      times(a.id) = sim.tick - a.birth_tick
    }
    return times.toMap
  }

  /*
  // Return a map from agent ID to trip time if that agent was moving through an
  // otherwise empty world. Compute the time analytically, so lower than the
  // true optimal time.
  def compute_optimal_times_analytically(): Map[AgentID, Double] = {
    // Hokey assumptions:
    // - don't stop for intersections; immediately blaze through
    // - instantly reach speed limit of each road
    // - (minor) travel entire distance of first edge, rather than just part of
    // it
    // TODO (major) use CH router always
    val graph = Graph.load(map_fn)
    val times = new mutable.HashMap[AgentID, Double]()
    var cnt = 0
    for (a <- agents) {
      cnt += 1
      // TODO every 1000 or so, refactor that from mapmaking
      //Util.log(s"Computing ~optimal time for agent $cnt/${agents.size}")
      val path = graph.ch_router.path(
        graph.edges(a.start_edge.int).directed_road,
        graph.edges(a.route.goal.int).directed_road, a.birth_tick
      )
      times(a.id) = path.map(step => step.road.length / step.road.speed_limit).sum
    }
    return times.toMap
  }*/

  def num_agents = agents.size
}

object Scenario {
  def unserialize(r: StateReader) = Scenario(
    r.string, r.string,
    Range(0, r.int).map(_ => MkAgent.unserialize(r)).toArray,
    Range(0, r.int).map(_ => MkIntersection.unserialize(r)).toArray,
    SystemWalletConfig.unserialize(r)
  )

  def load(fn: String) = unserialize(Util.reader(fn))

  def default(map_fn: String, graph: Graph): Scenario = {
    val s = Scenario(
      s"scenarios/default_${graph.name}",
      map_fn,
      AgentDistribution.default(graph),
      IntersectionDistribution.default(graph),
      SystemWalletConfig()
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

case class MkAgent(id: AgentID, birth_tick: Double, seed: Long,
                   start_edge: EdgeID, start_dist: Double, route: MkRoute,
                   wallet: MkWallet) extends Ordered[MkAgent]
{
  // break ties by ID
  def compare(other: MkAgent) =
    implicitly[Ordering[Tuple2[Double, Integer]]].compare(
      (other.birth_tick, other.id.int), (birth_tick, id.int)
    )

  def make(sim: Simulation) = new Agent(
    id, route.make(sim), new RNG(seed), wallet.make, birth_tick, start_edge
  )

  def serialize(w: StateWriter) {
    w.int(id.int)
    w.double(birth_tick)
    w.long(seed)
    w.int(start_edge.int)
    w.double(start_dist)
    route.serialize(w)
    wallet.serialize(w)
  }

  def diff(other: MkAgent) = {
    Util.assert_eq(id, other.id)
    val d = List(
      Util.diff(birth_tick, other.birth_tick, "spawn time"),
      Util.diff(seed, other.seed, "RNG seed"),
      Util.diff(start_edge, other.start_edge, "start"),
      Util.diff(start_dist, other.start_dist, "start distance"),
      Util.diff(route.goal, other.route.goal, "end"),
      Util.diff(route.strategy, other.route.strategy, "route"),
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
    new AgentID(r.int), r.double, r.long, new EdgeID(r.int), r.double, MkRoute.unserialize(r),
    MkWallet.unserialize(r)
  )
}

// orig_router, rerouter, and initial_path are only for strategy = Path
// TODO ID of a directed road would be way better for goal
case class MkRoute(
  strategy: RouteType.Value, orig_router: RouterType.Value, rerouter: RouterType.Value,
  initial_path: List[DirectedRoadID], goal: EdgeID, seed: Long
) {
  def make(sim: Simulation) = Factory.make_route(
    strategy, sim.graph, orig_router, rerouter, sim.edges(goal.int).directed_road, new RNG(seed),
    initial_path.map(id => sim.graph.directed_roads(id.int))
  )

  def serialize(w: StateWriter) {
    w.int(strategy.id)
    w.int(orig_router.id)
    w.int(rerouter.id)
    w.int(initial_path.size)
    initial_path.foreach(id => w.int(id.int))
    w.int(goal.int)
    w.long(seed)
  }
}

object MkRoute {
  def unserialize(r: StateReader) = MkRoute(
    RouteType(r.int), RouterType(r.int), RouterType(r.int),
    Range(0, r.int).map(_ => new DirectedRoadID(r.int)).toList, new EdgeID(r.int), r.long
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
  def make(v: Vertex) = new Intersection(v, policy, ordering)

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

  def uniform_default(graph: Graph) = uniform(
    graph, Array(default_policy), Array(default_ordering)
  )

  def default(graph: Graph) = realistic(graph)

  // Put stop signs at crossings of all small roads, signals or reservations at
  // crossings of all big roads, and common case hybrids at mixtures
  def realistic(graph: Graph) = graph.vertices.map(v => {
    val (big, small) = v.roads.partition(_.is_major)
    val policy =
      if (big.isEmpty)
        IntersectionType.StopSign
      else if (small.isEmpty)
        IntersectionType.Signal
      else
        IntersectionType.Reservation
    MkIntersection(v.id, policy, default_ordering)
  })
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
              budgets: (Int, Int)): Array[MkAgent] =
  {
    val actual_starts = filter_candidates(starts)
    return ids.map(id => {
      val start = rng.choose(actual_starts)
      val budget = rng.int(budgets._1, budgets._2)
      val raw_time = rng.double(times._1, times._2)
      val time = raw_time - (raw_time % cfg.dt_s) // TODO may still have fp issue
      MkAgent(
        new AgentID(id), time, rng.new_seed, start.id, start.safe_spawn_dist(rng),
        MkRoute(rng.choose(routes), RouterType.ContractionHierarchy, RouterType.Congestion,
                Nil, rng.choose(ends).id, rng.new_seed),
        // For now, force the same budget and priority here, and clean it up
        // later.
        MkWallet(rng.choose(wallets), budget, budget, false /* bid_ahead */)
      )
    }).toArray
  }

  def default(graph: Graph) = uniform(
    Range(0, cfg.army_size), graph.edges, graph.edges, (0.0, 60.0),
    Array(default_route), Array(default_wallet), (100, 200)
  )
}

// Enumeration stuff

object IntersectionType extends Enumeration {
  type IntersectionType = Value
  val NeverGo, StopSign, Signal, Reservation, CommonCase = Value
}

object RouteType extends Enumeration {
  type RouteType = Value
  val Dijkstra, Path, Drunken, DirectionalDrunk, DrunkenExplorer = Value
}

object RouterType extends Enumeration {
  type RouterType = Value
  // TODO RouteFeatureBased, the new clown car thing
  // TODO and rm TMP
  val ContractionHierarchy, Congestion, Fixed, TMP, DumbToll, TollThreshold = Value
}

object OrderingType extends Enumeration {
  type OrderingType = Value
  val FIFO, Auction = Value
}

object WalletType extends Enumeration {
  type WalletType = Value
  val Static, Freerider, Fair, System = Value
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
      new ReservationPolicy(i, make_intersection_ordering[Ticket](ordering))
    case IntersectionType.CommonCase =>
      new CommonCasePolicy(i, make_intersection_ordering[Ticket](ordering))
  }

  def make_route(
    enum: RouteType.Value, graph: Graph, orig_router: RouterType.Value, rerouter: RouterType.Value,
    goal: DirectedRoad, rng: RNG, initial_path: List[DirectedRoad]
  ) = enum match {
    case RouteType.Dijkstra => new DijkstraRoute(goal, rng)
    case RouteType.Path => new PathRoute(
      goal, make_router(orig_router, graph, initial_path), make_router(rerouter, graph, Nil), rng
    )
    case RouteType.Drunken => new DrunkenRoute(goal, rng)
    case RouteType.DirectionalDrunk => new DirectionalDrunkRoute(goal, rng)
    case RouteType.DrunkenExplorer => new DrunkenExplorerRoute(goal, rng)
  }

  def make_router(enum: RouterType.Value, graph: Graph, initial_path: List[DirectedRoad])
  = enum match {
    // TODO fall back on dijkstra if unusable?
    case RouterType.ContractionHierarchy => new CHRouter(graph)
    case RouterType.Congestion => new CongestionRouter(graph)
    case RouterType.Fixed => new FixedRouter(graph, initial_path)
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
}

// Command-line interface
object ScenarioTool {
  // TODO generate choices for things!
  val usage =
    ("scenarios/foo --out scenarios/new_foo [--vert ...]* [--agent ...]* [--spawn ...]* [--cfg_wallets ...]\n" +
     "  --vert 42 policy=StopSign ordering=FIFO\n" +
     "  --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90\n" +
     "  --spawn 500 start=0 end=100 time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90\n" +
     "  --all_verts policy=StopSign ordering=FIFO\n" +
     "\n" +
     "Or pass two scenarios to get a diff")
     // TODO removing all existing agents, or something

  private def dump_usage() = {
    Util.log(usage)
    sys.exit
  }

  // TODO split it up a bit for readability
  def main(arg_array: Array[String]) = {
    if (arg_array.size == 2) {
      val Array(a1, a2) = arg_array
      Util.log(s"Diffing $a1 with $a2\n")
      val s1 = Scenario.load(a1)
      val s2 = Scenario.load(a2)
      s1.diff(s2)
      sys.exit
    }

    var args = arg_array.toList
    def shift_args(): String = {
      if (args.isEmpty) {
        dump_usage
      }
      val head = args.head
      args = args.tail
      return head
    }

    def slurp_params(): Map[String, String] = {
      val pair = args.span(a => !a.startsWith("--"))
      args = pair._2
      return pair._1.map(p => {
        val Array(k, v) = p.split("=")
        k -> v
      }).toMap
    }

    // If they pass in a map instead, make an empty scenario
    val input = shift_args
    var s: Scenario = null
    // the logging will be ugly if this gets initialized in the middle of
    // something
    lazy val graph = Graph.load(s.map_fn)

    try {
      s = Scenario.load(input)
    } catch {
      case _: Throwable => {
        Util.log(s"Initializing empty scenario on $input...")
        s = Scenario(
          s"scenarios/empty_${input}", input, Array(), Array(),
          SystemWalletConfig()
        )
        s = s.copy(
          intersections = IntersectionDistribution.default(graph)
        )
      }
    }

    val rng = new RNG()
    var changed_output = false

    while (args.nonEmpty) {
      shift_args match {
        case "--out" => {
          s = s.copy(name = shift_args)
          changed_output = true
        }
        // --vert 42 policy=StopSign ordering=FIFO
        case "--vert" => {
          val id = shift_args.toInt
          val old_v = s.intersections(id)
          Util.assert_eq(old_v.id, id)
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set("policy", "ordering"))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --vert")
            sys.exit
          }

          val new_v = old_v.copy(
            policy = IntersectionType.withName(
              params.getOrElse("policy", old_v.policy.toString)
            ),
            ordering = OrderingType.withName(
              params.getOrElse("ordering", old_v.ordering.toString)
            )
          )

          old_v.diff(new_v)
          s.intersections(id) = new_v
        }
        // --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90
        case "--agent" => {
          val id = shift_args.toInt
          val old_a = s.agents(id)
          Util.assert_eq(old_a.id, id)
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "start", "time", "route", "end", "wallet", "budget"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --agent")
            sys.exit
          }

          // Preserve the original spawning distance, or choose an appropriate
          // new one
          val start_dist = params.get("start") match {
            case Some(e) if e.toInt == old_a.start_edge => old_a.start_dist
            case Some(e) => graph.edges(e.toInt).safe_spawn_dist(rng)
            case None => old_a.start_dist
          }

          val new_a = old_a.copy(
            // TODO fix all the orElse patterns here
            start_edge =
              params.get("start").map(e => new EdgeID(e.toInt)).getOrElse(old_a.start_edge),
            start_dist = start_dist,
            birth_tick = params.getOrElse(
              "time", old_a.birth_tick.toString
            ).toDouble,
            route = old_a.route.copy(
              strategy = RouteType.withName(
                params.getOrElse("route", old_a.route.strategy.toString)
              ),
              goal = params.get("end").map(e => new EdgeID(e.toInt)).getOrElse(old_a.route.goal)
            ),
            wallet = old_a.wallet.copy(
              policy = WalletType.withName(
                params.getOrElse("wallet", old_a.wallet.policy.toString)
              ),
              budget = params.getOrElse(
                "budget", old_a.wallet.budget.toString
              ).toInt
            )
          )

          old_a.diff(new_a)
          s.agents(id) = new_a
        }
        // TODO specifying ranges or choices for stuff.
        // --spawn 500 start=area_file end=area_file time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90
        case "--spawn" => {
          val number = shift_args.toInt
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "starts", "ends", "delay", "generations", "lifetime", "route",
            "wallet", "budget"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --spawn")
            sys.exit
          }

          val starts = params.get("starts") match {
            case Some(fn) => {
              val r = Util.reader(fn)
              Range(0, r.int).map(_ => graph.edges(r.int)).toArray
            }
            case None => graph.edges
          }
          val ends = params.get("ends") match {
            case Some(fn) => {
              val r = Util.reader(fn)
              Range(0, r.int).map(_ => graph.edges(r.int)).toArray
            }
            case None => graph.edges
          }
          val delay = params.get("delay") match {
            case Some(t) => t.toDouble
            case None => 0.0
          }
          val generations = params.get("generations") match {
            case Some(t) => t.toInt
            case None => 1
          }
          val lifetime = params.get("lifetime") match {
            case Some(t) => t.toDouble
            case None => 3600.0
          }
          val route = Array(
            RouteType.withName(params.getOrElse("route", cfg.route))
          )
          val wallet = Array(
            WalletType.withName(params.getOrElse("wallet", cfg.wallet))
          )
          val budget = params.get("budget") match {
            case Some(t) => {
              val Array(a, b) = t.split("-")
              (a.toInt, b.toInt)
            }
            case None => (0, 10)
          }

          // TODO describe more?
          Util.log(s"Adding $number * $generations new agents")
          for (generation <- Range(0, generations)) {
            val time = (generation * lifetime, generation * lifetime + delay)
            val new_agents = AgentDistribution.uniform(
              Range(s.agents.size, s.agents.size + number), starts, ends, time,
              route, wallet, budget
            )
            s = s.copy(agents = s.agents ++ new_agents)
          }
        }
        // --all_verts policy=StopSign ordering=FIFO
        case "--all_verts" => {
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set("policy", "ordering"))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --all_verts")
            sys.exit
          }

          params.get("policy") match {
            case Some(name) => {
              val p = IntersectionType.withName(name)
              Util.log(s"Changing all intersections to $p")
              s = s.copy(intersections = s.intersections.map(_.copy(policy = p)))
            }
            case None =>
          }

          params.get("ordering") match {
            case Some(name) => {
              val o = OrderingType.withName(name)
              Util.log(s"Changing all intersections to use $o ordering")
              s = s.copy(intersections = s.intersections.map(_.copy(ordering = o)))
            }
            case None =>
          }
        }
        // --all_agents wallet=Random budget=1
        case "--all_agents" => {
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set("wallet", "budget"))
          // TODO priority controls in all these tools!
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --all_agents")
            sys.exit
          }

          val budget_factory = params.get("budget") match {
            case Some(t) => {
              val Array(a, b) = t.split("-")
              (_: Int) => rng.int(a.toInt, b.toInt)
            }
            case None => (old: Int) => old
          }

          // TODO support all options. for now, these are the ones I need.
          Util.log(s"Changing all agents: $params")
          s = s.copy(agents = s.agents.map(a => a.copy(
            wallet = a.wallet.copy(
              policy = WalletType.withName(
                params.getOrElse("wallet", a.wallet.policy.toString)
              ),
              budget = budget_factory(a.wallet.budget)
            )
          )))
        }
        case "--cfg_wallets" => {
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "thruput_bonus", "avail_capacity_threshold", "capacity_bonus",
            "dependency_rate", "waiting_rate", "ready_rate"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --cfg_wallets")
            sys.exit
          }

          var wallet = s.system_wallet
          params.foreach(pair => wallet = pair match {
            case ("thruput_bonus", x) => wallet.copy(thruput_bonus = x.toInt)
            case ("avail_capacity_threshold", x) => wallet.copy(avail_capacity_threshold = x.toInt)
            case ("capacity_bonus", x) => wallet.copy(capacity_bonus = x.toInt)
            case ("dependency_rate", x) => wallet.copy(dependency_rate = x.toInt)
            case ("waiting_rate", x) => wallet.copy(waiting_rate = x.toInt)
            case ("ready_rate", x) => wallet.copy(ready_bonus = x.toInt)
          })

          Util.log(s"Changing system wallet configuration: $wallet")
          s = s.copy(system_wallet = wallet)
        }
        case _ => dump_usage
      }
    }
    Util.log("")

    s.summarize

    if (changed_output) {
      s.save()
      Util.log(s"\nSaved scenario to ${s.name}")
      // TODO warn if overwriting? prompt?
    }
  }
}

// TODO Most of this file is ripe for a java beans-esque generalization.
