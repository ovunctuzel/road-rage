// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.{Graph, Edge, Vertex, DirectedRoad}
import utexas.aorta.sim.policies._
import utexas.aorta.sim.market._

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.{Util, RNG, Common, cfg}

@SerialVersionUID(1)
// Array index and agent/intersection ID must correspond. Client's
// responsibility.
case class Scenario(map_fn: String, agents: Array[MkAgent],
                    intersections: Array[MkIntersection],
                    system_wallet: SystemWalletConfig)
{
  def make_sim(graph: Graph = Graph.load(map_fn)) = new Simulation(graph, this)
  def save(fn: String) = Util.serialize(this, fn)

  def make_intersection(v: Vertex) = intersections(v.id).make(v)
  def make_agents() = agents.foreach(a => a.make)

  // Although the massive numbers of agents were probably created with a
  // distribution function originally, we just see the individual list in the
  // end. So compute basic stats about it.
  def summarize() = {
    Util.log(s"Scenario for $map_fn\n")
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
      Util.log("Budget ($): " + basic_stats(agents.map(_.wallet.budget)))
      Util.log("Priority: " + basic_stats(agents.map(_.wallet.priority)))
      Util.log_pop
    }

    Util.log("System wallet rates:")
    Util.log(system_wallet.toString)
  }

  // Describe the percentages of each thing
  private def percentages[T](things: Iterable[T]) = {
    val count = new MutableMap[T, Int]().withDefaultValue(0)
    things.foreach(t => count(t) += 1)
    val total = things.size
    Util.log_push
    for (key <- count.keys) {
      val percent = count(key).toDouble / total * 100.0
      Util.log(f"$key: ${percent}%.2f%")
    }
    Util.log_pop
  }

  // Min, max, average
  private def basic_stats(nums: Iterable[Double]) =
    f"${nums.min}%.2f - ${nums.max}%.2f (average ${nums.sum / nums.size}%.2f)"

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
}

object Scenario {
  def load(fn: String) = Util.unserialize(fn).asInstanceOf[Scenario]

  def load_or_default_sim(fn: String) = Util.unserialize(fn) match {
    case s: Scenario => {
      Common.scenario = s
      s.make_sim()
    }
    case g: Graph => {
      Graph.set_params(g.width, g.height, g.offX, g.offY, g.scale)
      Common.edges = g.edges
      default(fn, g).make_sim(g)
    }
  }

  def default(map_fn: String, graph: Graph): Scenario = {
    val s = Scenario(
      map_fn,
      AgentDistribution.default(graph),
      IntersectionDistribution.default(graph),
      SystemWalletConfig()
    )
    Common.scenario = s
    // Always save it, so resimulation is easy.
    (new File("./scenarios")).mkdir
    s.save(s"scenarios/default_${graph.name}")
    return s
  }
}

// The "Mk" prefix means "Make". These're small serializable classes to make
// agents/intersections/etc.
// TODO associate directly with their corresponding class?

@SerialVersionUID(1)
case class MkAgent(id: Int, birth_tick: Double, seed: Long,
                   start_edge: Int, start_dist: Double, route: MkRoute,
                   wallet: MkWallet)
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
case class MkWallet(policy: WalletType.Value, budget: Double, priority: Double) {
  def make(a: Agent) = Factory.make_wallet(a, policy, budget, priority)
}

@SerialVersionUID(1)
case class MkIntersection(id: Integer, policy: IntersectionType.Value,
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
}

@SerialVersionUID(1)
case class SystemWalletConfig(
  thruput_bonus: Double            = 5.00,
  avail_capacity_threshold: Double = 25.0,
  capacity_bonus: Double           = 2.00,
  time_rate: Double                = 2.00,
  dependency_rate: Double          = 0.50,
  waiting_rate: Double             = 0.01
)

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
              budgets: (Double, Double)): Array[MkAgent] =
  {
    val actual_starts = filter_candidates(starts)
    return ids.map(id => {
      val start = rng.choose(actual_starts)
      val budget = rng.double(budgets._1, budgets._2)
      MkAgent(
        id, rng.double(times._1, times._2), rng.new_seed, start.id,
        start.safe_spawn_dist(rng),
        MkRoute(rng.choose(routes), rng.choose(ends).id, rng.new_seed),
        // For now, force the same budget and priority here, and clean it up
        // later.
        MkWallet(rng.choose(wallets), budget, budget)
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
  val NeverGo, StopSign, Signal, Reservation, CommonCase = Value
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
  val Random, Static, Freerider, Fair, System = Value
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

  def make_route(enum: RouteType.Value, goal: DirectedRoad, rng: RNG) = enum match {
    case RouteType.Dijkstra => new DijkstraRoute(goal, rng)
    case RouteType.Path => new PathRoute(goal, rng)
    case RouteType.Drunken => new DrunkenRoute(goal, rng)
    case RouteType.DirectionalDrunk => new DirectionalDrunkRoute(goal, rng)
    case RouteType.DrunkenExplorer => new DrunkenExplorerRoute(goal, rng)
  }

  def make_intersection_ordering[T <: Ordered[T]](enum: OrderingType.Value) = enum match
  {
    case OrderingType.FIFO => new FIFO_Ordering[T]()
    case OrderingType.Auction => new AuctionOrdering[T]()
  }

  def make_wallet(a: Agent, enum: WalletType.Value, budget: Double, priority: Double) = enum match {
    case WalletType.Random => new RandomWallet(a, budget, priority)
    case WalletType.Static => new StaticWallet(a, budget, priority)
    case WalletType.Freerider => new FreeriderWallet(a, priority)
    case WalletType.Fair => new FairWallet(a, budget, priority)
  }
}

// Command-line interface
object ScenarioTool {
  // TODO generate choices for things!
  val usage =
    ("scenarios/foo --out scenarios/new_foo [--vert ...]* [--agent ...]* [--spawn ...]* [--cfg_wallets ...]\n" +
     "  --vert 42 policy=StopSign ordering=FIFO\n" +
     "  --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90.0\n" +
     "  --spawn 500 start=0 end=100 time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90.0\n" +
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
          input, Array(), Array(), SystemWalletConfig()
        )
        s = s.copy(
          intersections = IntersectionDistribution.default(graph)
        )
      }
    }

    var output = ""
    val rng = new RNG()

    while (args.nonEmpty) {
      shift_args match {
        case "--out" => {
          output = shift_args
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
        // --agent 3 start=0 end=100 time=16.2 route=Drunken wallet=Random budget=90.0
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
            start_edge = params.getOrElse(
              "start", old_a.start_edge.toString
            ).toInt,
            start_dist = start_dist,
            birth_tick = params.getOrElse(
              "time", old_a.birth_tick.toString
            ).toDouble,
            route = old_a.route.copy(
              strategy = RouteType.withName(
                params.getOrElse("route", old_a.route.strategy.toString)
              ),
              goal = params.getOrElse("end", old_a.route.goal.toString).toInt
            ),
            wallet = old_a.wallet.copy(
              policy = WalletType.withName(
                params.getOrElse("wallet", old_a.wallet.policy.toString)
              ),
              budget = params.getOrElse(
                "budget", old_a.wallet.budget.toString
              ).toDouble
            )
          )

          old_a.diff(new_a)
          s.agents(id) = new_a
        }
        // TODO specifying ranges or choices for stuff.
        // --spawn 500 start=0 end=100 time=16.2 generations=1 lifetime=3600 route=Drunken wallet=Random budget=90.0
        case "--spawn" => {
          val number = shift_args.toInt
          val params = slurp_params
          val bad_params = params.keys.toSet.diff(Set(
            "start", "end", "delay", "generations", "lifetime", "route",
            "wallet", "budget"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --spawn")
            sys.exit
          }

          val starts = params.get("start") match {
            case Some(e) => Array(graph.edges(e.toInt))
            case None => graph.edges
          }
          val ends = params.get("end") match {
            case Some(e) => Array(graph.edges(e.toInt))
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
              (a.toDouble, b.toDouble)
            }
            case None => (0.0, 10.0)
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
        // --all_agents wallet=Random budget=1.0
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
              (_: Double) => rng.double(a.toDouble, b.toDouble)
            }
            case None => (old: Double) => old
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
            "time_rate", "dependency_rate", "waiting_rate"
          ))
          if (!bad_params.isEmpty) {
            Util.log(s"$bad_params aren't valid params for --cfg_wallets")
            sys.exit
          }

          var wallet = s.system_wallet
          params.foreach(pair => wallet = pair match {
            case ("thruput_bonus", x) => wallet.copy(thruput_bonus = x.toDouble)
            case ("avail_capacity_threshold", x) => wallet.copy(avail_capacity_threshold = x.toDouble)
            case ("capacity_bonus", x) => wallet.copy(capacity_bonus = x.toDouble)
            case ("time_rate", x) => wallet.copy(time_rate = x.toDouble)
            case ("dependency_rate", x) => wallet.copy(dependency_rate = x.toDouble)
            case ("waiting_rate", x) => wallet.copy(waiting_rate = x.toDouble)
          })

          Util.log(s"Changing system wallet configuration: $wallet")
          s = s.copy(system_wallet = wallet)
        }
        case _ => dump_usage
      }
    }
    Util.log("")

    s.summarize

    if (output.nonEmpty) {
      s.save(output)
      Util.log(s"\nSaved scenario to $output")
      // TODO warn if overwriting? prompt?
    }
  }
}

// TODO Most of this file is ripe for a java beans-esque generalization.
