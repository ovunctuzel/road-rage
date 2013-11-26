// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.make

import utexas.aorta.map.{Graph, DirectedRoad}
import utexas.aorta.common.{RNG, cfg,  AgentID}

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

  def filter_candidates(starts: Array[DirectedRoad]) = starts.filter(_.rightmost.ok_to_spawn)

  // TODO specify "80% x, 20% y" for stuff...
  def uniform(ids: Range, starts: Array[DirectedRoad], ends: Array[DirectedRoad],
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
        new AgentID(id), time, rng.new_seed, start.id, start.rightmost.safe_spawn_dist(rng),
        MkRoute(rng.choose(routes), RouterType.ContractionHierarchy, RouterType.Congestion,
                Nil, rng.choose(ends).id, rng.new_seed),
        // For now, force the same budget and priority here, and clean it up
        // later.
        MkWallet(rng.choose(wallets), budget, budget, false /* bid_ahead */)
      )
    }).toArray
  }

  def default(graph: Graph) = uniform(
    Range(0, cfg.army_size), graph.directed_roads, graph.directed_roads, (0.0, 60.0),
    Array(default_route), Array(default_wallet), (100, 200)
  )
}
