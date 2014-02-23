// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import Function.tupled
import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.{Edge, Road, Traversable, Turn, Vertex, Graph, Router}
import utexas.aorta.sim.{EV_Transition, EV_Reroute}
import utexas.aorta.sim.make.{RouteType, RouterType, Factory, ReroutePolicyType}

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader, TurnID, Serializable}
import utexas.aorta.common.algorithms.Pathfind

// TODO maybe unify the one class with the interface, or something. other routes were useless.

// Get a client to their goal by any means possible.
abstract class Route(val goal: Road, reroute_policy: ReroutePolicyType.Value) extends Serializable {
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  protected var owner: Agent = null
  protected var debug_me = false  // TODO just grab from owner?
  private var rerouter: ReroutePolicy = null // TODO is it transient?

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.ints(route_type.id, goal.id.int, reroute_policy.id)
  }

  protected def unserialize(r: StateReader, graph: Graph) {}

  def setup(a: Agent) {
    owner = a
    rerouter = Factory.make_reroute_policy(reroute_policy, a)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // For lookahead clients. No lane-changing.
  def steps_to(at: Traversable, v: Vertex): List[Traversable] = at match {
    case e: Edge if e.to == v => at :: Nil
    case e: Edge => at :: steps_to(pick_turn(e), v)
    case t: Turn => at :: steps_to(t.to, v)
  }

  def react() {
    rerouter.react()
  }

  // The client tells us they've physically moved
  def transition(from: Traversable, to: Traversable)
  // The client is being forced to pick a turn. If they ask us repeatedly, we
  // have to always return the same answer.
  def pick_turn(e: Edge): Turn
  // Prescribe the final lane on this road to aim for. We should be able to spazz around in
  // our answer here. True if e doesn't already lead to next road, aka, the final lane is more than
  // recommended.
  def pick_final_lane(e: Edge): (Edge, Boolean)
  // Just mark that we don't have to take the old turn prescribed
  def request_reroute(at: Edge) {}
  // Immediately perform rerouting requested
  def flush_reroute(at: Edge) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type(): RouteType.Value
  def done(at: Edge) = at.road == goal
  def dump_info()
  def current_path(): List[Road]
  // Includes current road
  def next_roads(num: Int): List[Road]
}

object Route {
  def unserialize(r: StateReader, graph: Graph): Route = {
    // Original router will never be used again, and rerouter will have to be reset by PathRoute.
    val route = Factory.make_route(
      RouteType(r.int), graph, RouterType.Fixed, RouterType.Fixed, graph.roads(r.int), Nil,
      ReroutePolicyType(r.int)
    )
    route.unserialize(r, graph)
    return route
  }
}

// Follow routes prescribed by routers. Only reroute when forced or encouraged to.
// TODO rerouter only var due to serialization
class PathRoute(
  goal: Road, orig_router: Router, private var rerouter: Router, reroute_policy: ReroutePolicyType.Value
) extends Route(goal, reroute_policy) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Head is the current step. If that step isn't immediately reachable, we have
  // to re-route.
  private var path: List[Road] = Nil
  private val chosen_turns = new mutable.ImmutableMapAdaptor(new immutable.TreeMap[Edge, Turn]())
  private val reroutes_requested = new mutable.TreeSet[Edge]()

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.int(rerouter.router_type.id)
    // We can't tell when we last rerouted given less state; store the full
    // path.
    w.list_int(path.map(_.id.int))
    w.int(chosen_turns.size)
    chosen_turns.foreach(tupled((e, t) => {
      w.ints(e.id.int, t.id.int)
    }))
    w.list_int(reroutes_requested.map(_.id.int).toList)
  }

  override def unserialize(r: StateReader, graph: Graph) {
    rerouter = Factory.make_router(RouterType(r.int), graph, Nil)
    path = Range(0, r.int).map(_ => graph.roads(r.int)).toList
    val chosen_size = r.int
    for (i <- Range(0, chosen_size)) {
      chosen_turns(graph.edges(r.int)) = graph.turns(new TurnID(r.int))
    }
    Range(0, r.int).map(_ => reroutes_requested += graph.edges(r.int))
  }

  override def setup(a: Agent) {
    super.setup(a)
    orig_router.setup(a)
    rerouter.setup(a)

    Util.assert_eq(path.isEmpty, true)
    path = orig_router.path(Pathfind(start = a.at.on.asEdge.road, goals = Set(goal)))
    owner.sim.publish(
      EV_Reroute(owner, path, true, orig_router.router_type, false, Nil), owner
    )
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def transition(from: Traversable, to: Traversable) {
    (from, to) match {
      case (e: Edge, _: Turn) => {
        chosen_turns -= e
        if (e.road == path.head) {
          path = path.tail
        } else {
          throw new Exception(
            s"Route not being followed! $from -> $to happened, with path $path"
          )
        }
      }
      case (e: Edge, _: Edge) => {
        chosen_turns -= e
      }
      case _ =>
    }
    owner.sim.publish(EV_Transition(owner, from, to), owner)
  }

  override def request_reroute(at: Edge) {
    Util.assert_ne(at.road, goal)
    chosen_turns -= at
    reroutes_requested += at
  }

  override def flush_reroute(at: Edge) {
    Util.assert_ne(at.road, goal)
    Util.assert_eq(reroutes_requested.contains(at), true)
    // If it fails, no worries
    perform_reroute(at, path.takeWhile(r => r != at.road), false)
  }

  def pick_turn(e: Edge): Turn = {
    // Just lookup if we've already committed to something.
    // TODO ultimately, our clients should ask us less and look at tickets.
    if (chosen_turns.contains(e)) {
      return chosen_turns(e)
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in the path.
    val (before, slice) = path.span(r => r != e.road)
    Util.assert_eq(slice.nonEmpty, true)
    val dest = slice.tail.head

    // Is the next step reachable?
    val must_reroute = e.next_turns.filter(t => t.to.road == dest).isEmpty
    // This variant only considers long roads capable of being congested, which is risky...
    val should_reroute = dest.road_agent.congested
    // Since short roads can gridlock too, have the client detect that and explicitly force us to
    // handle it
    val asked_to_reroute = reroutes_requested.contains(e)

    val turn = if (must_reroute || should_reroute || asked_to_reroute) {
      val success = perform_reroute(e, before, must_reroute)
      if (must_reroute) {
        Util.assert_eq(success, true)
      }
      val (_, rerouted_slice) = path.span(r => r != e.road)
      best_turn(e, rerouted_slice.tail.head, rerouted_slice.tail.tail.headOption.getOrElse(null))
    } else {
      best_turn(e, dest, slice.tail.tail.headOption.getOrElse(null))
    }
    chosen_turns(e) = turn
    return turn
  }

  def pick_final_lane(from: Edge): (Edge, Boolean) = {
    // We could be done!
    if (from.road == goal) {
      return (from, false)
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in the path.
    val slice = path.dropWhile(r => r != from.road)
    Util.assert_eq(slice.nonEmpty, true)

    val next_step = slice.tail.head
    // Find all lanes going to the next step.
    val candidates = candidate_lanes(from, next_step) match {
      case Nil => {
        throw new Exception(
          s"Other lanes around $from don't lead to $next_step!"
        )
      }
      case lanes => lanes
    }

    // Pick the lane closest to the current
    //return candidates.minBy(e => math.abs(from.lane_num - e.lane_num))

    // TODO lookahead a bit to lane-change early

    // Discretionary lane-changing: pick the lane with the fewest people ahead of us
    return (candidates.minBy(e => owner.num_ahead(e)), !candidates.contains(from))
  }

  // Returns true if rerouting succeeds
  private def perform_reroute(at: Edge, slice_before: List[Road], must_reroute: Boolean): Boolean = {
    reroutes_requested -= at

    // A problem: if we try to avoid looping back to roads at the start of the path, then some paths
    // aren't possible.
    // Ban roads in the prefix of our path, since drivers looping around to try to do LCing stuff is
    // buggy and funky.
    val banned =
      if (!must_reroute)
        (at.road :: slice_before).toSet
      else
        Set[Road]()

    // Re-route, but start from a source we can definitely reach without lane-changing.
    val choice = at.next_turns.maxBy(t => t.to.queue.percent_avail)
    val source = choice.to.road

    try {
      // TODO Erase all turn choices excluding slice_before stuff, if we've made any?
      val old_path = path
      path = slice_before ++ (at.road :: rerouter.path(Pathfind(start = source, goals = Set(goal), banned_nodes = banned)))
      owner.sim.publish(
        EV_Reroute(owner, path, false, rerouter.router_type, must_reroute, old_path), owner
      )
      return true
    } catch {
      // Couldn't A* due to constraints, but that's alright
      case e: Exception => {
        Util.assert_eq(banned.nonEmpty, true)
        return false
      }
    }
  }

  private def mandatory_reroute(at: Edge, slice_before: List[Road]) {
    // TODO Erase all turn choices excluding slice_before stuff, if we've made any?
    val old_path = path
    path = slice_before ++ (at.road :: rerouter.path(Pathfind(
      // Start from a source we can definitely reach without lane-changing.
      start = at.next_turns.maxBy(t => t.to.queue.percent_avail).to.road,
      goals = Set(goal)
      // Don't ban any roads from the path. If we wind up looping back on something, then for now,
      // so be it.
    )))
    owner.sim.publish(
      EV_Reroute(owner, path, false, rerouter.router_type, true, old_path), owner
    )
  }

  private def optional_reroute(at: Edge, slice_before: List[Road]) {
    // TODO Erase all turn choices excluding slice_before stuff, if we've made any?
    val old_path = path
    path = slice_before ++ rerouter.path(Pathfind(
        start = at.road,
        goals = Set(goal),
        // Don't hit anything already in our path
        banned_nodes = (at.road :: slice_before).toSet
        // Let the algorithm pick the best next step
      ).first_succs(at.next_roads)
    )
    owner.sim.publish(
      EV_Reroute(owner, path, false, rerouter.router_type, false, old_path), owner
    )
  }

  override def set_debug(value: Boolean) {
    debug_me = value
    orig_router.set_debug(value)
    rerouter.set_debug(value)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type = RouteType.Path
  def dump_info() {
    Util.log(s"Path route to $goal using $path")
  }

  def current_path = path
  def next_roads(num: Int) = path.take(num)

  // Prefer the one that's emptiest now and try to get close to a lane that
  // we'll want to LC to anyway. Only call when we haven't chosen something yet.
  private def best_turn(e: Edge, dest: Road, next_dest: Road): Turn = {
    val ideal_lanes =
      if (next_dest != null)
        candidate_lanes(dest.rightmost, next_dest)
      else
        dest.lanes.toList
    def ideal_dist(e: Edge) =
      ideal_lanes.map(ideal => math.abs(e.lane_num - ideal.lane_num)).min
    val total = dest.lanes.size
    def ranking(t: Turn) =
      if (t.to.road != dest)
        -1
      else
        // How far is this lane from an ideal lane?
        (total - ideal_dist(t.to)).toDouble / total.toDouble

    // Prefer things close to where we'll need to go next, and things with more
    // room.
    return e.next_turns.maxBy(
      t => (ranking(t), t.to.queue.percent_avail)
    )
  }

  private def candidate_lanes(from: Edge, dest: Road) =
    from.other_lanes.filter(f => f.succs.exists(t => t.road == dest)).toList
}

// Responsible for requesting reroutes
abstract class ReroutePolicy(a: Agent) {
  protected var should_reroute = false
  def react() {
    if (should_reroute) {
      should_reroute = false

      // Find the first edge for which the driver has no tickets, so we don't have to cancel
      // anything
      var at = a.at.on
      while (true) {
        at match {
          case e: Edge => {
            if (e.road == a.route.goal) {
              return
            }
            a.get_ticket(e) match {
              case Some(ticket) => at = ticket.turn
              case None => {
                // Reroute from there
                a.route.request_reroute(e)
                a.route.flush_reroute(e)
                return
              }
            }
          }
          case t: Turn => at = t.to
        }
      }
    }
  }
}

class NeverReroutePolicy(a: Agent) extends ReroutePolicy(a)

// Susceptible to perpetual oscillation
class RegularlyReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private var roads_crossed = 1
  private val reroute_frequency = 5

  a.sim.listen(classOf[EV_Transition], a, _ match {
    case EV_Transition(_, from, to: Turn) => {
      roads_crossed += 1
      if (roads_crossed % reroute_frequency == 0) {
        should_reroute = true
      }
    }
    case _ =>
  })
}

// Reroute when a road in our path is raised drastically
// TODO also try subscribing to changes in individual roads
class PriceChangeReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private val rescan_time = 30
  private val cost_ratio_threshold = 1.5

  private var total_orig_cost = 0.0
  private var start_countdown = false
  private var rescan_countdown = rescan_time

  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => start_countdown = true
  })

  override def react() {
    // The driver rerouted, so scan the new cost and set a timer to check on things
    if (start_countdown) {
      total_orig_cost = calc_route_cost
      rescan_countdown = rescan_time
      start_countdown = false
    }

    // Check to see if the route's cost has changed significantly
    if (rescan_countdown == 0) {
      if (calc_route_cost / total_orig_cost > cost_ratio_threshold) {
        should_reroute = true
        rescan_countdown = rescan_time
        super.react()
      }
    }
    rescan_countdown -= 1
  }

  // TODO remember cost of route from A* instead of recalculating it
  private def calc_route_cost(): Double = {
    var cost = 0.0
    var eta = a.sim.tick
    for (r <- a.route.current_path) {
      cost += r.road_agent.tollbooth.toll(eta).dollars
      eta += r.freeflow_time
    }
    return cost
  }
}
