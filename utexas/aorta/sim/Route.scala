// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import Function.tupled
import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.{Edge, Road, Traversable, Turn, Vertex, Graph}
import utexas.aorta.map.analysis.Router
import utexas.aorta.sim.make.{RouteType, RouterType, Factory}

import utexas.aorta.common.{Util, RNG, Common, cfg, StateWriter, StateReader, TurnID,
                            ListenerPattern}

// TODO maybe unify the one class with the interface, or something. other routes were useless.

// Get a client to their goal by any means possible.
abstract class Route(val goal: Road, rng: RNG) extends ListenerPattern[Route_Event] {
  //////////////////////////////////////////////////////////////////////////////
  // State

  protected var agent: Agent = null

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.int(route_type.id)
    w.int(goal.id.int)
    rng.serialize(w)
  }

  def setup(a: Agent) {
    agent = a
  }

  protected def unserialize(r: StateReader, graph: Graph) {}

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // For lookahead clients. No lane-changing.
  def steps_to(at: Traversable, v: Vertex): List[Traversable] = at match {
    case e: Edge if e.to == v => at :: Nil
    case e: Edge => at :: steps_to(pick_turn(e), v)
    case t: Turn => at :: steps_to(t.to, v)
  }

  // The client tells us they've physically moved
  def transition(from: Traversable, to: Traversable)
  // The client is being forced to pick a turn. If they ask us repeatedly, we
  // have to always return the same answer.
  def pick_turn(e: Edge): Turn
  // Prescribe the final lane on this road to aim for. We should be able to spazz around in
  // our answer here.
  def pick_final_lane(e: Edge): Edge
  // Just mark that we don't have to take the old turn prescribed
  def reroute(at: Edge) {}

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type(): RouteType.Value
  def done(at: Edge) = at.road == goal
  def dump_info()
}

object Route {
  def unserialize(r: StateReader, graph: Graph): Route = {
    // Original router will never be used again, and rerouter will have to be reset by PathRoute.
    val route = Factory.make_route(
      RouteType(r.int), graph, RouterType.Fixed, RouterType.Fixed, graph.roads(r.int),
      RNG.unserialize(r), Nil
    )
    route.unserialize(r, graph)
    return route
  }
}

abstract class Route_Event
final case class EV_Transition(from: Traversable, to: Traversable) extends Route_Event
// orig = true when initializing the path. bit of a hack.
// if unrealizable, then couldn't follow path. if not, congestion or gridlock.
final case class EV_Reroute(
  path: List[Road], orig: Boolean, method: RouterType.Value, unrealizable: Boolean
) extends Route_Event

// Follow routes prescribed by routers. Only reroute when forced or encouraged to.
// TODO rerouter only var due to serialization
class PathRoute(goal: Road, orig_router: Router, private var rerouter: Router, rng: RNG)
  extends Route(goal, rng)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Head is the current step. If that step isn't immediately reachable, we have
  // to re-route.
  private var path: List[Road] = Nil
  private val chosen_turns = new mutable.ImmutableMapAdaptor(new immutable.TreeMap[Edge, Turn]())
  private val reroutes_requested = new mutable.HashSet[Edge]()

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.int(rerouter.router_type.id)
    // We can't tell when we last rerouted given less state; store the full
    // path.
    if (path == null) {
      w.int(0)
    } else {
      w.int(path.size)
      path.foreach(step => w.int(step.id.int))
    }
    w.int(chosen_turns.size)
    chosen_turns.foreach(tupled((e, t) => {
      w.int(e.id.int)
      w.int(t.id.int)
    }))
    w.int(reroutes_requested.size)
    reroutes_requested.foreach(e => w.int(e.id.int))
  }

  override def unserialize(r: StateReader, graph: Graph) {
    rerouter = Factory.make_router(RouterType(r.int), graph, Nil)
    val path_size = r.int
    // Leave null otherwise
    if (path_size > 0) {
      path = Range(0, path_size).map(_ => graph.roads(r.int)).toList
    }
    val chosen_size = r.int
    for (i <- Range(0, chosen_size)) {
      chosen_turns(graph.edges(r.int)) = graph.turns(new TurnID(r.int))
    }
    Range(0, r.int).map(_ => reroutes_requested += graph.edges(r.int))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def transition(from: Traversable, to: Traversable) = {
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
    tell_listeners(EV_Transition(from, to))
  }

  override def reroute(at: Edge) {
    Util.assert_eq(chosen_turns.contains(at), true)
    chosen_turns -= at
    reroutes_requested += at
  }

  def pick_turn(e: Edge): Turn = {
    // Just lookup if we've already committed to something.
    // TODO ultimately, our clients should ask us less and look at tickets.
    if (chosen_turns.contains(e)) {
      return chosen_turns(e)
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in
    // the path.
    val pair = path.span(r => r != e.road)
    val before = pair._1
    val slice = pair._2
    Util.assert_eq(slice.nonEmpty, true)
    val dest = slice.tail.head

    // Is the next step reachable?
    val must_reroute =
      e.next_turns.filter(t => t.to.road == dest).isEmpty
    // This variant only considers long roads capable of being congested, which is risky...
    val should_reroute = dest.auditor.congested
    // Since short roads can gridlock too, have the client detect that and explicitly force us to
    // handle it
    val asked_to_reroute = reroutes_requested.contains(e)

    val best = if (must_reroute || should_reroute || asked_to_reroute) {
      reroutes_requested -= e
      // Re-route, but start from a source we can definitely reach without
      // lane-changing.
      val choice = e.next_turns.maxBy(t => t.to.queue.percent_avail)
      val source = choice.to.road

      // TODO Erase all turn choices AFTER source, if we've made any?

      // Stitch together the new path into the full thing.
      val new_path = slice.head :: source :: rerouter.path(source, goal, Common.tick)
      path = before ++ new_path
      tell_listeners(EV_Reroute(path, false, rerouter.router_type, must_reroute))
      choice
    } else {
      best_turn(e, dest, slice.tail.tail.headOption.getOrElse(null))
    }
    chosen_turns(e) = best
    return best
  }

  def pick_final_lane(from: Edge): Edge = {
    // This method is called first, so do the lazy initialization here.
    if (path.isEmpty) {
      path = from.road :: orig_router.path(from.road, goal, Common.tick)
      tell_listeners(EV_Reroute(path, true, orig_router.router_type, false))
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in
    // the path.
    val pair = path.span(r => r != from.road)
    val slice = pair._2
    Util.assert_eq(slice.nonEmpty, true)

    // We could be done!
    if (slice.tail.isEmpty) {
      Util.assert_eq(from.road, goal)
      return from
    }

    // Find all lanes going to the next step.
    val candidates = candidate_lanes(from, slice.tail.head) match {
      case Nil => {
        throw new Exception(
          s"Other lanes around $from don't lead to ${slice.tail.head}!"
        )
      }
      case lanes => lanes
    }

    // Pick the lane closest to the current
    //return candidates.minBy(e => math.abs(from.lane_num - e.lane_num))

    // Discretionary lane-changing: pick the lane with the least congestion
    return candidates.minBy(e => e.queue.percent_full)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type = RouteType.Path
  def dump_info() {
    Util.log(s"Static route to $goal using $path")
  }

  def roads = path.toSet

  // Prefer the one that's emptiest now and try to get close to a lane that
  // we'll want to LC to anyway. Only call when we haven't chosen something yet.
  private def best_turn(e: Edge, dest: Road, next_dest: Road): Turn = {
    val ideal_lanes =
      if (next_dest != null)
        candidate_lanes(dest.edges.head, next_dest)
      else
        dest.edges.toList
    def ideal_dist(e: Edge) =
      ideal_lanes.map(ideal => math.abs(e.lane_num - ideal.lane_num)).min
    val total = dest.edges.size
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
