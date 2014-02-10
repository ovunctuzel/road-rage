// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import Function.tupled
import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.{Edge, Road, Traversable, Turn, Vertex, Graph, Router}
import utexas.aorta.sim.{EV_Transition, EV_Reroute}
import utexas.aorta.sim.make.{RouteType, RouterType, Factory}

import utexas.aorta.common.{Util, cfg, StateWriter, StateReader, TurnID, Serializable}

// TODO maybe unify the one class with the interface, or something. other routes were useless.

// Get a client to their goal by any means possible.
abstract class Route(val goal: Road) extends Serializable {
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  protected var owner: Agent = null
  protected var debug_me = false  // TODO just grab from owner?

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    w.ints(route_type.id, goal.id.int)
  }

  protected def unserialize(r: StateReader, graph: Graph) {}

  def setup(a: Agent) {
    owner = a
  }

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
  // our answer here. True if e doesn't already lead to next road, aka, the final lane is more than
  // recommended.
  def pick_final_lane(e: Edge): (Edge, Boolean)
  // Just mark that we don't have to take the old turn prescribed
  def reroute(at: Edge) {}

  def set_debug(value: Boolean) {
    debug_me = value
  }

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
      RouteType(r.int), graph, RouterType.Fixed, RouterType.Fixed, graph.roads(r.int), Nil
    )
    route.unserialize(r, graph)
    return route
  }
}

// Follow routes prescribed by routers. Only reroute when forced or encouraged to.
// TODO rerouter only var due to serialization
class PathRoute(goal: Road, orig_router: Router, private var rerouter: Router) extends Route(goal) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  private var first_time = true
  // Head is the current step. If that step isn't immediately reachable, we have
  // to re-route.
  private var path: List[Road] = Nil
  private val chosen_turns = new mutable.ImmutableMapAdaptor(new immutable.TreeMap[Edge, Turn]())
  private val reroutes_requested = new mutable.TreeSet[Edge]()

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    w.bool(first_time)
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
    first_time = r.bool
    rerouter = Factory.make_router(RouterType(r.int), graph, Nil)
    path = Range(0, r.int).map(_ => graph.roads(r.int)).toList
    val chosen_size = r.int
    for (i <- Range(0, chosen_size)) {
      chosen_turns(graph.edges(r.int)) = graph.turns(new TurnID(r.int))
    }
    Range(0, r.int).map(_ => reroutes_requested += graph.edges(r.int))
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
    val dest = slice.tail.headOption

    // Is the next step reachable?
    val must_reroute = dest match {
      case Some(d) => e.next_turns.filter(t => t.to.road == d).isEmpty
      // Our router has only given us a piece of the path
      case None => true
    }
    // This variant only considers long roads capable of being congested, which is risky...
    val should_reroute = dest.map(_.auditor.congested).getOrElse(false)
    // Since short roads can gridlock too, have the client detect that and explicitly force us to
    // handle it
    val asked_to_reroute = reroutes_requested.contains(e)

    val best =
      if (must_reroute || should_reroute || asked_to_reroute)
        perform_reroute(e, before, must_reroute)
      else
        best_turn(e, dest.get, slice.tail.tail.headOption.getOrElse(null))
    chosen_turns(e) = best
    return best
  }

  def pick_final_lane(from: Edge): (Edge, Boolean) = {
    // We could be done!
    if (from.road == goal) {
      return (from, false)
    }

    // This method is called first, so do the lazy initialization here.
    if (first_time) {
      Util.assert_eq(path.isEmpty, true)
      first_time = false
      val new_path = from.road :: orig_router.path(from.road, goal, owner.sim.tick)
      owner.sim.publish(EV_Reroute(owner, new_path, true, orig_router.router_type, false, path))
      path = new_path
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in
    // the path.
    val pair = path.span(r => r != from.road)
    val slice = pair._2
    Util.assert_eq(slice.nonEmpty, true)

    val next_step = slice.tail.headOption match {
      case Some(r) => r
      // Our router hasn't told us everything yet
      case None => perform_reroute(from, pair._1, true).to.road
    }

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

  // Returns the turn we must make from at to continue down the new route
  private def perform_reroute(at: Edge, slice_before: List[Road], must_reroute: Boolean): Turn = {
    reroutes_requested -= at

    // TODO only do rerouting of the ENTIRE path
    // For now, don't do optional rerouting in the middle of the path
    if (!must_reroute && slice_before.nonEmpty) {
      val next_road = path.span(r => r != at.road)._2.tail.head
      return at.next_turns.find(t => t.to.road == next_road).get
    }

    // Re-route, but start from a source we can definitely reach without
    // lane-changing.
    val choice = at.next_turns.maxBy(t => t.to.queue.percent_avail)
    val source = choice.to.road

    // TODO Erase all turn choices AFTER source, if we've made any?

    // Stitch together the new path into the full thing.
    val new_path =
      slice_before ++ (at.road :: source :: rerouter.path(source, goal, owner.sim.tick))
    owner.sim.publish(EV_Reroute(owner, new_path, false, rerouter.router_type, must_reroute, path))
    path = new_path
    return choice
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

  def roads = path.toSet

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
