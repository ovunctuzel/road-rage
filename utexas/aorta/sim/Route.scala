// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import Function.tupled
import scala.collection.immutable
import scala.collection.mutable

import utexas.aorta.map.{Edge, DirectedRoad, Traversable, Turn, Vertex, Graph}
import utexas.aorta.map.analysis.{Router, DijkstraRouter}
import utexas.aorta.sim.make.{RouteType, RouterType, Factory}

import utexas.aorta.common.{Util, RNG, Common, cfg, StateWriter, StateReader, TurnID,
                            ListenerPattern}

// Get a client to their goal by any means possible.
abstract class Route(val goal: DirectedRoad, rng: RNG) extends ListenerPattern[Route_Event] {
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
  // Prescribe the final lane on this directed road to aim for. We should be able to spazz around in
  // our answer here.
  def pick_final_lane(e: Edge): Edge
  // Just mark that we don't have to take the old turn prescribed
  def reroute(at: Edge) {}

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type(): RouteType.Value
  def done(at: Edge) = at.directed_road == goal
  def dump_info()
}

object Route {
  def unserialize(r: StateReader, graph: Graph): Route = {
    // Original router will never be used again, and rerouter will have to be reset by PathRoute.
    val route = Factory.make_route(
      RouteType(r.int), graph, RouterType.Fixed, RouterType.Fixed, graph.directed_roads(r.int),
      RNG.unserialize(r), Nil
    )
    route.unserialize(r, graph)
    return route
  }
}

abstract class Route_Event
final case class EV_Transition(from: Traversable, to: Traversable) extends Route_Event
// orig = true when initializing the path. bit of a hack.
final case class EV_Reroute(path: List[DirectedRoad], orig: Boolean, method: RouterType.Value)
  extends Route_Event

// Compute the cost of the path from every source to our single goal, then
// hillclimb each step.
// TODO rename 'cost' router? hillclimber?
class DijkstraRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  private val costs = new DijkstraRouter(Common.sim.graph).costs_to(goal)

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // We don't care.
  def transition(from: Traversable, to: Traversable) = {}

  def pick_turn(e: Edge) = e.next_turns.minBy(t => costs(t.to.directed_road.id.int))
  // Break ties for the best lane overall by picking the lane closest to the
  // current.
  def pick_final_lane(from: Edge) = from.other_lanes.minBy(
    e => (costs(pick_turn(e).to.directed_road.id.int), math.abs(from.lane_num - e.lane_num))
  )

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type = RouteType.Dijkstra
  def dump_info() {
    Util.log(s"Static route to $goal")
  }
}

// Follow routes prescribed by routers. Only reroute when forced or encouraged to.
// TODO rerouter only var due to serialization
class PathRoute(goal: DirectedRoad, orig_router: Router, private var rerouter: Router, rng: RNG)
  extends Route(goal, rng)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Head is the current step. If that step isn't immediately reachable, we have
  // to re-route.
  private var path: List[DirectedRoad] = Nil
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

  override protected def unserialize(r: StateReader, graph: Graph) {
    rerouter = Factory.make_router(RouterType(r.int), graph, Nil)
    val path_size = r.int
    // Leave null otherwise
    if (path_size > 0) {
      path = Range(0, path_size).map(_ => graph.directed_roads(r.int)).toList
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
        if (e.directed_road == path.head) {
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
    val pair = path.span(r => r != e.directed_road)
    val before = pair._1
    val slice = pair._2
    Util.assert_eq(slice.nonEmpty, true)
    val dest = slice.tail.head

    // Is the next step reachable?
    val must_reroute =
      e.next_turns.filter(t => t.to.directed_road == dest).isEmpty
    // This variant only considers long roads capable of being congested, which is risky...
    val should_reroute = dest.is_congested
    // Since short roads can gridlock too, have the client detect that and explicitly force us to
    // handle it
    val asked_to_reroute = reroutes_requested.contains(e)

    val best = if (must_reroute || should_reroute || asked_to_reroute) {
      reroutes_requested -= e
      // Re-route, but start from a source we can definitely reach without
      // lane-changing.
      val choice = e.next_turns.maxBy(t => t.to.queue.percent_avail)
      val source = choice.to.directed_road

      // TODO Erase all turn choices AFTER source, if we've made any?

      // Stitch together the new path into the full thing.
      val new_path = slice.head :: source :: rerouter.path(source, goal, Common.tick)
      path = before ++ new_path
      tell_listeners(EV_Reroute(path, false, rerouter.router_type))
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
      path = from.directed_road :: orig_router.path(from.directed_road, goal, Common.tick)
      tell_listeners(EV_Reroute(path, true, orig_router.router_type))
    }

    // Lookahead could be calling us from anywhere. Figure out where we are in
    // the path.
    val pair = path.span(r => r != from.directed_road)
    val slice = pair._2
    Util.assert_eq(slice.nonEmpty, true)

    // We could be done!
    if (slice.tail.isEmpty) {
      Util.assert_eq(from.directed_road, goal)
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

  def roads = path.map(_.road).toSet

  // Prefer the one that's emptiest now and try to get close to a lane that
  // we'll want to LC to anyway. Only call when we haven't chosen something yet.
  private def best_turn(e: Edge, dest: DirectedRoad, next_dest: DirectedRoad): Turn = {
    val ideal_lanes =
      if (next_dest != null)
        candidate_lanes(dest.edges.head, next_dest)
      else
        dest.edges.toList
    def ideal_dist(e: Edge) =
      ideal_lanes.map(ideal => math.abs(e.lane_num - ideal.lane_num)).min
    val total = dest.edges.size
    def ranking(t: Turn) =
      if (t.to.directed_road != dest)
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

  private def candidate_lanes(from: Edge, dest: DirectedRoad) =
    from.other_lanes.filter(f => f.succs.exists(t => t.directed_road == dest)).toList
}

// DOOMED TO WALK FOREVER (until we happen to reach our goal)
class DrunkenRoute(goal: DirectedRoad, rng: RNG) extends Route(goal, rng) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // Remember answers we've given for the sake of consistency
  private var desired_lane: Option[Edge] = None
  private val chosen_turns = new mutable.ImmutableMapAdaptor(new immutable.TreeMap[Edge, Turn]())

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    desired_lane match {
      case Some(e) => w.int(e.id.int)
      case None => w.int(-1)
    }
    chosen_turns.foreach(tupled((e, t) => {
      w.int(e.id.int)
      w.int(t.id.int)
    }))
  }

  override protected def unserialize(r: StateReader, graph: Graph) {
    r.int match {
      case x if x != -1 => desired_lane = Some(graph.edges(x))
      case _ =>
    }
    val chosen_size = r.int
    for (i <- Range(0, chosen_size)) {
      chosen_turns(graph.edges(r.int)) = graph.turns(new TurnID(r.int))
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // Forget what we've remembered
  def transition(from: Traversable, to: Traversable) = {
    (from, to) match {
      case (t: Turn, e: Edge) => {
        desired_lane = None
      }
      case (e: Edge, t: Turn) => {
        chosen_turns.remove(e)
      }
      case _ =>
    }
  }

  def pick_turn(e: Edge) = chosen_turns.getOrElseUpdate(e, choose_turn(e))

  def pick_final_lane(e: Edge): Edge = {
    if (!desired_lane.isDefined) {
      desired_lane = Some(rng.choose(e.other_lanes))
    }
    return desired_lane.get
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def route_type = RouteType.Drunken
  def dump_info() {
    Util.log(s"Drunken route to $goal")
    Util.log(s"  Desired lane: $desired_lane")
    Util.log(s"  Chosen turns: $chosen_turns")
  }

  // With the right amount of alcohol, a drunk can choose uniformly at random
  protected def choose_turn(e: Edge) = rng.choose(e.next_turns)
}

// Wanders around slightly less aimlessly by picking directions
class DirectionalDrunkRoute(goal: DirectedRoad, rng: RNG)
  extends DrunkenRoute(goal, rng)
{
  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // pick the most direct path 75% of the time
  override def choose_turn(e: Edge) =
    if (rng.percent(.75))
      e.next_turns.minBy(heuristic)
    else
      super.choose_turn(e)

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def route_type = RouteType.DirectionalDrunk

  protected def heuristic(t: Turn) = t.to.to.location.dist_to(goal.start_pt)
}

// Don't keep making the same choices for roads
class DrunkenExplorerRoute(goal: DirectedRoad, rng: RNG)
  extends DirectionalDrunkRoute(goal, rng)
{
  //////////////////////////////////////////////////////////////////////////////
  // State

  private val past = new mutable.ImmutableMapAdaptor(new immutable.TreeMap[DirectedRoad, Int]())

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  override def serialize(w: StateWriter) {
    super.serialize(w)
    past.foreach(tupled((dr, count) => {
      w.int(dr.id.int)
      w.int(count)
    }))
  }

  override protected def unserialize(r: StateReader, graph: Graph) {
    val past_size = r.int
    for (i <- Range(0, past_size)) {
      past(graph.directed_roads(r.int)) = r.int
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  override def choose_turn(e: Edge): Turn = {
    // break ties by the heuristic of distance to goal
    val choice = e.next_turns.minBy(turn => (
      past.getOrElse(turn.to.directed_road, -1) + rng.int(0, 5),
      heuristic(turn)
    ))
    val road = choice.to.directed_road
    past(road) = past.getOrElse(road, 0) + 1
    return choice
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def route_type = RouteType.DrunkenExplorer
}
