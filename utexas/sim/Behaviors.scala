package utexas.sim

import utexas.map.{Edge, Turn, Traversable}
import utexas.Util

abstract class Behavior(a: Agent) {
  val graph = a.graph

  // get things rollin'
  def set_goal(e: Edge)
  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex. None means disappear.
  def choose_turn(e: Edge): Option[Turn]
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  override def set_goal(e: Edge) = {} // we don't care

  override def choose_action() = Act_Set_Speed(0)

  override def choose_turn(e: Edge): Option[Turn] = {
    // lol @ test behavior
    if (e.next_turns.size == 0) {
      // TODO fix this in the map properly.
      Util.log("wtf @ " + e)
      return Some(e.prev_turns.head)
    }
    return Some(e.next_turns.head)
  }

  override def transition(from: Traversable, to: Traversable) = {}   // mmkay

  override def dump_info() = {
    Util.log("Idle behavior")
  }
}

// Pathfinding somewhere spatially and proceeds to clobber through agents
class DangerousBehavior(a: Agent) extends Behavior(a) {
  // route.head is always our next move
  var route: List[Traversable] = List[Traversable]()
  // Just to force collisions to almost happen
  val our_speed = Util.mph_to_si(Util.rand_double(5, 35))
  // Once we start stopping for the end of an edge, keep doing so, even if it
  // seems like our stopping distance is getting fine again.
  // TODO explicit state machine might work better
  var keep_stopping = false

  override def set_goal(to: Edge): Unit = a.at.on match {
    case e: Edge => { route = graph.pathfind_astar(e, to) }
    case _       => throw new Exception("Start agent on an edge to do stuff!")
  }

  override def choose_action(): Action = {
    // We disappear in choose_turn, so when route is empty here, just keep
    // moving so we can reach the end of our destination edge.
    if (route.size != 0) {
      // Time to lane-change?
      // TODO choose the right time for it, once we have a model for lane-changing
      (a.at, route.head) match {
        case (Position(e1: Edge, _), e2: Edge) => {
          return Act_Lane_Change(e2)
        }
        case _ =>
      }
    }

    // Plow ahead! (through cars)
    return Act_Set_Speed(math.min(our_speed, max_safe_speed))
  }

  override def choose_turn(e: Edge): Option[Turn] = {
    // Done!
    if (route.size == 0) {
      return None
    }

    route.head match {
      case t: Turn => {
        return Some(t)
      }
      case _ => throw new Exception("Asking us to choose a turn at the wrong time!")
    }
  }

  override def transition(from: Traversable, to: Traversable) = {
    if (route.head == to) {
      route = route.tail      // moving right along
      keep_stopping = false   // always reset
    } else {
      throw new Exception("We missed a move!")
    }
  }

  override def dump_info() = {
    Util.log("Dangerous behavior")
    Util.log("Route:")
    Util.log_push
    route.foreach(s => Util.log("" + s))
    Util.log_pop
  }

  // This is defined by several things, in some order: the agent in front of us now,
  // the agent in front of us at the way we're going, whether the intersection
  // is ready for us, speed limits of current and future edge
  def max_safe_speed(): Double = {
    val speed_limit = Util.mph_to_si(30)  // TODO ask the road or whatever

    val avoid_agent_cur = a.cur_queue.ahead_of(a)
    val avoid_agent_next = a.at match {
      case Position(t: Turn, _) => Agent.sim.queues(t.to)
      case _                    => None
    }
    val next_turn: Option[Turn] = route.headOption match {
      case Some(t: Turn) => Some(t)
      case _             => None
    }
    val should_stop_at_end = a.at match {
      // Keep going if we're currently doing a turn
      case Position(t: Turn, _) => false
      // Stop if we're arriving at our destination
      case (Position(e: Edge, _)) if route.size == 0 => true
      // Otherwise, let the intersection decide
      case Position(e: Edge, _) => Agent.sim.intersections(e.to).should_stop(a, next_turn.get)
    }
    // Then be consistent with stopping, once we decide to!
    // TODO do we need a threshold? depends on speed and timestep, i guess.
    if (should_stop_at_end && !keep_stopping && a.stopping_distance * 1.1 >= a.at.dist_left) {
      keep_stopping = true
    }
    val stop_at_end = should_stop_at_end && keep_stopping

    val set_speed = (avoid_agent_cur, avoid_agent_next) match {
      // First see if there's somebody currently in front of us.
      case (Some(avoid: Agent), _)     => speed_to_avoid(avoid)
      // Next try and find somebody on the edge we're about to be at
      case (None, Some(avoid: Agent))  => speed_to_avoid(avoid)
      // No? Plow ahead!
      case _                           => speed_limit
    }

    // We actually want to pay attention to a few constraints, since the next
    // agent may be doing something different.
    return if (stop_at_end)
             math.min(set_speed, speed_to_end)
           else
             set_speed
  }

  // TODO we could slow down more gradually and be fuel efficient.

  private def speed_to_avoid(avoid: Agent): Double = {
    assert(avoid.at.on != a.at.on || avoid.at.dist > a.at.dist)

    // Maintain our stopping distance away from this guy...
    val stop_dist = a.stopping_distance

    // How far away are we?
    // TODO assume we're at most one traversable away (since we could be trying
    // to avoid the person on the edge we're headed towards
    val dist_from_them = if (a.at.on == avoid.at.on)
                           avoid.at.dist - a.at.dist
                         else
                           a.at.dist_left + avoid.at.dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = dist_from_them - stop_dist

    // And we know we can react every cfg.max_dt at worst...

    // TODO figure this out
    return 0.8 * avoid.speed

    // TODO they might not be on the same traversable, in which case we need the
    // distance between us and them properly
  }

  // Make sure we stop at the end of this edge. Could slow down more gradually
  // later.
  private def speed_to_end = 0
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Speed(new_speed: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
