package utexas.sim

import utexas.map.{Edge, Turn, Traversable}
import utexas.{Util, cfg}

abstract class Behavior(a: Agent) {
  val graph = a.graph

  // get things rollin'
  def set_goal(e: Edge)
  // asked every tick after everybody has moved
  def choose_action(): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
  def done_with_route: Boolean
  // every time the agent moves to a new traversable
  def transition(from: Traversable, to: Traversable)
  // just for debugging
  def dump_info()
}

// Never speeds up from rest, so effectively never does anything
class IdleBehavior(a: Agent) extends Behavior(a) {
  override def set_goal(e: Edge) = {} // we don't care

  override def choose_action() = Act_Set_Accel(0)

  override def choose_turn(e: Edge): Turn = {
    // lol @ test behavior
    if (e.next_turns.size == 0) {
      // TODO fix this in the map properly.
      Util.log("wtf @ " + e)
      return e.prev_turns.head
    }
    return e.next_turns.head
  }

  override def done_with_route = true

  override def transition(from: Traversable, to: Traversable) = {}   // mmkay

  override def dump_info() = {
    Util.log("Idle behavior")
  }
}

// Pathfinding somewhere spatially and proceeds to clobber through agents
class AutonomousBehavior(a: Agent) extends Behavior(a) {
  // route.head is always our next move
  var route: List[Traversable] = List[Traversable]()
  // Just to force collisions to almost happen
  val our_speed = Util.mph_to_si(Util.rand_double(5, 35))
  // Once we start stopping for the end of an edge, keep doing so, even if it
  // seems like our stopping distance is getting fine again.
  // TODO explicit state machine might work better
  var keep_stopping = false
  // Remember if we're polling a new intersection or not
  var first_request = true

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

    // Plow ahead! (not through cars, hopefully)
    val a1 = safe_accel

    return Act_Set_Accel(
      if (a1 >= 0)
        // never go faster than our own random limit, to make it more fun
        math.min(a1, accel_to_achieve(our_speed))
      else
        a1
    )
  }

  override def choose_turn(e: Edge): Turn = route match {
    case ((t: Turn) :: rest) => t
    case _                 => throw new Exception("Asking us to choose turn now?!")
  }

  override def done_with_route = route.size == 0

  override def transition(from: Traversable, to: Traversable) = {
    if (route.head == to) {
      route = route.tail      // moving right along
      keep_stopping = false   // always reset
      first_request = true
    } else {
      throw new Exception("We missed a move!")
    }
  }

  override def dump_info() = {
    Util.log("Autonomous behavior")
    Util.log("Route:")
    Util.log_push
    route.foreach(s => Util.log("" + s))
    Util.log_pop
  }

  // TODO implement piyush's optimal stopping. decide to stop once, set a speed
  // that accounts for the lag once, then mash at max_accel towards 0.

  // This is defined by several things, in some order: the agent in front of us now,
  // the agent in front of us at the way we're going, whether the intersection
  // is ready for us, speed limits of current and future edge
  def safe_accel(): Double = {
    val speed_limit = Util.mph_to_si(30)  // TODO ask the road or whatever

    val follow_agent_cur = a.cur_queue.ahead_of(a)
    val follow_agent_next = a.at match {
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
      case Position(e: Edge, _) => Agent.sim.intersections(e.to).should_stop(
                                     a, next_turn.get, first_request
                                   )
    }
    first_request = false   // this is reset once we move to a new edge
    // Then be consistent with stopping, once we decide to!
    // threshold accounts for time lag
    val threshold = a.speed * cfg.max_dt
    if (should_stop_at_end && !keep_stopping && a.stopping_distance >= a.at.dist_left - threshold) {
      keep_stopping = true
    }
    val stop_at_end = should_stop_at_end && keep_stopping

    val set_accel = (follow_agent_cur, follow_agent_next) match {
      // First see if there's somebody currently in front of us.
      case (Some(follow: Agent), _)     => accel_to_follow(follow)
      // Next try and find somebody on the edge we're about to be at
      case (None, Some(follow: Agent))  => accel_to_follow(follow)
      // No? Plow ahead!
      case _                           => accel_to_achieve(speed_limit)
    }

    val choice = if (stop_at_end)
                   // it's always conservative to slow down more
                   math.min(set_accel, accel_to_end)
                 else
                   set_accel
    // and we can never exceed our possibilities
    return if (choice >= 0)
             math.min(choice, a.max_accel)
           else
             math.max(choice, -a.max_accel)
  }

  private def accel_to_follow(follow: Agent): Double = {
    assert(follow.at.on != a.at.on || follow.at.dist > a.at.dist)

    // Maintain our stopping distance away from this guy, plus don't scrunch
    // together too closely...
    // TODO we wind up with negative speeds! not stopping well enough!
    val our_stop_dist = a.stopping_distance //+ cfg.follow_dist

    // How far away are we?
    // TODO assume we're at most one traversable away (since we could be trying
    // to avoid the person on the edge we're headed towards
    // TODO they might not be on the same traversable, in which case we need the
    // distance between us and them properly
    val dist_from_them = if (a.at.on == follow.at.on)
                           follow.at.dist - a.at.dist
                         else
                           a.at.dist_left + follow.at.dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = dist_from_them - our_stop_dist

    /*if (delta_dist <= 0.0) {
      Util.log(a + " cover neg dist " + delta_dist + " with accel " + accel_to_cover(delta_dist))
    }*/

    // TODO make sure it can handle negatives...
    return if (delta_dist > 0)
      // don't go too fast, either
      math.min(a.max_accel, accel_to_cover(delta_dist))
    else
      accel_to_cover(delta_dist)
  }

  private def accel_to_end = accel_to_cover(a.at.dist_left * 0.5)
  // TODO this is based on piyush's proof, but it doesn't quite work yet.
  /*private def accel_to_end(): Double = {
    if (a.stopping_distance >= a.at.dist_left) {
      Util.log("DOOMED?")
    }
    Util.log("stop dist " + a.stopping_distance + ", left " + a.at.dist_left) 

    // TODO -max_accel?
    // a, b, c for a normal quadratic
    val q_a = 1 / a.max_accel
    // TODO maybe this dt, not max? why?
    val q_b = cfg.max_dt
    val q_c = (a.speed * cfg.max_dt) - (2 * a.at.dist_left)
    // TODO the other soln?
    val desired_speed = (-q_b + math.sqrt((q_b * q_b) - (4 * q_a * q_c))) / (2 * q_a)

    Util.log("want speed " + a.speed + " -> " + desired_speed)
    // TODO eh.. the max.
    return (math.max(0, desired_speed) - a.speed) / cfg.max_dt
  }*/

  // This directly follows from the distance traveled at constant accel
  private def accel_to_cover(dist: Double) =
    2 * (dist - (a.speed * cfg.max_dt)) / (cfg.max_dt * cfg.max_dt)
  def accel_to_achieve(target_speed: Double) = Util.accel_to_achieve(a.speed, target_speed)
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
