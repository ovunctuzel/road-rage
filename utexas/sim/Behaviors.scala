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

  override def choose_turn(e: Edge) = e.next_turns.head

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
    return Act_Set_Accel(safe_accel)
  }

  override def choose_turn(e: Edge): Turn = route match {
    case ((t: Turn) :: rest) => t
    case _                   => throw new Exception("Asking " + a + " to choose turn now?!")
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

  // This is defined by several things, in some order: the agent in front of us now,
  // the agent in front of us at the way we're going, whether the intersection
  // is ready for us, speed limits of current and future edge
  def safe_accel(): Double = {
    val speed_limit = Util.mph_to_si(30)  // TODO ask the road or whatever

    val follow_agent_cur = a.cur_queue.ahead_of(a)
    // TODO avoid the agent on the next traversable, whether that's a turn or an edge
    val follow_agent_next = a.at match {
      case Position(t: Turn, _) => Agent.sim.queues(t.to)
      case _                 => None
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
    // Since we can't react instantly, we have to consider the worst-case of the
    // next tick, which happens when we speed up as much as possible this tick.
    // If we would overshoot by then, then start braking now.
    val worst_speed = a.speed + (a.max_accel * cfg.dt_s)
    val worst_stop_dist = a.stopping_distance(worst_speed)
    val worst_travel = Util.dist_at_constant_accel(a.max_accel, cfg.dt_s, a.speed)
    //Util.log("we have " + a.at.dist_left + " left now. least left next is " + (a.at.dist_left - threshold) + ", and stop dist is " + a.stopping_distance)
    if (should_stop_at_end && !keep_stopping && worst_stop_dist + worst_travel >= a.at.dist_left) {
      // Then be consistent with stopping, once we decide to!
      keep_stopping = true
      //Util.log("  start stopping now")
    }
    val stop_at_end = should_stop_at_end && keep_stopping

    val set_accel = (follow_agent_cur, follow_agent_next) match {
      // First see if there's somebody currently in front of us.
      case (Some(follow: Agent), _)     => accel_to_follow(follow)
      // Next try and find somebody on the edge we're about to be at
      case (None, Some(follow: Agent))  => accel_to_follow(follow)
      // No? Plow ahead!
      case _                           => math.min(a.max_accel, accel_to_achieve(speed_limit))
    }

    return if (stop_at_end)
             // it's always conservative to slow down more
             math.min(set_accel, accel_to_end)
           else
             set_accel
  }

  private def accel_to_follow(follow: Agent): Double = {
    assert(follow.at.on != a.at.on || follow.at.dist > a.at.dist)

    // Maintain our stopping distance away from this guy, plus don't scrunch
    // together too closely...

    // How far away are we currently? Starting with this value lets us shove all
    // the complications of being on different traversables here. Doesn't matter
    // if 'follow' is about to cross to another traversable.
    // TODO assume we're at most one traversable away (since we could be trying
    // to avoid the person on the edge we're headed towards
    // TODO they might not be on the same traversable, in which case we need the
    // distance between us and them properly
    val dist_from_them_now = if (a.at.on == follow.at.on)
                               follow.at.dist - a.at.dist
                             else
                               a.at.dist_left + follow.at.dist

    // Again, reason about the worst-case: we speed up as much as possible, they
    // slam on their brakes.
    val us_worst_speed = a.speed + (a.max_accel * cfg.dt_s)
    val us_worst_stop_dist = a.stopping_distance(us_worst_speed)
    val most_we_could_go = Util.dist_at_constant_accel(a.max_accel, cfg.dt_s, a.speed)

    val least_they_could_go = Util.dist_at_constant_accel(-follow.max_accel, cfg.dt_s, follow.speed)

    // TODO this optimizes for next tick, so we're playing it really
    // conservative here... will that make us fluctuate more?
    val projected_dist_from_them = dist_from_them_now - most_we_could_go + least_they_could_go

    // don't ride bumper-to-bumper
    val desired_dist_btwn = us_worst_stop_dist + cfg.follow_dist

    // Positive = speed up, zero = go their speed, negative = slow down
    val delta_dist = projected_dist_from_them - desired_dist_btwn

    // TODO can we nix this?
    /*if (delta_dist <= 0.0) {
      Util.log(a + " cover neg dist " + delta_dist + " with accel " + accel_to_cover(delta_dist))
    }*/

    // Try to cover whatever the distance is, and cap off our values.
    // TODO pretty sure this handles both + and - deltas
    val accel = accel_to_cover(delta_dist)

    // TODO its a bit scary that this ever happens? does that mean we're too
    // close..?
    // Make sure we don't deaccelerate past 0 either.
    val accel_to_stop = accel_to_achieve(0)

    return if (accel > 0)
      math.min(a.max_accel, accel)
    else
      math.max(-a.max_accel, math.max(accel_to_stop, accel))
  }

  // This is based on Piyush's proof.
  private def accel_to_end(): Double = {
    // Just a simple short-circuit case.
    // TODO maybe not needed, and this fails for 1327896599636 on btr at 0.9 dt
    if (a.speed == 0.0) {
      //assert(a.at_end_of_edge)
      if (!a.at_end_of_edge) {
        Util.log(a + " isn't moving, but isnt at end of edge")
      }
      return 0
    }

    // most rounds, stop dist should == a.at.dist_left, within epsilon or so
    /*Util.log("stop dist " + a.stopping_distance() + ", left " + a.at.dist_left) 
    if (a.stopping_distance() > a.at.dist_left) {
      Util.log("  DOOMED? by " + (a.stopping_distance() - a.at.dist_left))
    }*/

    // a, b, c for a normal quadratic
    val q_a = 1 / a.max_accel
    val q_b = cfg.dt_s
    // we take away end_threshold, since stopping right at the end of the edge
    // makes us technically enter the intersection
    val q_c = (a.speed * cfg.dt_s) - (2 * (a.at.dist_left - cfg.end_threshold))
    val desired_speed = (-q_b + math.sqrt((q_b * q_b) - (4 * q_a * q_c))) / (2 * q_a)

    // TODO why does this or NaN ever happen?
    /*if (desired_speed < 0) {
      Util.log("why neg speed?")
    } else if (desired_speed.toString == "NaN") {
      // TODO Double.NaN comparison doesnt seem to work. also, not sure why this
      // happens, nor exactly how to fix...
      // try seed 1327894373344 to make it happen, though. synthetic.
      Util.log("NaN speed... a=" + q_a + ", b=" + q_b + ", c=" + q_c)
    }*/

    //Util.log("want speed " + a.speed + " -> " + desired_speed + "\n")

    // in the NaN case, just try to stop?
    val needed_accel = if (desired_speed.toString == "NaN")
                         accel_to_achieve(0)
                       else
                         accel_to_achieve(math.max(0, desired_speed))

    if (needed_accel > 0) {
      //Util.log("really? speed up?!")
      return math.min(a.max_accel, needed_accel)
    }

    // TODO make sure this difference is very small.. just floating pt issues
    // sometimes it isn't, and that seems to lead to NaN... figure out why.
    /*if (needed_accel < 0 && needed_accel < -a.max_accel) {
      println("how is " + needed_accel + " exceeding " + -a.max_accel)
    }*/
    return math.max(-a.max_accel, needed_accel)
  }

  // This directly follows from the distance traveled at constant accel
  private def accel_to_cover(dist: Double) =
    2 * (dist - (a.speed * cfg.dt_s)) / (cfg.dt_s * cfg.dt_s)
  def accel_to_achieve(target_speed: Double) = Util.accel_to_achieve(a.speed, target_speed)
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Accel(new_accel: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
