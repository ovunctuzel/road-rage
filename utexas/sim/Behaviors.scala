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
      route = route.tail   // moving right along
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

  def max_safe_speed(): Double = {
    // TODO put this where it belongs
    val speed_limit = Util.mph_to_si(30)

    // Find the person currently in front of us or who will be
    val q = a.cur_queue
    val next = q.ahead_of(a)

    // Say there's nobody on our queue. It's... not easy to figure out who, if
    // anyone, we want to pay attention to.


    // (on same trav, on trav we next plan on being at... but then we have to
    // predict who could be there!) TODO

    return next match {
      case Some(avoid: Agent) => avoid.speed
      case None               => speed_limit
    }

    // And say they choose to slam on their brakes in this step. What do we
    // have to do to prevent collision? TODO

    // Will it be emergent behavior that we get some distance between ourselves
    // and then converge on their speed?

    // TODO I think we want to do math.max(this result, speed limit of
    // current/next possible)
  }
}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Set_Speed(new_speed: Double) extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
