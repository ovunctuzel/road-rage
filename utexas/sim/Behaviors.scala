package utexas.sim

import utexas.map.{Edge, Turn}
import utexas.Util

abstract class Behavior(a: Agent) {
  // asked every time. just one action per tick?
  def choose_action(dt_ms: Long, tick: Double): Action
  // only queried when the agent reaches a vertex
  def choose_turn(e: Edge): Turn
}

class IdleBehavior(a: Agent) extends Behavior(a) {
  override def choose_action(dt_ms: Long, tick: Double): Action = {
    return Act_Nothing()
  }

  override def choose_turn(e: Edge): Turn = {
    // lol @ test behavior
    if (e.next_turns.size == 0) {
      // TODO fix this in the map properly.
      Util.log("wtf @ " + e)
      return e.prev_turns.head
    }
    return e.next_turns.head
  }
}

//class OldStaticBehavior(a: Agent) extends Behavior(a) {
//}

// TODO this would be the coolest thing ever... driving game!
//class HumanControlBehavior(a: Agent) extends Behavior(a) {
//}

abstract class Action
final case class Act_Change_Speed(new_speed: Double) extends Action
final case class Act_Disappear() extends Action
final case class Act_Lane_Change(lane: Edge) extends Action
final case class Act_Nothing() extends Action
