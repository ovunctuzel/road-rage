// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.TurnLike
import utexas.aorta.sim.{Policy, Agent, UberSection, Intersection}

import utexas.aorta.{Util, cfg}

class UberDelegatePolicy(intersection: Intersection, master: Policy) extends Policy(intersection)
{
  val ubersection = master.junction match {
    case u: UberSection => u
    case _ => throw new ClassCastException("master isn't uber?")
  }

  def turn_of(a: Agent, turn: TurnLike) = ubersection.uberturn_of(a, turn)

  // pass the call up to master unless the agent is already in the intersection
  def can_go(a: Agent, turn: TurnLike, far_away: Double): Boolean = {
    if (ubersection.v.verts.contains(turn.from.from)) {
      return true
    } else {
      val uberturn = turn_of(a, turn)
      return master.can_go(a, uberturn, far_away)
    }
  }

  def validate_entry(a: Agent, turn: TurnLike) = master.validate_entry(a, turn_of(a, turn))

  // ignore this unless the agent is totally leaving the ubersection
  def handle_exit(a: Agent, turn: TurnLike) = {
    val uberturn = turn_of(a, turn)
    if (uberturn.turns.last == turn) {
      ubersection.forget(a)
      master.handle_exit(a, uberturn)
    }
  }

  def unregister(a: Agent) = master.unregister(a)

  // Just filter the parent's answer
  override def current_greens = master.current_greens & intersection.v.turns.toSet
}
