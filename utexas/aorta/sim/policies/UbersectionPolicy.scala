// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import utexas.aorta.sim.{Intersection, Policy, Agent}
import utexas.aorta.map.Turn
import utexas.aorta.{Util, cfg}
import scala.collection.mutable.ListBuffer

class UbersectionPolicy(intersection: Intersection, parentPolicy: Policy) extends Policy(intersection) {
  //Pass the call up to the parent unless it's already been approved
  def can_go(a: Agent, turn: Turn, far_away: Double): Boolean = {
    val firstTurn = parentPolicy.get_agent_turn_start(a)
	firstTurn match {
      //If the Ubersection hasn't seen this agent yet, compute its UberTurn then let the parent decide what to do
      case None => {
        val otherTurns = parentPolicy.intersection.turns
        val turns = ListBuffer[Turn]()
        var nextTurn = turn
        var isDone = false
        while (otherTurns.contains(nextTurn) && !isDone){
          turns += nextTurn
          a.route.next_turn(turn) match {
            case t: Turn => nextTurn = t
            case _ => isDone = true
          }
        }
        parentPolicy.set_agent_turn_set(a, turns.toList)
        parentPolicy.can_go(a, turn, far_away)
      }
      //If the agent is entering the Ubersection through us, ask the parent what to do
      case t:Turn if t == this => parentPolicy.can_go(a, turn, far_away)
      //If the agent is already in the Ubersection, it should continue
      //TODO This won't be right once we add re-planning (which is necessary for this to work right anyway)
      case _ => true
    }
  }

  def validate_entry(a: Agent, turn: Turn) = parentPolicy.validate_entry(a, turn)

  //Pass the call to the parent IF the agent is actually leaving the ubersection.  Otherwise, just ignore it
  def handle_exit(a: Agent, turn: Turn) = {
    parentPolicy.get_agent_turn_start(a) match{
      case None => Util.log("Agent exiting part of ubersection without entering ubersection...")
      case t:Turn if t == this => parentPolicy.handle_exit(a, turn)
      case _ =>
    }
  }

  //Ask the parent, but limit response to this intersection
  //TODO Is this going to call parentPolicy.current_greens multiple times?
  override def current_greens = intersection.v.turns.filter(x => parentPolicy.current_greens.contains(x)).toSet

  def unregister(a: Agent) = parentPolicy.unregister(a)

}
