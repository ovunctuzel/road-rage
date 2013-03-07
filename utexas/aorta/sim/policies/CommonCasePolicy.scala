// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.TreeSet

import utexas.aorta.map.{Turn, Edge}
import utexas.aorta.sim.{Intersection, Policy, Ticket, Agent, IntersectionType}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, cfg}

// Automatically admit agents requesting common turns, and make the others queue
// and go when they can.
class CommonCasePolicy(intersection: Intersection,
                       ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  val common_turns = compute_common_turns

  private val accepted_commoners = new MutableSet[Ticket]()
  private val accepted_rares = new MutableSet[Ticket]()
  private def commoner_blocks(turn: Turn) =
    accepted_commoners.find(t => t.turn.conflicts_with(turn)).isDefined
  private def rare_blocks(turn: Turn) =
    accepted_rares.find(t => t.turn.conflicts_with(turn)).isDefined

  // Rare agents must be the head of their queue and be close enough to us (in
  // case they looked ahead over small edges).
  private def is_ready(a: Agent) =
    (a.cur_queue.head.get == a &&
     a.how_far_away(intersection) <= cfg.end_threshold)

  def react() = {
    // First admit everybody trying to do a common turn, unless somebody rare
    // and conflicting has been approved.

    // Because we have to maintain turn invariants as we accept, do a fixpoint
    // approach and accept till there's nobody left that we can.
    // TODO ^ refactor this by making an abstract 'candidates' routine
    val candidates = new TreeSet[Ticket]()
    candidates ++= waiting_agents.filter(
      ticket => common_turns.contains(ticket.turn) && !rare_blocks(ticket.turn)
    )
    var changed = true
    while (changed && candidates.nonEmpty) {
      changed = false
      for (ticket <- candidates) {
        if (!ticket.turn_blocked) {
          ticket.approve
          accepted_commoners += ticket
          waiting_agents -= ticket
          candidates -= ticket
          changed = true
        }
      }
    }

    // TODO how to use ordering to do diff stuff than from normal?
    // TODO looking ahead is unfair to waiting people...
    // Now admit rare agents who're ready and wouldn't interrupt an accepted
    // commoner. We don't need to do a fixpoint here, since is_ready demands
    // they be the head of their queue. We're really not nice to rare turns;
    // they've got to act like a stop sign.
    for (ticket <- waiting_agents) {
      if (!common_turns.contains(ticket.turn) && is_ready(ticket.a) &&
          !commoner_blocks(ticket.turn) && !rare_blocks(ticket.turn))
      {
        if (!ticket.turn_blocked) {
          ticket.approve
          accepted_rares += ticket
          waiting_agents -= ticket
        }
      }
    }
  }

  def validate_entry(a: Agent, turn: Turn) =
    (accepted_commoners.find(t => t.a == a && t.turn == turn).isDefined ||
     accepted_rares.find(t => t.a == a && t.turn == turn).isDefined)

  def handle_exit(a: Agent, t: Turn) = {
    unregister_body(a)
  }

  def unregister_body(a: Agent) = {
    accepted_commoners.retain(t => t.a != a)
    accepted_rares.retain(t => t.a != a)
  }

  def approveds_to(e: Edge) =
    (accepted_commoners ++ accepted_rares).filter(t => t.turn.to == e).map(_.a)

  def current_greens = (accepted_commoners ++ accepted_rares).map(_.turn).toSet

  def dump_info() = {
    Util.log(s"Common Case policy for $intersection")
    Util.log(s"Common turns: $common_turns")
    Util.log(s"Waiting agents: $waiting_agents")
  }
  def policy_type = IntersectionType.CommonCase

  private def compute_common_turns(): Set[Turn] = {
    // Heuristic...
    // TODO this is computed inefficiently, isnt manually tuneable, and probably
    // isnt the best choice
    // 1) Turns that don't conflict with many other things are good
    // 2) Turns coming from and going to major roads are good

    val turns = new MutableSet[Turn]()

    // TODO is everything here deterministic?
    intersection.v.turns.sortBy(t => {
      val major_bonus = List(t.from.road, t.to.road).filter(_.is_major).size
      t.conflicts.size + (5 * major_bonus)
    }).foreach(new_turn => {
      if (!turns.find(t => t.conflicts_with(new_turn)).isDefined) {
        turns += new_turn
      }
    })

    return turns.toSet
  }
}
