// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import scala.collection.mutable.{HashSet => MutableSet}

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Ticket, Agent}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, cfg}

// Automatically admit agents requesting common turns, and make the others queue
// and go when they can.
class CommonCasePolicy(intersection: Intersection,
                       ordering: IntersectionOrdering[Ticket])
  extends Policy(intersection)
{
  val common_turns = compute_common_turns

  // TODO how to use ordering to do diff stuff than from normal?
  // TODO looking ahead is unfair to waiting people...
  def react() = {
  }

  def validate_entry(a: Agent, t: Turn) = false

  def handle_exit(a: Agent, t: Turn) = {
  }

  def unregister_body(a: Agent) = {
  }

  def current_greens = common_turns

  def dump_info() = {
    Util.log(s"Common Case policy for $intersection")
    Util.log(s"Common turns: $common_turns")
  }

  private def compute_common_turns(): Set[Turn] = {
    // Heuristic...
    // TODO this is computed inefficiently, isnt manually tuneable, and probably
    // isnt the best choice
    // 1) Turns that don't conflict with many other things are good
    // 2) Turns coming from and going to major roads are good

    val turns = new MutableSet[Turn]()

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
