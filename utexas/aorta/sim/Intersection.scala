// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.TreeSet

import utexas.aorta.sim.policies._
import utexas.aorta.map.{Vertex, Turn, Edge}
import utexas.aorta.analysis.Turn_Stat

import utexas.aorta.{Util, Common, cfg}

// Common stuff goes here, particular implementations are in utexas.aorta.sim.policies

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex, policy_type: IntersectionType.Value,
                   val ordering_type: OrderingType.Value)
{
  val policy = Factory.make_policy(this, policy_type, ordering_type)

  override def toString = "Intersection(" + v + ")"

  def react = policy.react
  def request_turn(ticket: Ticket) = {
    // Sanity check...
    Util.assert_eq(ticket.turn.vert, v)
    policy.request_turn(ticket)
  }
  def change_turn(old: Ticket, ticket: Ticket) = {
    Util.assert_eq(old.turn.vert, v)
    Util.assert_eq(ticket.turn.vert, v)
    Util.assert_eq(old.is_approved, false)
    Util.assert_eq(old.is_interruption, false)
    policy.change_turn(old, ticket)
  }

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  val turns = MutableMap[Turn, Int]()

  // Check for collisions
  def end_step(): Unit = {
    if (turns.size < 2) {
      return
    }

    // The naive way is quadratic (a Cartesian product), with a slight
    // optimization by observing that the conflicts-with relation is symmetric.
    for (t1 <- turns.keys; t2 <- turns.keys if t1.id < t2.id) {
      if (t1.conflicts_with(t2)) {
        throw new Exception(s"Intersection collision: $t1 and $t2 conflict")
      }
    }

    // TODO something more clever with equivalence sets, even though this isn't
    // a transitive relation?
  }

  def enter(ticket: Ticket) = {
    val t = ticket.turn
    if (!turns.contains(t)) {
      // We don't care until there are at least two... and this only changes when
      // we add a new one...
      // It's not really that much better to do the checking in-place and only
      // atomic-check when there could be a problem.
      if (turns.size == 1) {
        Common.sim.active_intersections += this
      }

      turns(t) = 0
    }
    turns(t) += 1
    if (!policy.validate_entry(ticket)) {
      // Misleading error message. They may be going 0 speed, but the agent step
      // code hasn't finished moving them.
      Util.log("!!! %s illegally entered %s, going %f m/s".format(ticket.a, this, ticket.a.speed))
      Util.log("  Illegal entry was near " + t.from + " and " + t + " (vert " + t.from.to.id + ")")
      Util.log("  Origin lane length: " + t.from.length + "; time " + Common.tick)
      Util.log("  Happened at " + Common.tick)
      ticket.a.debug
      policy.dump_info
      sys.exit
    }
  }

  def exit(ticket: Ticket) = {
    val t = ticket.turn
    turns(t) -= 1
    if (turns(t) == 0) {
      turns -= t
      // Potentially we now don't care...
      if (turns.size == 1) {
        Common.sim.active_intersections -= this
      }
    }
    policy.handle_exit(ticket)
  }
}

// TODO when we talk to intersection, dont pass agent, pass this!
class Ticket(val a: Agent, val turn: Turn) extends Ordered[Ticket] {
  // TODO if an agent ever loops and requests the same turn before clearing the
  // prev one, gonna have a bad time!
  override def compare(other: Ticket) =
    implicitly[Ordering[Tuple2[Agent, Turn]]].compare((a, turn), (other.a, other.turn))
  override def toString = s"Ticket($a, $turn, approved? $is_approved)"

  def intersection = turn.vert.intersection

  // Don't initially know: accept_tick, done_tick, cost_paid.
  // TODO keep state, dont shuffle it into this >_<
  var stat = Turn_Stat(a.id, turn.vert.id, Common.tick, -1.0, -1.0, 0.0)

  def approve() = {
    stat = stat.copy(accept_tick = Common.tick)
    // Allocate a spot for them. This must be called when the turn isn't
    // blocked.
    // TODO this messes up parallelism again...
    if (!is_interruption) {
      // When we became the interrupting ticket, we grabbed the slot.
      turn.to.queue.allocate_slot
    }
  }

  def is_approved = stat.accept_tick != -1.0

  // If we interrupt a reservation successfully, don't let people block us.
  var is_interruption = false

  // When our turn first becomes not blocked while we're stopped, start the
  // timer.
  var waiting_since = -1.0
  def how_long_waiting =
    if (waiting_since == -1.0)
      0.0
    else
      Common.tick - waiting_since

  // To enforce liveness, policies shouldn't accept a turn that can't certainly
  // be finished. Once a turn is accepted, lane-changing and spawning and such
  // have to maintain the stability of this property.
  def turn_blocked(): Boolean = {
    val target = turn.to
    val intersection = turn.vert.intersection

    // They might not finish LCing before an agent in the new or old lane
    // stalls.
    if (a.is_lanechanging) {
      return true
    }

    // Lane-changing and spawning respect allocations of capacity too, so this
    // tells us if we can finish the turn.
    if (!target.queue.slot_avail) {
      return true
    }

    val steps = a.route.steps_to(a.at.on, turn.vert)
    // TODO efficiency... dont search behind us, only look one person ahead once
    // this is really an enforced invariant
    val blocked_cur_step = steps.head.queue.all_agents.find(
      agent => agent.at.dist > a.at.dist && !agent.wont_block(intersection)
    ).isDefined
    if (blocked_cur_step) {
      return true
    }

    val blocked_future_step = steps.tail.find(step => step.queue.all_agents.find(
      agent => !agent.wont_block(intersection)
    ).isDefined).isDefined
    if (blocked_future_step) {
      return true
    }

    // We're clear!
    if (waiting_since == -1.0 && a.speed == 0.0) {
      waiting_since = Common.tick
    }
    return false

    // Perform gridlock detection!
    /*var current = target
    val seen = new MutableSet[Edge]()
    seen += turn.from
    while (current != null && !current.queue.slot_avail) {
      // A cycle!
      if (seen(current)) {
        Util.log(s"Gridlock detected by $this. Seen: $seen")
        return true
      }
      seen += current

      // Where's the head of that stuck queue trying to go?
      current = current.queue.head match {
        case Some(a) => a.all_tickets(current.to.intersection).toList match {
          case Nil => null
          // TODO which ticket? the approved one?
          case ls => ls.head.turn.to
        }
        case None => null
      }
    }

    return false*/
  }

  // An under-estimate; the earliest possible time.
  def time_till_arrival() =
    a.how_far_away(turn.vert.intersection) / a.at.on.speed_limit

  def close_to_start =
    if (a.at.on == turn.from) {
      a.our_lead match {
        // Are we following reasonably closely?
        // TODO better math to account for following at speed
        case Some(other) => other.at.dist - a.at.dist <= cfg.follow_dist * 10.0
        // If we're the head, just be close to starting
        case None => time_till_arrival <= 5.0
      }
    } else {
      // When we register early or lookahead over small steps, just demand we'll
      // arrive at the turn soon
      time_till_arrival <= 10.0
    }
}

abstract class Policy(val intersection: Intersection) {
  // When intersections pull agents off this list, the order is arbitrary but
  // deterministic.
  protected var waiting_agents = new TreeSet[Ticket]()
  // Agents inform intersections of their intention ONCE and receive a lease
  // eventually.
  def request_turn(ticket: Ticket) = {
    synchronized {
      waiting_agents += ticket
      // TODO do extra book-keeping to verify agents aren't double requesting?
    }
  }
  def change_turn(old: Ticket, ticket: Ticket) = {
    synchronized {
      waiting_agents -= old
      waiting_agents += ticket
      cancel_turn(old)
    }
  }

  // The intersection grants leases to waiting_agents
  def react(): Unit
  // TODO validate_entry, handle_exit, and waiting_agents -> queue are all
  // almost common. refactor them?
  def validate_entry(ticket: Ticket): Boolean
  def handle_exit(ticket: Ticket)
  // Happens when an agent is requesting a different turn
  def cancel_turn(ticket: Ticket)
  def approveds_to(target: Edge): Iterable[Ticket]
  def current_greens(): Set[Turn]
  def dump_info()
  def policy_type(): IntersectionType.Value
}

// Simplest base-line ever.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def react = {}
  def validate_entry(ticket: Ticket) = false
  def handle_exit(ticket: Ticket) = {}
  def cancel_turn(ticket: Ticket) = {}
  def approveds_to(target: Edge) = Nil
  def current_greens = Set()
  def dump_info = {
    Util.log(s"Never go policy for $intersection")
  }
  def policy_type = IntersectionType.NeverGo
}
