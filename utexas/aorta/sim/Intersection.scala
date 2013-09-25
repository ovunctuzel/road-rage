// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{HashSet => MutableSet}
import scala.collection.mutable.{TreeSet, ListBuffer}

import utexas.aorta.sim.policies._
import utexas.aorta.map.{Vertex, Turn, Edge, Graph}
import utexas.aorta.analysis.Turn_Stat

import utexas.aorta.common.{Util, Common, cfg, StateWriter, StateReader}

// Common stuff goes here, particular implementations are in utexas.aorta.sim.policies

// Reason about collisions from conflicting simultaneous turns.
class Intersection(val v: Vertex, policy_type: IntersectionType.Value,
                   val ordering_type: OrderingType.Value)
{
  val policy = Factory.make_policy(this, policy_type, ordering_type)

  // Multiple agents can be on the same turn; the corresponding queue will
  // handle collisions. So in fact, we want to track which turns are active...
  // but we have to know how many are on the turn to know when nobody's
  // attempting it.
  val turns = MutableMap[Turn, Int]() // TODO count type

  // For reporting stats
  private var sum_waiting_times = 0.0
  private var number_finished = 0

  override def toString = "Intersection(" + v + ")"

  def request_turn(ticket: Ticket) = {
    // Sanity check...
    Util.assert_eq(ticket.turn.vert, v)
    policy.request_turn(ticket)
  }

  def cancel_turn(ticket: Ticket) {
    Util.assert_eq(ticket.turn.vert, v)
    policy.cancel_turn(ticket)
  }

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
      Util.log(s"!!! ${ticket.a} illegally entered $this, going ${ticket.a.speed} m/s")
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
    number_finished += 1
    sum_waiting_times += ticket.how_long_waiting
  }

  def average_waiting_time =
    if (number_finished > 0)
      sum_waiting_times / number_finished
    else
      0.0
}

object Intersection {
  // TODO wheres this belong?
  def detect_gridlock(turn: Turn): Boolean = {
    var current = turn.from
    val seen = new MutableSet[Edge]()
    while (current != null && !current.queue.slot_avail) {
      // A cycle!
      if (seen(current)) {
        //Util.log(s"Gridlock detected, involving: $seen")
        return true
      }
      seen += current

      // Where's the head of that stuck queue trying to go?
      current = current.queue.head match {
        case Some(a) => a.all_tickets(current.to.intersection).toList match {
          case Nil => null
          // TODO which ticket?
          case ticket :: rest =>
            if (ticket.is_approved)
              null
            else
              ticket.turn.to
        }
        case None => null
      }
    }
    return false
  }
}

class Ticket(val a: Agent, val turn: Turn) extends Ordered[Ticket] {
  //////////////////////////////////////////////////////////////////////////////
  // State
  
  // Don't initially know: accept_tick, done_tick, cost_paid.
  // TODO keep state properly, dont shuffle it into this >_<
  var stat = Turn_Stat(a.id, turn.vert.id, Common.tick, -1.0, -1.0, 0.0)

  // If we interrupt a reservation successfully, don't let people block us.
  var is_interruption = false

  // When our turn first becomes not blocked while we're stopped, start the
  // timer.
  private var waiting_since = -1.0

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def serialize(w: StateWriter) {
    // Agent is implied, since we belong to them
    w.int(turn.id)
    w.double(stat.req_tick)
    w.double(stat.accept_tick)
    w.double(stat.done_tick)
    w.double(stat.cost_paid)
    w.bool(is_interruption)
    w.double(waiting_since)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

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

  def cancel() {
    Util.assert_eq(is_approved, false)
    a.tickets.remove(this)
    intersection.cancel_turn(this)
  }

  // To enforce liveness, policies shouldn't accept a turn that can't certainly
  // be finished. Once a turn is accepted, lane-changing and spawning and such
  // have to maintain the stability of this property.
  // This is an action because it touches the waiting_since state.
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
    if (waiting_since == -1.0 && a.is_stopped) {
      waiting_since = Common.tick
    }
    return false
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = s"Ticket($a, $turn, approved? $is_approved. interrupt? $is_interruption)"

  // TODO if an agent ever loops and requests the same turn before clearing the
  // prev one, gonna have a bad time!
  override def compare(other: Ticket) =
    implicitly[Ordering[Tuple2[Agent, Turn]]].compare((a, turn), (other.a, other.turn))

  def intersection = turn.vert.intersection

  def is_approved = stat.accept_tick != -1.0

  def how_long_waiting =
    if (waiting_since == -1.0)
      0.0
    else
      Common.tick - waiting_since

  def eta_earliest() =
    a.how_far_away(turn.vert.intersection) / a.at.on.speed_limit

  // This isn't really optimistic or pessimistic; we could be going the speed
  // limit or not
  def eta_at_cur_speed() =
    a.how_far_away(turn.vert.intersection) / math.max(a.speed, cfg.epsilon)

  def close_to_start =
    if (a.at.on == turn.from) {
      a.our_lead match {
        // Are we following reasonably closely?
        // TODO better math to account for following at speed
        case Some(other) => other.at.dist - a.at.dist <= cfg.follow_dist * 10.0
        // If we're the head, just be close to starting
        case None => eta_earliest <= 5.0
      }
    } else {
      // When we register early or lookahead over small steps, just demand we'll
      // arrive at the turn soon
      eta_earliest <= 10.0
    }

  // If we're part of gridlock and we have a choice, bail out.
  def should_cancel() =
    turn.from.next_turns.size > 1 && Intersection.detect_gridlock(turn)
}

object Ticket {
  def unserialize(r: StateReader, a: Agent, graph: Graph): Ticket = {
    val ticket = new Ticket(a, graph.turns(r.int))
    ticket.stat = ticket.stat.copy(
      req_tick = r.double, accept_tick = r.double, done_tick = r.double,
      cost_paid = r.double
    )
    ticket.is_interruption = r.bool
    ticket.waiting_since = r.double
    return ticket
  }
}

abstract class Policy(val intersection: Intersection) {
  //////////////////////////////////////////////////////////////////////////////
  // State

  // This will have a deterministic order.
  protected var request_queue: List[Ticket] = Nil
  protected val accepted = new TreeSet[Ticket]()

  // Agents could be added to this in any order, but they'll wind up in
  // request_queue in a deterministic order. This is transient state.
  private var new_requests = new TreeSet[Ticket]()

  //////////////////////////////////////////////////////////////////////////////
  // State

  def serialize(w: StateWriter) {
    Util.assert_eq(new_requests.isEmpty, true)
    w.int(request_queue.size)
    for (ticket <- request_queue) {
      w.id(ticket.a.id)
      w.int(ticket.turn.id)
    }
  }

  protected def unserialize(r: StateReader, sim: Simulation) {}

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // Agents inform intersections of their intention ONCE and receive a lease
  // eventually.
  def request_turn(ticket: Ticket) = {
    synchronized {
      new_requests += ticket
      // TODO do extra book-keeping to verify agents aren't double requesting?
    }
  }

  def cancel_turn(ticket: Ticket) {
    synchronized {
      // TODO assert not in new_requests
      // TODO assert it was in here!
      request_queue = request_queue.filter(t => t != ticket)
    }
  }

  def react_tick() {
    request_queue ++= new_requests
    new_requests.clear()
    react()
  }

  protected def accept(ticket: Ticket) {
    ticket.approve()
    accepted += ticket
    unqueue(ticket)
  }

  protected def unqueue(ticket: Ticket) {
    request_queue = request_queue.filter(_ != ticket)
  }

  def react(): Unit

  // This could be called for several reasons, so assume they could be queued or
  // accepted.
  def handle_exit(ticket: Ticket) {
    accepted -= ticket
    unqueue(ticket)
  }

  def unserialize_accepted(ticket: Ticket) {
    accepted += ticket
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  def policy_type(): IntersectionType.Value

  def dump_info() {
    Util.log(s"$intersection is a $policy_type")
    Util.log(s"Accepted: $accepted")
    Util.log(s"Queued: $request_queue")
  }
  def approveds_to(target: Edge) = accepted.filter(_.turn.to == target)
  def validate_entry(ticket: Ticket) = accepted.contains(ticket)
  def current_greens() = accepted.map(_.turn).toSet
  def queued_count = request_queue.size
}

object Policy {
  def unserialize(policy: Policy, r: StateReader, sim: Simulation) {
    val num_requests = r.int
    for (i <- Range(0, num_requests)) {
      policy.request_queue :+= find_ticket(r, sim)
    }
    policy.unserialize(r, sim)
  }

  def find_ticket(r: StateReader, sim: Simulation): Ticket =
    find_ticket(sim, r.int, r.int)
  def find_ticket(sim: Simulation, agent_id: Int, turn_id: Int): Ticket =
    sim.get_agent(agent_id).get.get_ticket(sim.graph.turns(turn_id)).get
}

// Simplest base-line ever.
class NeverGoPolicy(intersection: Intersection) extends Policy(intersection) {
  def react() {}
  def policy_type = IntersectionType.NeverGo
}
