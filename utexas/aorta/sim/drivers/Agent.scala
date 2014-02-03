// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import scala.collection.mutable

import utexas.aorta.map.{Edge, Coordinate, Turn, Traversable, Graph, Position}
import utexas.aorta.sim.{Simulation, EV_AgentQuit, AgentMap, EV_Breakpoint}
import utexas.aorta.sim.intersections.{Intersection, Ticket}
import utexas.aorta.ui.Renderable

import utexas.aorta.common.{Util, cfg, Physics, StateWriter, StateReader, AgentID, EdgeID,
                            ValueOfTime, Flags}

class Agent(
  val id: AgentID, val route: Route, val wallet: Wallet, val sim: Simulation
) extends Ordered[Agent] with Renderable
{
  //////////////////////////////////////////////////////////////////////////////
  // Transient state

  var debug_me = false  // TODO public read, private write

  //////////////////////////////////////////////////////////////////////////////
  // State

  var at: Position = null

  // We can only set a target acceleration, which we travel at for the entire
  // duration of timesteps.
  val max_accel = cfg.max_accel
  // TODO max_deaccel too
  var speed: Double = 0.0   // meters/sec
  var target_accel: Double = 0  // m/s^2
  private val behavior = new LookaheadBehavior(this, route)

  // old_lane is where we're shifting from. we immediately warp into the target
  // lane.
  var old_lane: Option[Edge] = None
  var lanechange_dist_left: Double = 0

  // how long has our speed been 0?
  private var idle_since = -1.0

  // keyed by origin lane
  private val tickets = new mutable.HashMap[Edge, Ticket]()

  //////////////////////////////////////////////////////////////////////////////
  // Meta

  def setup(spawn: Edge, dist: Double) {
    wallet.setup(this)
    route.setup(this)
    at = enter(spawn, dist)
    spawn.queue.allocate_slot
    sim.insert_agent(this)
    AgentMap.maps.foreach(m => m.when_created(this))
    set_debug(Flags.int("--track", -1) == id.int)
    if (debug_me) {
      sim.publish(EV_Breakpoint(this))
    }
  }

  def serialize(w: StateWriter) {
    // First do parameters
    w.int(id.int)
    route.serialize(w)
    wallet.serialize(w)

    // Then the rest of our state
    at.serialize(w)
    w.double(speed)
    w.double(target_accel)
    behavior.target_lane match {
      case Some(e) => w.int(e.id.int)
      case None => w.int(-1)
    }
    old_lane match {
      case Some(e) => w.int(e.id.int)
      case None => w.int(-1)
    }
    w.double(lanechange_dist_left)
    w.double(idle_since)
    w.int(tickets.size)
    tickets.values.toList.sorted.foreach(ticket => ticket.serialize(w))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  // Returns true if we move or do anything at all
  def step(): Boolean = {
    // If they're not already lane-changing, should they start?
    if (!is_lanechanging && behavior.wants_to_lc) {
      val lane = behavior.target_lane.get
      if (safe_to_lc(lane)) {
        // We have to cover a fixed distance to lane-change. The scaling is kind
        // of arbitrary and just forces lane-changing to not be completely
        // instantaneous at higher speeds.
        lanechange_dist_left = cfg.lanechange_dist
        old_lane = Some(at.on.asInstanceOf[Edge])

        // Immediately enter the target lane
        behavior.transition(at.on, lane)
        at = enter(lane, at.dist)
        lane.queue.allocate_slot
      }
    }

    // Do physics to update current speed and figure out how far we've traveled
    // in this timestep.
    // Subtle note: Do this after LCing, else we see the wrong speed
    val new_dist = update_kinematics(cfg.dt_s)

    // Currently lane-changing?
    old_lane match {
      case Some(lane) => {
        lanechange_dist_left -= new_dist
        if (lanechange_dist_left <= 0) {
          // Done! Leave the old queue
          exit(lane)
          lane.queue.free_slot

          // Return to normality
          old_lane = None
          lanechange_dist_left = 0
          // We'll only shift lanes once per tick.
        }
      }
      case None =>
    }

    if (is_stopped && target_accel <= 0.0) {
      if (idle_since == -1.0) {
        idle_since = sim.tick
      }
      // We shouldn't ever stall during a turn! Leave a little slack time-wise
      // since the agents in front might not be packed together yet...
      /*if (how_long_idle >= cfg.dt_s * 10) {
        at.on match {
          case t: Turn => Util.log(s"  $this stalled during $t!")
          case _ =>
        }
      }*/

      // TODO get rid of this short circuit entirely?
      if (new_dist == 0.0) {
        return false
      }
    }

    val start_on = at.on
    val old_dist = at.dist

    idle_since = if (is_stopped && idle_since == -1.0)
                   sim.tick   // we've started idling
                 else if (is_stopped)
                   idle_since   // keep same
                 else
                   -1.0   // we're not idling

    // Check speed limit. Allow a bit of slack.
    Util.assert_le(speed, start_on.speed_limit + cfg.epsilon)

    // Apply this distance. 
    var current_on = start_on
    var current_dist = old_dist + new_dist

    while (current_dist >= current_on.length) {
      if (current_on != start_on && is_lanechanging) {
        throw new Exception(this + " just entered an intersection while lane-changing!")
      }
      current_dist -= current_on.length
      // Are we finishing a turn or starting one?
      val next: Traversable = current_on match {
        case e: Edge => {
          val turn = behavior.choose_turn(e)
          assert(e.next_turns.contains(turn))    // Verify it was a legal choice
          turn
        }
        case t: Turn => t.to
      }

      // tell the intersection
      (current_on, next) match {
        case (e: Edge, t: Turn) => {
          val i = t.vert.intersection
          i.enter(get_ticket(t).get)
          e.queue.free_slot
        }
        case (t: Turn, e: Edge) => {
          val ticket = get_ticket(t).get
          ticket.done_tick = sim.tick
          remove_ticket(ticket)
          ticket.intersection.exit(ticket)
          sim.publish(ticket.stat)
        }
      }

      // this lets behaviors make sure their route is being followed
      behavior.transition(current_on, next)
      current_on = next
    }

    // so we finally end up somewhere...
    if (start_on == current_on) {
      val old_dist = at.dist
      at = move(start_on, current_dist, old_dist)
      // Also stay updated in the other queue
      old_lane match {
        // our distance is changed since we moved above...
        case Some(lane) => move(lane, current_dist, old_dist)
        case None =>
      }
    } else {
      exit(start_on)
      at = enter(current_on, current_dist)
    }

    return new_dist > 0.0
  }

  // Returns true if we're done
  def react(): Boolean = {
    return behavior.choose_action match {
      case Act_Set_Accel(new_accel) => {
        // we have physical limits
        Util.assert_le(new_accel.abs, max_accel)
        target_accel = new_accel
        false
      }
      case Act_Done_With_Route() => {
        at.on match {
          case e: Edge => // normal
          case t: Turn => throw new Exception(s"Done with route at $t")
        }
        //Util.assert_eq(at.on.asInstanceOf[Edge].road, route.goal)
        // Trust behavior, don't abuse this.
        // (Plus, it doesn't hold for RouteAnalyzer vehicles...)
        Util.assert_eq(speed <= cfg.epsilon, true)
        true
      }
    }
  }

  // returns distance traveled, updates speed. note unit of the argument.
  private def update_kinematics(dt_sec: Double): Double = {
    // Travel at the target constant acceleration for the duration of the
    // timestep, capping off when speed hits zero.
    val dist = Physics.dist_at_constant_accel(target_accel, dt_sec, speed)
    Util.assert_ge(dist, 0.0)
    speed = Physics.update_speed(speed, target_accel, dt_sec)
    return dist
  }

  // Delegate to the queues and intersections that simulation manages
  private def enter(t: Traversable, dist: Double) = t.queue.enter(this, dist)
  private def exit(t: Traversable) {
    t.queue.exit(this, at.dist)
  }
  private def move(t: Traversable, new_dist: Double, old_dist: Double) =
    t.queue.move(this, new_dist, old_dist)

  // Caller must remove this agent from the simulation list
  def terminate(interrupted: Boolean = false) = {
    if (!interrupted) {
      exit(at.on)
      at.on match {
        case e: Edge => e.queue.free_slot
        case _ =>
      }
      Util.assert_eq(tickets.isEmpty, true)
    }
    val maker = sim.scenario.agents(id.int)
    sim.publish(EV_AgentQuit(
      this, maker.birth_tick, sim.graph.get_r(maker.start), route.goal, route.route_type,
      wallet.wallet_type, maker.wallet.budget, sim.tick, wallet.budget, wallet.priority,
      !interrupted
    ))
    AgentMap.maps.foreach(m => m.destroy(this))
  }

  def set_debug(value: Boolean) {
    debug_me = value
    behavior.set_debug(value)
    route.set_debug(value)
    wallet.set_debug(value)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Queries

  override def toString = "Agent " + id
  override def compare(other: Agent) = id.int.compare(other.id.int)
  override def tooltip = List(toString, wallet.toString) ++ wallet.tooltip
  def debug() {
    Util.log("" + this)
    Util.log_push
    Util.log("At: " + at)
    Util.log("Speed: " + speed)
    Util.log("How long idle? " + how_long_idle)
    Util.log("Max next speed: " + kinematic.max_next_speed)
    Util.log("Stopping distance next: " + Physics.stopping_distance(kinematic.max_next_speed))
    Util.log("Lookahead dist: " + kinematic.max_lookahead_dist)
    Util.log("Dist left here: " + at.dist_left)
    at.on match {
      case e: Edge => {
        Util.log(s"${num_ahead(e)} agents ahead of us in this lane, ${num_ahead(e.leftmost_lane)} in the leftmost lane")
      }
      case _ =>
    }
    Util.log("Tickets:")
    for (ticket <- tickets.values) {
      Util.log(s"  $ticket waiting for ${ticket.how_long_waiting}")
      Util.log("  Gridlock? " + Intersection.detect_gridlock(ticket.turn))
    }
    if (is_lanechanging) {
      Util.log(s"Lane-changing from ${old_lane.get}. $lanechange_dist_left to go!")
    } else {
      Util.log("Not lane-changing")
    }
    behavior.dump_info
    Util.log_pop
  }

  def kinematic = Kinematic(at.dist, speed, at.on.speed_limit)

  def how_long_idle = if (idle_since == -1.0)
                        0
                      else
                        sim.tick - idle_since
  def is_stopped = speed <= cfg.epsilon

  def add_ticket(ticket: Ticket) {
    Util.assert_eq(ticket.a, this)
    Util.assert_eq(tickets.contains(ticket.turn.from), false)
    tickets += ((ticket.turn.from, ticket))
  }
  def remove_ticket(ticket: Ticket) {
    val removed = tickets.remove(ticket.turn.from)
    Util.assert_eq(removed.get, ticket)
  }
  def get_ticket(from: Edge) = tickets.get(from)
  def get_ticket(turn: Turn) = tickets.get(turn.from)
  // TODO rm this method.
  def all_tickets(i: Intersection) = tickets.values.filter(t => t.intersection == i)
  // If true, we will NOT block when trying to proceed past this intersection
  def wont_block(i: Intersection) = at.on match {
    // We won't block any intersection if we're about to vanish
    case e: Edge if route.done(e) => true
    case _ => tickets.values.find(
      t => t.intersection == i && (t.is_approved || t.is_interruption)
    ).isDefined
  }

  def is_lanechanging = old_lane.isDefined

  // Just see if we have enough static space to pull off a lane-change.
  def room_to_lc(target: Edge): Boolean = {
    // One lane could be shorter than the other. When we want to avoid the end
    // of a lane, worry about the shorter one to be safe.
    val min_len = math.min(at.on.length, target.length)

    // Satisfy the physical model, which requires us to finish lane-changing
    // before reaching the intersection.
    if (at.dist + cfg.lanechange_dist + cfg.end_threshold >= min_len) {
      return false
    }

    // Furthermore, we probably have to stop for the intersection, so be sure we
    // have enough room to do that.
    // This is confusing, but we're called in two places, the most important of
    // which is step(), right before an acceleration chosen in the previous tick
    // will be applied. The behavior chose that acceleration assuming we'd be in
    // the old lane, not the new. TODO redo the reaction here?
    // So we have to apply what will happen next...

    val initial_speed = speed
    val final_speed = math.max(0.0, initial_speed + (target_accel * cfg.dt_s))
    val dist = Physics.dist_at_constant_accel(target_accel, cfg.dt_s, initial_speed)
    val our_max_next_speed = final_speed + (Physics.max_next_accel(final_speed, at.on.speed_limit) * cfg.dt_s)

    if (at.dist + dist + Physics.stopping_distance(our_max_next_speed) >= min_len) {
      return false
    }
    
    return true
  }

  // Would we cut anybody off if we LC in front of them?
  def can_lc_without_blocking(target: Edge): Boolean = {
    // TODO is it ok to cut off somebody thats done with their route?
    val intersection = target.to.intersection
    return !target.queue.all_agents.find(
      agent => agent.at.dist < at.dist && agent.wont_block(intersection)
    ).isDefined && !target.dont_block
  }

  def can_lc_without_crashing(target: Edge): Boolean = {
    // We don't want to merge in too closely to the agent ahead of us, nor do we
    // want to make somebody behind us risk running into us. So just make sure
    // there are no agents in that danger range.
    // TODO assumes all vehicles the same. not true forever.

    val initial_speed = speed
    val final_speed = math.max(0.0, initial_speed + (target_accel * cfg.dt_s))
    val this_dist = Physics.dist_at_constant_accel(target_accel, cfg.dt_s, initial_speed)

    // TODO this is overconservative too. we wont pick max next dist if we see
    // somebody in front of us!
    val ahead_dist = cfg.follow_dist + Physics.max_next_dist_plus_stopping(final_speed, target.speed_limit)
    // For behind, assume somebody behind us is going full speed. Give them time
    // to stop fully and not hit where we are now (TODO technically should
    // account for the dist we'll travel, but hey, doesnt hurt to be safe...
    // Don't forget they haven't applied their step this turn either!
    val behind_dist = (target.speed_limit * cfg.dt_s) + cfg.follow_dist + Physics.max_next_dist_plus_stopping(target.speed_limit, target.speed_limit)

    // TODO +dist for behind as well, but hey, overconservative doesnt hurt for
    // now...
    val nearby = target.queue.all_in_range(
      at.dist - behind_dist, true, at.dist + this_dist + ahead_dist, true
    )
    return nearby.isEmpty
  }

  private def safe_to_lc(target: Edge): Boolean = {
    at.on match {
      case e: Edge => {
        if (e.road != target.road) {
          throw new Exception(this + " wants to lane-change across roads")
        }
        if (math.abs(target.lane_num - e.lane_num) != 1) {
          throw new Exception(this + " wants to skip lanes when lane-changing")
        }                                                               
      }
      case _ => throw new Exception(this + " wants to lane-change from a turn!")
    }

    if (!room_to_lc(target)) {
      return false
    }

    // Does the target queue have capacity? Don't cause gridlock!
    if (!target.queue.slot_avail) {
      return false
    }
    if (!can_lc_without_blocking(target)) {
      return false
    }

    // If there's somebody behind us on the target, or if we're beyond the
    // worst-case entry distance, we don't have to worry about drivers on other
    // roads about to take turns and fly into the new lane. But if we are
    // concerned, just ask the intersection!
    if (!target.queue.closest_behind(at.dist).isDefined &&
        at.dist <= at.on.worst_entry_dist + cfg.follow_dist)
    {
      // Smart look-behind: the intersection knows.
      val beware = target.from.intersection.policy.approveds_to(target)
      // TODO for now, if theres any -- worry. could do more work using the
      // below to see if they'll wind up too close, though.
      if (beware.nonEmpty) {
        return false
      }
    }

    if (!can_lc_without_crashing(target)) {
      return false
    }

    return true
  }

  def cur_queue = at.on.queue
  // Meaningless unless e is in the same road as the agent
  def num_ahead(e: Edge) = at.on match {
    case edge: Edge if edge.road == e.road =>
      // min() handles when lanes in same road are very different in length
      e.queue.all_in_range(math.min(at.dist, e.length), false, e.length, true).size
    // If we're not there yet, then all of them are ahead!
    case _ => e.queue.agents.size
  }
  def on(t: Traversable) = (at.on, old_lane) match {
    case (ours, _) if ours == t => true
    case (_, Some(l)) if l == t => true
    case _ => false
  }
  def cur_vert = at.on match {
    case e: Edge => e.to
    case t: Turn => t.vert
  }
  def our_lead = at.on.queue.ahead_of(this)
  def our_tail = at.on.queue.behind(this)
  def how_far_away(i: Intersection) = route.steps_to(at.on, i.v).map(_.length).sum - at.dist
  // Report some number that fully encodes our current choice
  // TODO should be getting turns chosen and LCs done too, to distinguish a few
  // rare cases thatd we'll otherwise blur.
  def characterize_choice = target_accel

  // Seconds saved per dollar. Just use priority for now.
  def value_of_time = new ValueOfTime(wallet.priority)
}

object Agent {
  def unserialize(r: StateReader, sim: Simulation): Agent = {
    val a = new Agent(
      new AgentID(r.int), Route.unserialize(r, sim.graph), Wallet.unserialize(r), sim
    )
    a.at = Position.unserialize(r, sim.graph)
    a.speed = r.double
    a.target_accel = r.double
    a.behavior.target_lane = r.int match {
      case -1 => None
      case x => Some(sim.graph.edges(x))
    }
    a.old_lane = r.int match {
      case -1 => None
      case x => Some(sim.graph.edges(x))
    }
    a.lanechange_dist_left = r.double
    a.idle_since = r.double
    val num_tickets = r.int
    a.tickets ++= Range(0, num_tickets).map(_ => {
      val t = Ticket.unserialize(r, a, sim.graph)
      (t.turn.from, t)
    })
    // Add ourselves back to a queue
    a.at.on.queue.enter(a, a.at.dist)
    a.old_lane match {
      case Some(e) => e.queue.enter(a, a.at.dist)
      case None =>
    }
    // Add ourselves back to intersections
    for (ticket <- a.tickets.values if ticket.is_approved) {
      ticket.intersection.policy.unserialize_accepted(ticket)
    }
    a.at.on match {
      case t: Turn => t.vert.intersection.enter(a.get_ticket(t).get)
      case _ =>
    }
    return a
  }
}
