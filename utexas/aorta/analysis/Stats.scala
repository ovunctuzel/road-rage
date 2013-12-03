// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import utexas.aorta.sim.make.{MkIntersection, RouteType, WalletType}

import utexas.aorta.common.{AgentID, VertexID, DirectedRoadID, Util}

trait Measurement

// Summarizes an agent's lifetime
case class Agent_Lifetime_Stat(
  id: AgentID, birth_tick: Double, start: DirectedRoadID, end: DirectedRoadID,
  route: RouteType.Value, wallet: WalletType.Value, start_budget: Int,
  end_tick: Double, end_budget: Int, priority: Int, finished: Boolean
) extends Measurement
{
  // Note this includes waiting to actually spawn on the first lane
  def trip_time = end_tick - birth_tick
  def total_spent = start_budget - end_budget
  // High priority and long trip time is bad; low priority or low trip time is
  // good.
  // Don't neglect freeriders, ever.
  def weight = priority + 1
  def weighted_value = weight * trip_time
}

// Describes a turn.
case class Turn_Stat(
  agent: AgentID, vert: VertexID, req_tick: Double, accept_tick: Double,
  done_tick: Double, cost_paid: Double
) extends Measurement
{
  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick
}

// Logged every 1.0 real-time seconds. Number of agent_steps and paths found are
// since the last heartbeat; this is one of the few things tracked online.
// Active agents is the number that moved the specific tick that this heartbeat
// was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, done_agents: Int, tick: Double,
  agent_steps: Int, counts: RerouteCountMonitor
) extends Measurement {
  //private val agent_str = "%,d moved, %,d live, %,d ready, %,d done"
  private val agent_str = "A[%,d] L[%,d] R[%,d] D[%,d]"
  def describe_agents = agent_str.format(active_agents, live_agents, spawning_agents, done_agents)
  def describe = "At t=%s: %s {%,03d moves, %,d A* (%,dm + %,df)}".format(
    Util.time_num(tick), describe_agents, agent_steps, counts.astar_count,
    counts.unrealizable_count, counts.discretionary_count
  )
}
