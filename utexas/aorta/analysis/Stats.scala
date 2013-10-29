// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, EOFException}
import scala.annotation.elidable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

import utexas.aorta.sim.{MkIntersection, RouteType, WalletType,
                         IntersectionType, OrderingType}

import utexas.aorta.common.{Util, AgentID, VertexID, EdgeID, DirectedRoadID}

// Anything that we log online and post-process offline.
// Hacky fast, memory-happy serialization... use java's convenient way to
// read/write primitives, but don't even use their stuff to encode class.
// TODO header should be a byte, not 4 byte int.
abstract class Measurement {
  def write(stream: ObjectOutputStream)
}

class StatsListener() {
  def record(item: Measurement) {}
}

// Appends to logfile
class StatsRecorder(fn: String) extends StatsListener {
  val log = new ObjectOutputStream(new FileOutputStream(fn, true))

  @elidable(elidable.ASSERTION) override def record(item: Measurement) {
    if (log != null) {
      synchronized {
        item.write(log)
      }
    }
  }
}

// Just to record where we're simulating and what the intersections do.
case class Scenario_Stat(
  map_fn: String, intersections: Array[MkIntersection]
) extends Measurement {
  // ID 0

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(0)
    stream.writeUTF(map_fn)
    stream.writeInt(intersections.size)
    for (i <- intersections) {
      // The ID is implicit
      stream.writeInt(i.policy.id)
      stream.writeInt(i.ordering.id)
    }
  }
}

// Summarizes an agent's lifetime
case class Agent_Lifetime_Stat(
  id: AgentID, spawn_tick: Double, start_tick: Double, start: EdgeID, end: DirectedRoadID,
  route: RouteType.Value, wallet: WalletType.Value, start_budget: Int,
  end_tick: Double, end_budget: Int, priority: Int, finished: Boolean
) extends Measurement
{
  // ID 1

  def trip_time = end_tick - spawn_tick // could be start_tick
  def total_spent = end_budget - start_budget
  // High priority and long trip time is bad; low priority or low trip time is
  // good.
  // Don't neglect freeriders, ever.
  def weight = priority + 1
  def weighted_value = weight * trip_time

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(1)
    stream.writeInt(id.int)
    stream.writeDouble(spawn_tick)
    stream.writeDouble(start_tick)
    stream.writeInt(start.int)
    stream.writeInt(end.int)
    stream.writeInt(route.id)
    stream.writeInt(wallet.id)
    stream.writeInt(start_budget)
    stream.writeDouble(end_tick)
    stream.writeInt(end_budget)
    stream.writeInt(priority)
    stream.writeBoolean(finished)
  }
}

// Describes a turn.
case class Turn_Stat(
  agent: AgentID, vert: VertexID, req_tick: Double, accept_tick: Double,
  done_tick: Double, cost_paid: Double
) extends Measurement
{
  // ID 2

  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(2)
    stream.writeInt(agent.int)
    stream.writeInt(vert.int)
    stream.writeDouble(req_tick)
    stream.writeDouble(accept_tick)
    stream.writeDouble(done_tick)
    stream.writeDouble(cost_paid)
  }
}

// Logged every 1.0 real-time seconds. Number of agent_steps and paths found are
// since the last heartbeat; this is one of the few things tracked online.
// Active agents is the number that moved the specific tick that this heartbeat
// was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, tick: Double,
  agent_steps: Int, ch_paths: Int, astar_paths: Int
) extends Measurement {
  // ID 3

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(3)
    stream.writeInt(active_agents)
    stream.writeInt(live_agents)
    stream.writeInt(spawning_agents)
    stream.writeDouble(tick)
    stream.writeInt(agent_steps)
    stream.writeInt(ch_paths)
    stream.writeInt(astar_paths)
  }

  def describe = s"$active_agents moved / $live_agents live / $spawning_agents ready"
}
