// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream}
import scala.annotation.elidable

import utexas.aorta.sim.{MkIntersection, RouteType, WalletType}

import utexas.aorta.Util

// Anything that we log online and post-process offline.
abstract class Measurement

object Stats {
  // TODO will the file get closed and flushed automatically?
  var log: ObjectOutputStream = null

  def setup_logging(fn: String) = {
    log = new ObjectOutputStream(new FileOutputStream(fn))
  }

  @elidable(elidable.ASSERTION) def record(item: Measurement) = {
    if (log != null) {
      synchronized {
        log.writeObject(item)
      }
    }
  }
}

// Just to record where we're simulating and what the intersections do.
case class Scenario_Stat(
  map_fn: String, intersections: Array[MkIntersection]
) extends Measurement

case class Agent_Start_Stat(
  id: Int, tick: Double, start: Int, end: Int, route: RouteType.Value,
  wallet: WalletType.Value, budget: Double
) extends Measurement

case class Agent_Finish_Stat(
  id: Int, tick: Double, budget: Double
) extends Measurement

// TODO when we first request a turn?
case class Turn_Request_Stat(
  agent: Int, vert: Int, tick: Double, budget: Double
) extends Measurement

// When the intersection accepts us. The budget difference should imply how much
// we spent on this intersection only.
case class Turn_Accept_Stat(
  agent: Int, vert: Int, tick: Double, budget: Double
) extends Measurement

// When we completely finish a turn.
case class Turn_Done_Stat(
  agent: Int, vert: Int, tick: Double
) extends Measurement

// Logged every 1.0 real-time seconds. Number of agent_steps is since the last
// heartbeat; this is one of the few things tracked online. Active agents is the
// number that moved the specific tick that this heartbeat was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, tick: Double,
  agent_steps: Int
) extends Measurement

// Offline, read the measurements and figure stuff out.
object PostProcess {
  def main(args: Array[String]) = {
    val fn = args.head
    val log = new ObjectInputStream(new FileInputStream(fn))

    // TODO do different stuff for each analysis, but the general paradigm is
    // read everything, aggregate data, then run through aggregate again
    // probably.
    
    // analysis: make a histogram of the time to get through an intersection
    // (from requesting the turn to finishing it)
  }
}
