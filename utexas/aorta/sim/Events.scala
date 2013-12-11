// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import utexas.aorta.map.Turn
import utexas.aorta.sim.make.IntersectionType
import utexas.aorta.analysis.{Measurement, Heartbeat_Stat}

sealed trait Sim_Event
final case class EV_Signal_Change(greens: Set[Turn]) extends Sim_Event
final case class EV_Heartbeat(heartbeat: Heartbeat_Stat) extends Sim_Event
final case class EV_AgentSpawned(a: Agent) extends Sim_Event
final case class EV_Step(tick: Double) extends Sim_Event
final case class EV_Stat(stat: Measurement) extends Sim_Event
final case class EV_IntersectionOutcome(intersection: IntersectionType.Value, losers: List[Ticket])
  extends Sim_Event
