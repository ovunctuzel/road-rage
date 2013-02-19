// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.policies

import utexas.aorta.map.Turn
import utexas.aorta.sim.{Intersection, Policy, Ticket}
import utexas.aorta.sim.market.IntersectionOrdering

import utexas.aorta.{Util, cfg}

// Automatically admit agents requesting common turns, and make the others queue
// and go when they can.
class CommonCasePolicy(intersection: Intersection,
                       ordering: IntersectionOrdering[Ticket])
  //extends Policy(intersection)
  extends StopSignPolicy(intersection, ordering)  // TODO
{
}
