// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.make.OrderingType
import utexas.aorta.map.Road

// Use the current toll of the origin lane to sort
// (It may make sense to consider the destination lane's toll also)
class TollOrdering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.Toll
  def choose(choices: Iterable[T], participants: Iterable[Ticket], client: Policy): Option[T] = {
    if (choices.isEmpty) {
      return None
    } else {
      if (choices.head.isInstanceOf[Ticket]) {
        val tick = choices.head.asInstanceOf[Ticket].a.sim.tick // Messy?
        // This blurs tickets from the same road, but such turns rarely conflict
        return Some(choices.maxBy(
          t => t.asInstanceOf[Ticket].turn.from.road.road_agent.tollbooth.toll(tick).dollars
        ))
      } else {
        // TODO broken for signals -- What tick is it if there may not be agents? :\
        val tick = 0.0
        return Some(choices.maxBy(p => p.asInstanceOf[Phase].turns.map(_.from.road).toSet.map(
          (r: Road) => r.road_agent.tollbooth.toll(tick).dollars
        ).sum))
      }
    }
  }
}
