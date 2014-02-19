// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.intersections

import utexas.aorta.sim.drivers.Agent
import utexas.aorta.sim.make.OrderingType
import utexas.aorta.map.{Edge, Turn}

class PressureOrdering[T <: Ordered[T]]() extends IntersectionOrdering[T]() {
  def ordering_type = OrderingType.Pressure
  def choose(choices: Iterable[T], participants: Iterable[Ticket], client: Policy): Option[T] = {
    if (choices.isEmpty) {
      return None
    } else {
      if (choices.head.isInstanceOf[Ticket]) {
        return Some(choices.maxBy(t => weight(t.asInstanceOf[Ticket].a)))
      } else {
        return Some(choices.maxBy(p => p.asInstanceOf[Phase].head_agents.map(a => weight(a)).sum))
      }
    }
  }

  private def weight(a: Agent): Double = a.at.on match {
    //case t: Turn => throw new IllegalArgumentException("weight only defined for agents on lanes")
    case t: Turn => 0 // TODO define it for everyone.
    case e: Edge => a.num_behind + pred_leaders(e).map(leader => weight(leader)).sum
  }

  private def pred_leaders(e: Edge) = e.preds.map(_.queue.head).flatten.filter(
    a => a.get_ticket(a.at.on.asInstanceOf[Edge]) match {
      case Some(t) => t.turn.to == e
      case None => false
    })
}
