package utexas.sim

import scala.collection.mutable.LinkedList

import utexas.map.Edge
import utexas.Util.{log, log_push, log_pop}

trait Queue_of_Agents extends Edge {
  val agents = new LinkedList[Agent]()
  // TODO insertion method that sorts by position, removal

  // TODO queries: collision checks, nearest to agent/pos
}
