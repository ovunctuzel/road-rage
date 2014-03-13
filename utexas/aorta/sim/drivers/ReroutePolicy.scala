// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim.drivers

import utexas.aorta.map.{Edge, Turn}
import utexas.aorta.sim.{EV_Transition, EV_Reroute}

// Responsible for requesting reroutes
abstract class ReroutePolicy(a: Agent) {
  protected var should_reroute = false
  def react() {
    if (should_reroute) {
      should_reroute = false

      // Find the first edge for which the driver has no tickets, so we don't have to cancel
      // anything
      var at = a.at.on
      while (true) {
        at match {
          case e: Edge => {
            if (e.road == a.route.goal) {
              return
            }
            a.get_ticket(e) match {
              case Some(ticket) => at = ticket.turn
              case None => {
                // Reroute from there
                a.route.optional_reroute(e)
                return
              }
            }
          }
          case t: Turn => at = t.to
        }
      }
    }
  }
}

class NeverReroutePolicy(a: Agent) extends ReroutePolicy(a)

// Susceptible to perpetual oscillation
class RegularlyReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private var roads_crossed = 1
  private val reroute_frequency = 5

  a.sim.listen(classOf[EV_Transition], a, _ match {
    case EV_Transition(_, from, to: Turn) => {
      roads_crossed += 1
      if (roads_crossed % reroute_frequency == 0) {
        should_reroute = true
      }
    }
    case _ =>
  })
}

// Reroute when a road in our path is raised drastically
// TODO also try subscribing to changes in individual roads
class PriceChangeReroutePolicy(a: Agent) extends ReroutePolicy(a) {
  private val rescan_time = 30
  private val cost_ratio_threshold = 1.5

  private var total_orig_cost = 0.0
  private var start_countdown = false
  private var rescan_countdown = rescan_time

  a.sim.listen(classOf[EV_Reroute], a, _ match {
    case ev: EV_Reroute => start_countdown = true
  })

  override def react() {
    // The driver rerouted, so scan the new cost and set a timer to check on things
    if (start_countdown) {
      total_orig_cost = calc_route_cost
      rescan_countdown = rescan_time
      start_countdown = false
    }

    // Check to see if the route's cost has changed significantly
    if (rescan_countdown == 0) {
      if (calc_route_cost / total_orig_cost > cost_ratio_threshold) {
        should_reroute = true
        rescan_countdown = rescan_time
        super.react()
      }
    }
    rescan_countdown -= 1
  }

  // TODO remember cost of route from A* instead of recalculating it
  private def calc_route_cost(): Double = {
    var cost = 0.0
    var eta = a.sim.tick
    for (r <- a.route.current_path) {
      cost += r.road_agent.tollbooth.toll(eta).dollars
      eta += r.freeflow_time
    }
    return cost
  }
}
