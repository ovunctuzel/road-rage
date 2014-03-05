// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.contrib

import utexas.aorta.sim.{Simulation, EV_TurnStarted, EV_TurnApproved}
import utexas.aorta.sim.drivers.Agent
import utexas.aorta.map.Vertex
import utexas.aorta.common.{Physics, cfg}

import scala.collection.mutable

// TODO use levin's follow accel
class ArrivalEstimator(sim: Simulation) {
  case class Prediction(time: Double, speed: Double)

  private val estimates = new mutable.HashMap[(Agent, Vertex), Prediction]()

  // TODO when to set the prediction? ever update it?
  // - if we do it when we request a turn, thats fine, but estimate assumes we speed up and have an
  // approved turn. so intersection will have to say "if we accept them now and they speed up, will
  // it work out?" there's many variations of that, such as telling them to arrive at a time/speed,
  // but thats complicated.
  sim.listen(classOf[EV_TurnApproved], _ match { case EV_TurnApproved(ticket) => {
    val a = ticket.a
    // TODO off by timestep?
    a.our_lead match {
      case Some(leader) => {
        val leader_prediction = estimates(leader, ticket.intersection.v)
        val follow_dist = cfg.follow_dist + math.max(0,
          math.pow(math.max(a.speed - cfg.max_accel * cfg.dt_s, 0), 2) -
          math.pow(leader.speed, 2) / (-cfg.max_accel)
        )
        // Assume a is follow_dist behind leader when they enter and also the same speed
        // (blatanly untrue sometimes)
        estimates((a, ticket.intersection.v)) = Prediction(
          leader_prediction.time + (follow_dist / leader_prediction.speed),
          leader_prediction.speed
        )
        println(s"$a following $leader")
      }
      case None => {
        // TODO I didn't use formulas
        val speed_lim = math.min(ticket.turn.speed_limit, ticket.turn.from.speed_limit)
        val (time, final_speed) = Physics.simulate_steps(
          a.how_far_away(ticket.intersection), a.speed, speed_lim
        )
        estimates((a, ticket.intersection.v)) = Prediction(a.sim.tick + time, final_speed)
      }
    }
  }})

  sim.listen(classOf[EV_TurnStarted], _ match { case EV_TurnStarted(ticket) => {
    val a = ticket.a
    if (estimates.contains((a, ticket.intersection.v))) {
      val predict = estimates.remove((a, ticket.intersection.v)).get
      val actual_time = a.sim.tick
      val actual_speed = a.speed

      if (math.abs(actual_time - predict.time) > 1.0) {// || actual_speed != predict.speed) {
        println(s"$a time (actual $actual_time, should ${predict.time}), speed (actual $actual_speed, should ${predict.speed})")
        println("    !!! significant error")
      }
    }
  }})
}
