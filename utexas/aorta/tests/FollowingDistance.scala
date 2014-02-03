// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.tests

import utexas.aorta.sim.drivers.Kinematic
import utexas.aorta.common.{cfg, Physics}

object FollowingDistance {
  def main(args: Array[String]) = {
    val speed_lim = 30
    // Perfect start
    var leader = Kinematic(100, speed_lim, speed_lim)
    var follower = Kinematic(100 + cfg.follow_dist, speed_lim, speed_lim)

    leader = step(leader, 0)
    follower = step(follower, follower.accel_to_follow(leader, leader.dist - follower.dist))
    println(s"after one tick, ${descr(leader)} and ${descr(follower)}")
  }

  private def step(k: Kinematic, want_accel: Double): Kinematic = {
    val accel = math.max(-cfg.max_accel, math.min(want_accel, cfg.max_accel))
    return Kinematic(
      k.dist + Physics.dist_at_constant_accel(accel, cfg.dt_s, k.speed),
      Physics.update_speed(k.speed, accel, cfg.dt_s), k.speed_limit
    )
  }

  private def descr(k: Kinematic) = s"K(dist = ${k.dist}, speed = ${k.speed})"
}
