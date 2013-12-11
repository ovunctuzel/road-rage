// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.sim

import scala.collection.mutable

import utexas.aorta.map.Road
import utexas.aorta.common.{cfg, Util, Price}

// Manage information at the road level
class LinkAuditor(val r: Road, sim: Simulation) extends CongestionMeasure(sim) with CurrentCongestion {
  // TODO need to savestate.
  // TODO do different congestion policies based on scenarios, or cfg
  override def congested_now = r.lanes.exists(e => e.queue.is_congested)

  // Worst-case of any constituent lanes
  def freeflow_capacity = r.lanes.map(_.queue.freeflow_capacity).min
  def freeflow_percent_full = r.lanes.map(_.queue.percent_freeflow_full).max

  // Free until 50% freeflow capacity, then $1 per % full. Should range from $0-$50 until
  // congestion.
  // Also, ignore roads with absurdly low capacity. Those're always free.
  def toll =
    if (freeflow_capacity >= 3)
      new Price(math.max(0, freeflow_percent_full - 50))
    else
      new Price(0)
}

abstract class CongestionMeasure(val sim: Simulation) {
  protected def congested_now(): Boolean
  // The more permanent notion that clients should use
  def congested(): Boolean
  // TODO how often must this be called?
  def react()
}

// Just report the current state of congestion. Subject to oscillation.
trait CurrentCongestion extends CongestionMeasure {
  override def congested = congested_now
  override def react() {}
}

// Only flip states congested<->not if the state persists for >= 30s. Has a bias for whatever state
// starts.
trait StickyCongestion extends CongestionMeasure {
  private val threshold = 30.0  // seconds
  private var congested_state = false
  private var opposite_since: Option[Double] = None

  override def congested = congested_state

  override def react() {
    // Are we currently the same as we are permanently?
    if (congested_now == congested_state) {
      // Reset any timers that could've changed state
      opposite_since = None
    } else {
      // We're different! How long has it been?
      opposite_since match {
        case Some(start_time) if sim.tick - start_time >= threshold => {
          // State change!
          congested_state = congested_now
          opposite_since = None
        }
        // Start the timer
        case None => opposite_since = Some(sim.tick)
        case Some(start_time) =>  // Haven't stayed this way long enough yet
      }
    }
  }
}

// Report the most popular state of the last 30s
trait MovingWindowCongestion extends CongestionMeasure {
  private val duration = 31.0  // seconds
  private def num_observations = (cfg.dt_s / duration).toInt
  Util.assert_eq(num_observations % 2, 1) // must be odd
  private val last_observations = new mutable.Queue[Boolean]()
  Range(0, num_observations).foreach(_ => last_observations += false)
  private var true_cnt = 0
  private var false_cnt = 30

  override def congested = true_cnt > false_cnt  // == never happens because duration is odd

  // TODO assumes we'll be called every tick... fix
  override def react() {
    last_observations.dequeue() match {
      case true => true_cnt -= 1
      case false => false_cnt -= 1
    }
    val now = congested_now
    last_observations.enqueue(now)
    now match {
      case true => true_cnt += 1
      case false => false_cnt += 1
    }
  }
}
