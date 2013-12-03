// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

import scala.collection.mutable
import utexas.aorta.common.{DirectedRoadID, Price, cfg, Common, Util}

// Represent a group of directed edges on one road
// TODO var id because things get chopped up
class DirectedRoad(val road: Road, var id: DirectedRoadID, val dir: Direction.Value)
  extends Ordered[DirectedRoad]
{
  // TODO lets figure out how to build immutable stuff.
  val houses = new mutable.ListBuffer[Coordinate]()
  val shops = new mutable.ListBuffer[Coordinate]()

  override def toString = "%s's %s lanes (DR %s)".format(road, dir, id)
  override def compare(other: DirectedRoad) = id.int.compare(other.id.int)

  def edges = if (dir == Direction.POS)
                road.pos_lanes
              else
                road.neg_lanes
  def rightmost = edges.head

  def from: Vertex = edges.head.from
  def to: Vertex = edges.head.to

  def start_pt = edges.head.from.location
  def end_pt = edges.head.to.location
  // TODO dont assume some edge being lane-changeable means others are too
  // TODO could even predict/take into account the current distance to see if
  // there's room left
  def naive_leads_to = edges.flatMap(_.succs).map(_.directed_road).toSet
  def leads_to(from: Edge) = if (from.ok_to_lanechange)
                               naive_leads_to
                             else
                               from.succs.map(_.directed_road).toSet

  def length = road.length
  def freeflow_time = road.length / road.speed_limit

  def succs = edges.flatMap(e => e.next_turns.map(t => t.to.directed_road))
  def preds = edges.flatMap(e => e.prev_turns.map(t => t.from.directed_road))

  def next_roads = edges.flatMap(e => e.next_roads).toSet

  // We're congested if any of our lanes are.
  def is_congested = edges.exists(e => e.queue.is_congested)

  // Worst-case of any constituent lanes
  def freeflow_capacity = edges.map(_.queue.freeflow_capacity).min
  def freeflow_percent_full = edges.map(_.queue.percent_freeflow_full).max

  // TODO decorate dir roads with something to add these

  // Free until 50% freeflow capacity, then $1 per % full. Should range from $0-$50 until
  // congestion.
  // Also, ignore roads with absurdly low capacity. Those're always free.
  def toll =
    if (freeflow_capacity >= 3)
      new Price(math.max(0, freeflow_percent_full - 50))
    else
      new Price(0)
}

// TODO this stuff all belongs in sim somewhere, and it needs to be savestated.

abstract trait CongestionMeasure {
  def congested_now(): Boolean
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
        case Some(start_time) if Common.tick - start_time >= threshold => {
          // State change!
          congested_state = congested_now
          opposite_since = None
        }
        // Start the timer
        case None => opposite_since = Some(Common.tick)
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
