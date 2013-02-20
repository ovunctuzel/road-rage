// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, EOFException}
import scala.annotation.elidable
import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.statistics.HistogramDataset
import java.io.File
import javax.imageio.ImageIO

import utexas.aorta.sim.{MkIntersection, RouteType, WalletType, IntersectionType}

import utexas.aorta.Util

// Anything that we log online and post-process offline.
abstract class Measurement

object Stats {
  // TODO will the file get closed and flushed automatically?
  var log: ObjectOutputStream = null

  def setup_logging(fn: String) = {
    log = new ObjectOutputStream(new FileOutputStream(fn))
  }

  @elidable(elidable.ASSERTION) def record(item: Measurement) = {
    if (log != null) {
      synchronized {
        log.writeObject(item)
      }
    }
  }
}

// Just to record where we're simulating and what the intersections do.
case class Scenario_Stat(
  map_fn: String, intersections: Array[MkIntersection]
) extends Measurement

case class Agent_Start_Stat(
  id: Int, tick: Double, start: Int, end: Int, route: RouteType.Value,
  wallet: WalletType.Value, budget: Double
) extends Measurement

case class Agent_Finish_Stat(
  id: Int, tick: Double, budget: Double
) extends Measurement

// TODO when we first request a turn?
case class Turn_Request_Stat(
  agent: Int, vert: Int, tick: Double, budget: Double
) extends Measurement

// When the intersection accepts us. The budget difference should imply how much
// we spent on this intersection only.
case class Turn_Accept_Stat(
  agent: Int, vert: Int, tick: Double, budget: Double
) extends Measurement

// When we completely finish a turn.
case class Turn_Done_Stat(
  agent: Int, vert: Int, tick: Double
) extends Measurement

// Logged every 1.0 real-time seconds. Number of agent_steps is since the last
// heartbeat; this is one of the few things tracked online. Active agents is the
// number that moved the specific tick that this heartbeat was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, tick: Double,
  agent_steps: Int
) extends Measurement

// Summarizes an entire turn. Built offline.
case class Turn_Summary_Stat(
  agent: Int, vert: Int, req_tick: Double, accept_tick: Double,
  done_tick: Double, budget_at_req: Double, budget_at_accept: Double
) extends Measurement
{
  // TODO if we pay for stuff in the future, gonna be wrong
  def cost_paid = budget_at_req - budget_at_accept
  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick
}

// Summarizes an agent's lifetime. Built offline.
case class Agent_Summary_Stat(
  id: Int, start_tick: Double, start: Int, end: Int, route: RouteType.Value,
  wallet: WalletType.Value, start_budget: Double, end_tick: Double,
  end_budget: Double
) extends Measurement
{
  def trip_time = end_tick - start_tick
  def total_spent = end_budget - start_budget
}

// Offline, read the measurements and figure stuff out.
object PostProcess {
  def main(args: Array[String]): Unit = {
    val fn = args.head
    val log = new ObjectInputStream(new FileInputStream(fn))

    val stats = group_raw_stats(log)
    log.close
    val dir = "plots/" + fn.replace("logs/", "")
    (new File(dir)).mkdirs

    //stats.foreach(s => Util.log(s"$s"))
    analyze_turn_times(stats, dir)
    analyze_trip_times(stats, dir)
    Util.log(s"\nResults at: $dir")
  }

  // First pair the raw stats into bigger-picture stats.
  private def group_raw_stats(log: ObjectInputStream): List[Measurement] = {
    Util.log("Post-processing raw stats into higher-level stats...")
    val stats = new ListBuffer[Measurement]()
    // map from (agent, vert) to the request and accept stats
    val last_turn = new MutableMap[(Int, Int), (Turn_Request_Stat, Turn_Accept_Stat)]()
    try {
      val agent_start = new MutableMap[Int, Agent_Start_Stat]()
      while (true) {
        log.readObject match {
          // Group turns
          case s: Turn_Request_Stat => {
            val key = (s.agent, s.vert)
            Util.assert_eq(last_turn.contains(key), false)
            last_turn(key) = ((s, null))
          }
          case s: Turn_Accept_Stat => {
            val key = (s.agent, s.vert)
            val pair = last_turn(key)
            Util.assert_eq(pair._2, null)
            last_turn(key) = (pair._1, s)
          }
          case Turn_Done_Stat(a, v, tick) => {
            val pair = last_turn.remove((a, v)).get
            stats += Turn_Summary_Stat(
              a, v, pair._1.tick, pair._2.tick, tick, pair._1.budget,
              pair._2.budget
            )
          }

          // Group agent lifetimes
          case s: Agent_Start_Stat => {
            Util.assert_eq(agent_start.contains(s.id), false)
            agent_start(s.id) = s
          }
          case Agent_Finish_Stat(id, tick, budget) => {
            val orig = agent_start(id)
            stats += Agent_Summary_Stat(
              id, orig.tick, orig.start, orig.end, orig.route, orig.wallet,
              orig.budget, tick, budget
            )
          }

          // Echo other stuff
          case s: Measurement => stats += s
        }
      }
    } catch {
      case e: EOFException =>
    }
    Util.assert_eq(last_turn.isEmpty, true)
    return stats.toList
  }

  private def histogram[T](
    data: Map[T, ArrayBuffer[Double]], title: String, x_axis: String, fn: String
  ) = {
    val dataset = new HistogramDataset() // TODO relative freq?
    for ((group, numbers) <- data if numbers.nonEmpty) {
      dataset.addSeries(
        // TODO buckets?
        // TODO set a global min and max?
        group.toString, numbers.toArray, 15, numbers.min, numbers.max
      )
    }

    val chart = ChartFactory.createHistogram(
      title, x_axis, "Frequency", dataset, PlotOrientation.VERTICAL, true,
      false, false
    )
    val img = chart.createBufferedImage(800, 600)
    ImageIO.write(img, "png", new File(fn))
  }

  private def analyze_turn_times(stats: List[Measurement], dir: String) = {
    Util.log("Analyzing turn delays...")

    val intersections = stats.head match {
      case Scenario_Stat(_, i) => i
      case _ => throw new Exception("Didn't find Scenario Stat first!")
    }

    // TODO show min (and where/who), max, average
    // TODO correlate with the combos of intersection policy and ordering
    // TODO correlate with agent budget (only expect relation when Auction
    // ordering)

    // Histogram showing times, broken down by just policy.
    val delays_per_policy = IntersectionType.values.toList.map(
      p => p -> new ArrayBuffer[Double]()
    ).toMap
    // TODO groupBy? filter?
    for (stat <- stats) {
      stat match {
        case s: Turn_Summary_Stat => {
          delays_per_policy(intersections(s.vert).policy) += s.accept_delay
        }
        case _ =>
      }
    }

    histogram(
      data = delays_per_policy,
      title = "Turn delays (from request to acceptance",
      x_axis = "Delay (s)",
      fn = s"$dir/turn_delay_per_policy.png"
    )
  }

  private def analyze_trip_times(stats: List[Measurement], dir: String) = {
    Util.log("Analyzing trip times...")

    // TODO show min (and where/who), max, average
    // TODO lots of weighted/unweighted stuff

    // Histogram showing times, broken down by just route.
    val times_per_route = RouteType.values.toList.map(
      r => r -> new ArrayBuffer[Double]()
    ).toMap
    // TODO groupBy? filter?
    for (stat <- stats) {
      stat match {
        case s: Agent_Summary_Stat => {
          times_per_route(s.route) += s.trip_time
        }
        case _ =>
      }
    }

    histogram(
      data = times_per_route,
      title = "Agent trip times",
      x_axis = "Total time (s)",
      fn = s"$dir/trip_time_per_route.png"
    )
  }
}
