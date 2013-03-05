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
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import java.io.File
import javax.imageio.ImageIO

import utexas.aorta.sim.{MkIntersection, RouteType, WalletType, IntersectionType}

import utexas.aorta.Util

// Anything that we log online and post-process offline.
// Hacky fast, memory-happy serialization... use java's convenient way to
// read/write primitives, but don't even use their stuff to encode class.
// TODO header should be a byte, not 4 byte int.
abstract class Measurement {
  def write(stream: ObjectOutputStream)
}

object Stats {
  var log: ObjectOutputStream = null

  def setup_logging(fn: String) = {
    log = new ObjectOutputStream(new FileOutputStream(fn))
  }

  @elidable(elidable.ASSERTION) def record(item: Measurement) = {
    if (log != null) {
      synchronized {
        item.write(log)
      }
    }
  }
}

// Just to record where we're simulating and what the intersections do.
case class Scenario_Stat(
  map_fn: String, intersections: Array[MkIntersection]
) extends Measurement {
  // ID 0

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(0)
    // TODO being lazy, reconstructing an array is hard :|
    stream.writeObject(this)
    /*stream.writeString(map_fn)
    stream.writeInt(intersections.size)
    for (i <- intersections) {
      // TODO dont need to write the ID, really
      stream.write(i.id)
      stream.write(i.policy.id)
      stream.write(i.ordering.id)
    }*/
  }
}

case class Agent_Start_Stat(
  id: Int, tick: Double, start: Int, end: Int, route: RouteType.Value,
  wallet: WalletType.Value, budget: Double
) extends Measurement {
  // ID 1

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(1)
    stream.writeInt(id)
    stream.writeDouble(tick)
    stream.writeInt(start)
    stream.writeInt(end)
    stream.writeInt(route.id)
    stream.writeInt(wallet.id)
    stream.writeDouble(budget)
  }
}

case class Agent_Finish_Stat(
  id: Int, tick: Double, budget: Double
) extends Measurement {
  // ID 2

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(2)
    stream.writeInt(id)
    stream.writeDouble(tick)
    stream.writeDouble(budget)
  }
}

// When we first request a turn.
case class Turn_Request_Stat(
  agent: Int, vert: Int, tick: Double
) extends Measurement {
  // ID 3

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(3)
    stream.writeInt(agent)
    stream.writeInt(vert)
    stream.writeDouble(tick)
  }
}

// When we pay as part of our turn. May happen multiple times, and we may pay
// for something that isn't ours to get to our turn faster.
case class Turn_Pay_Stat(
  agent: Int, vert: Int, amount: Double
) extends Measurement {
  // ID 4

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(4)
    stream.writeInt(agent)
    stream.writeInt(vert)
    stream.writeDouble(amount)
  }
}

// When the intersection accepts us.
case class Turn_Accept_Stat(
  agent: Int, vert: Int, tick: Double
) extends Measurement {
  // ID 5

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(5)
    stream.writeInt(agent)
    stream.writeInt(vert)
    stream.writeDouble(tick)
  }
}

// When we completely finish a turn.
case class Turn_Done_Stat(
  agent: Int, vert: Int, tick: Double
) extends Measurement {
  // ID 6

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(6)
    stream.writeInt(agent)
    stream.writeInt(vert)
    stream.writeDouble(tick)
  }
}

// Logged every 1.0 real-time seconds. Number of agent_steps is since the last
// heartbeat; this is one of the few things tracked online. Active agents is the
// number that moved the specific tick that this heartbeat was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, tick: Double,
  agent_steps: Int
) extends Measurement {
  // ID 7

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(7)
    stream.writeInt(active_agents)
    stream.writeInt(live_agents)
    stream.writeInt(spawning_agents)
    stream.writeDouble(tick)
    stream.writeInt(agent_steps)
  }
}

// Summarizes an entire turn. Built offline.
case class Turn_Summary_Stat(
  agent: Int, vert: Int, req_tick: Double, accept_tick: Double,
  done_tick: Double, cost_paid: Double
) extends Measurement
{
  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick

  def write(stream: ObjectOutputStream) = {
    // TODO
  }
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
  // High priority and long trip time is bad; low priority or low trip time is
  // good.
  def weighted_value = start_budget * trip_time

  def write(stream: ObjectOutputStream) = {
    // TODO
  }
}

// Offline, read the measurements and figure stuff out.
object PostProcess {
  // TODO when this is true, dont demand a scenario to have completed.
  private val allow_unfinished = true

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
    analyze_agent_count(stats, dir)
    Util.log(s"\nResults at: $dir")
  }

  def read_stat(s: ObjectInputStream): Measurement = s.readInt match {
    case 0 => s.readObject.asInstanceOf[Scenario_Stat]
    case 1 => Agent_Start_Stat(
      s.readInt, s.readDouble, s.readInt, s.readInt, RouteType(s.readInt),
      WalletType(s.readInt), s.readDouble
    )
    case 2 => Agent_Finish_Stat(s.readInt, s.readDouble, s.readDouble)
    case 3 => Turn_Request_Stat(s.readInt, s.readInt, s.readDouble)
    case 4 => Turn_Pay_Stat(s.readInt, s.readInt, s.readDouble)
    case 5 => Turn_Accept_Stat(s.readInt, s.readInt, s.readDouble)
    case 6 => Turn_Done_Stat(s.readInt, s.readInt, s.readDouble)
    case 7 => Heartbeat_Stat(
      s.readInt, s.readInt, s.readInt, s.readDouble, s.readInt
    )
  }

  // First pair the raw stats into bigger-picture stats.
  private def group_raw_stats(log: ObjectInputStream): List[Measurement] = {
    Util.log("Post-processing raw stats into higher-level stats...")
    val stats = new ListBuffer[Measurement]()
    // map from (agent, vert) to the requestand accept stats and total cost
    val last_turn = new MutableMap[(Int, Int), (Turn_Request_Stat, Turn_Accept_Stat, Double)]()
    val agent_start = new MutableMap[Int, Agent_Start_Stat]()
    try {
      var iters = 0
      while (true) {
        read_stat(log) match {
          // Group turns
          case s: Turn_Request_Stat => {
            val key = (s.agent, s.vert)
            Util.assert_eq(last_turn.contains(key), false)
            last_turn(key) = (s, null, 0.0)
          }
          case Turn_Pay_Stat(a, v, amount) => {
            val key = (a, v)
            val triple = last_turn(key)
            last_turn(key) = (triple._1, null, triple._3 + amount)
          }
          case s: Turn_Accept_Stat => {
            val key = (s.agent, s.vert)
            val triple = last_turn(key)
            Util.assert_eq(triple._2, null)
            last_turn(key) = (triple._1, s, triple._3)
          }
          case Turn_Done_Stat(a, v, tick) => {
            val triple = last_turn.remove((a, v)).get
            stats += Turn_Summary_Stat(
              a, v, triple._1.tick, triple._2.tick, tick, triple._3
            )
          }

          // Group agent lifetimes
          case s: Agent_Start_Stat => {
            Util.assert_eq(agent_start.contains(s.id), false)
            agent_start(s.id) = s
          }
          case Agent_Finish_Stat(id, tick, budget) => {
            val orig = agent_start.remove(id).get
            stats += Agent_Summary_Stat(
              id, orig.tick, orig.start, orig.end, orig.route, orig.wallet,
              orig.budget, tick, budget
            )
          }

          // Echo other stuff
          case s: Measurement => stats += s
        }

        iters += 1
        if (iters % 1000 == 0) {
          Util.log("Processed " + Util.comma_num(iters) + " raw stats")
        }
      }
    } catch {
      case e: EOFException =>
    }
    if (allow_unfinished) {
      for (s <- agent_start.values) {
        // Never finished, so infinity
        // and hard to figure out budget
        stats += Agent_Summary_Stat(
          s.id, s.tick, s.start, s.end, s.route, s.wallet,
          s.budget, Double.PositiveInfinity, -1.0
        )
      }
    } else {
      Util.assert_eq(last_turn.isEmpty, true)
      Util.assert_eq(agent_start.isEmpty, true)
    }
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

    var unweighted_total = 0.0
    var weighted_total = 0.0

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
          unweighted_total += s.trip_time
          weighted_total += s.weighted_value
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

    Util.log("Unweighted total trip time: " + unweighted_total)
    Util.log("Weighted total trip time: " + weighted_total)
  }

  private def analyze_agent_count(stats: List[Measurement], dir: String) = {
    Util.log("Analyzing agent counts...")

    val live_count = new XYSeries("Live agents")
    val active_count = new XYSeries("Active agents")
    for (stat <- stats) {
      stat match {
        case s: Heartbeat_Stat => {
          live_count.add(s.tick, s.live_agents)
          active_count.add(s.tick, s.active_agents)
        }
        case _ =>
      }
    }
    val dataset = new XYSeriesCollection()
    dataset.addSeries(live_count)
    dataset.addSeries(active_count)

    val chart = ChartFactory.createXYLineChart(
      "Agent counts per time", "Time (s)", "Number of agents", dataset,
      PlotOrientation.VERTICAL, true, false, false
    )
    val img = chart.createBufferedImage(800, 600)
    ImageIO.write(img, "png", new File(s"$dir/agent_count_per_time.png"))
  }
}
