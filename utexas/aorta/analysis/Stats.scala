// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.analysis

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, EOFException}
import scala.annotation.elidable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import java.io.File
import javax.imageio.ImageIO

import utexas.aorta.sim.{MkIntersection, RouteType, WalletType,
                         IntersectionType, OrderingType}

import utexas.aorta.Util

// Anything that we log online and post-process offline.
// Hacky fast, memory-happy serialization... use java's convenient way to
// read/write primitives, but don't even use their stuff to encode class.
// TODO header should be a byte, not 4 byte int.
abstract class Measurement {
  def write(stream: ObjectOutputStream)
}

// Appends to logfile
class StatsRecorder(fn: String) {
  val log = new ObjectOutputStream(new FileOutputStream(fn, true))

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
    stream.writeUTF(map_fn)
    stream.writeInt(intersections.size)
    for (i <- intersections) {
      // The ID is implicit
      stream.writeInt(i.policy.id)
      stream.writeInt(i.ordering.id)
    }
  }
}

// Summarizes an agent's lifetime
case class Agent_Lifetime_Stat(
  id: Int, start_tick: Double, start: Int, end: Int, route: RouteType.Value,
  wallet: WalletType.Value, start_budget: Int, end_tick: Double,
  end_budget: Int, priority: Int, finished: Boolean
) extends Measurement
{
  // ID 1

  def trip_time = end_tick - start_tick
  def total_spent = end_budget - start_budget
  // High priority and long trip time is bad; low priority or low trip time is
  // good.
  // Don't neglect freeriders, ever.
  def weight = priority + 1
  def weighted_value = weight * trip_time

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(1)
    stream.writeInt(id)
    stream.writeDouble(start_tick)
    stream.writeInt(start)
    stream.writeInt(end)
    stream.writeInt(route.id)
    stream.writeInt(wallet.id)
    stream.writeInt(start_budget)
    stream.writeDouble(end_tick)
    stream.writeInt(end_budget)
    stream.writeInt(priority)
    stream.writeBoolean(finished)
  }
}

// Describes a turn.
case class Turn_Stat(
  agent: Int, vert: Int, req_tick: Double, accept_tick: Double,
  done_tick: Double, cost_paid: Double
) extends Measurement
{
  // ID 2

  // Total delay means turn length factors in.
  def total_delay = done_tick - req_tick
  def accept_delay = accept_tick - req_tick

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(2)
    stream.writeInt(agent)
    stream.writeInt(vert)
    stream.writeDouble(req_tick)
    stream.writeDouble(accept_tick)
    stream.writeDouble(done_tick)
    stream.writeDouble(cost_paid)
  }
}

// Logged every 1.0 real-time seconds. Number of agent_steps and paths found are
// since the last heartbeat; this is one of the few things tracked online.
// Active agents is the number that moved the specific tick that this heartbeat
// was taken.
case class Heartbeat_Stat(
  active_agents: Int, live_agents: Int, spawning_agents: Int, tick: Double,
  agent_steps: Int, ch_paths: Int, astar_paths: Int
) extends Measurement {
  // ID 3

  def write(stream: ObjectOutputStream) = {
    stream.writeInt(3)
    stream.writeInt(active_agents)
    stream.writeInt(live_agents)
    stream.writeInt(spawning_agents)
    stream.writeDouble(tick)
    stream.writeInt(agent_steps)
    stream.writeInt(ch_paths)
    stream.writeInt(astar_paths)
  }

  def describe = s"$active_agents moved / $live_agents live / $spawning_agents ready"
}

// Offline, read the measurements and figure stuff out.
object PostProcess {
  def main(args: Array[String]): Unit = {
    val fn = args.head
    Util.log(s"Analyzing $fn...")
    val dir = "plots/" + fn.replace("logs/", "")
    Util.mkdir(dir)
    val log = new ObjectInputStream(new FileInputStream(fn))

    val analyses = List(
      new TurnTimeAnalysis(dir), new TripTimeAnalysis(dir), new
      AgentCountAnalysis(dir)
    )

    try {
      var iters = 0
      while (true) {
        val s = read_stat(log)
        analyses.foreach(a => a.process(s))

        iters += 1
        if (iters % 100000 == 0) {
          print("\rProcessed " + Util.comma_num(iters) + " stats")
        }
      }
    } catch {
      case e: EOFException =>
    }
    Util.log("\n")
    log.close

    analyses.foreach(a => a.finish)
    Util.log(s"\nResults at: $dir")
  }

  def read_stat(s: ObjectInputStream): Measurement = s.readInt match {
    case 0 => Scenario_Stat(s.readUTF, read_intersections(s))
    case 1 => Agent_Lifetime_Stat(
      s.readInt, s.readDouble, s.readInt, s.readInt, RouteType(s.readInt),
      WalletType(s.readInt), s.readInt, s.readDouble, s.readInt,
      s.readInt, s.readBoolean
    )
    case 2 => Turn_Stat(
      s.readInt, s.readInt, s.readDouble, s.readDouble, s.readDouble,
      s.readDouble
    )
    case 3 => Heartbeat_Stat(
      s.readInt, s.readInt, s.readInt, s.readDouble, s.readInt, s.readInt,
      s.readInt
    )
  }

  private def read_intersections(s: ObjectInputStream): Array[MkIntersection] =
  {
    val ls = new Array[MkIntersection](s.readInt)
    for (id <- Range(0, ls.size)) {
      ls(id) = MkIntersection(
        id, IntersectionType(s.readInt), OrderingType(s.readInt)
      )
    }
    return ls
  }
}

abstract class StatAnalysis(dir: String) {
  def process(stat: Measurement)
  def finish()

  def histogram[T](
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
}

class TurnTimeAnalysis(dir: String) extends StatAnalysis(dir) {
  var intersections: Array[MkIntersection] = null

  // Histogram showing times, broken down by just policy.
  val delays_per_policy = IntersectionType.values.toList.map(
    p => p -> new ArrayBuffer[Double]()
  ).toMap

  def process(stat: Measurement) = stat match {
    case s: Turn_Stat => {
      delays_per_policy(intersections(s.vert).policy) += s.accept_delay
    }
    case Scenario_Stat(_, i) => {
      intersections = i
    }
    case _ =>
  }

  def finish() = {
    // TODO show min (and where/who), max, average
    // TODO correlate with the combos of intersection policy and ordering
    // TODO correlate with agent budget (only expect relation when Auction
    // ordering)

    histogram(
      data = delays_per_policy,
      title = "Turn delays (from request to acceptance)",
      x_axis = "Delay (s)",
      fn = s"$dir/turn_delay_per_policy.png"
    )
  }
}

class TripTimeAnalysis(dir: String) extends StatAnalysis(dir) {
  // TODO range? doubt these... and sci notatn sucks...
  var unweighted_total = BigDecimal(0.0)
  var weighted_total = BigDecimal(0.0)
  var count_finished = 0
  var count_unfinished = 0

  // TODO show min (and where/who), max, average
  // TODO lots of weighted/unweighted stuff

  // Histogram showing times, broken down by just route.
  val times_per_route = RouteType.values.toList.map(
    r => r -> new ArrayBuffer[Double]()
  ).toMap

  def process(stat: Measurement) = stat match {
    case s: Agent_Lifetime_Stat => {
      if (s.finished) {
        times_per_route(s.route) += s.trip_time
        unweighted_total += s.trip_time
        weighted_total += s.weighted_value
        count_finished += 1
      } else {
        count_unfinished += 1
      }
    }
    case _ =>
  }

  def finish() = {
    histogram(
      data = times_per_route,
      title = "Agent trip times",
      x_axis = "Total time (s)",
      fn = s"$dir/trip_time_per_route.png"
    )

    val unweighted = Util.comma_num_big(unweighted_total.toBigInt)
    val weighted = Util.comma_num_big(weighted_total.toBigInt)
    val strays = Util.comma_num(count_unfinished)
    // Since we can't count trip time for unfinished agents, normalize by the
    // number that did finish.
    val unweighted_normed = Util.comma_num_big(
      (unweighted_total / count_finished).toBigInt
    )
    val weighted_normed = Util.comma_num_big(
      (weighted_total / count_finished).toBigInt
    )
    Util.log(s"Unweighted total trip time: $unweighted ($unweighted_normed normalized)")
    Util.log(s"Weighted total trip time: $weighted ($weighted_normed normalized)")
    Util.log(s"$strays agents didn't finish their route")
    // For automatic scraping
    Util.log(s"TABLE:${unweighted}:${weighted}:${strays}:${unweighted_normed}:${weighted_normed}")
  }
}

class AgentCountAnalysis(dir: String) extends StatAnalysis(dir) {
  val live_count = new XYSeries("Live agents")
  val active_count = new XYSeries("Active agents")
  val done_count = new XYSeries("Finished agents")
  var total_done = 0

  def process(stat: Measurement) = stat match {
    case s: Heartbeat_Stat => {
      live_count.add(s.tick, s.live_agents)
      active_count.add(s.tick, s.active_agents)
    }
    case s: Agent_Lifetime_Stat if s.finished => {
      total_done += 1
      done_count.add(s.end_tick, total_done)
    }
    case _ =>
  }

  def finish() = {
    val dataset = new XYSeriesCollection()
    dataset.addSeries(live_count)
    dataset.addSeries(active_count)
    dataset.addSeries(done_count)

    // TODO use some TimeSeries for nicer printing of x axis?
    val chart = ChartFactory.createXYLineChart(
      "Agent counts per time", "Time (s)", "Number of agents", dataset,
      PlotOrientation.VERTICAL, true, false, false
    )
    val img = chart.createBufferedImage(800, 600)
    ImageIO.write(img, "png", new File(s"$dir/agent_count_per_time.png"))
  }
}
