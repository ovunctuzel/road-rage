// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.util.Random // TODO use scala one when its serializable in 2.11
import java.io.{FileWriter, Serializable, File}
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import java.awt.Color
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.map.Graph
import utexas.aorta.sim.{Simulation, Scenario}
import utexas.aorta.analysis.{Timer, StatsListener, StatsRecorder, Measurement}

object Util {
  private var indent_log = 0
  def log_push = { indent_log += 1 }
  def log_pop =  { indent_log -= 1 }
  def indent = "  " * indent_log
  def log(msg: String) = println(indent + msg)

  // due to internationalization, printf doesn't have a way of adding commas
  // after every 3 orders of mag
  def comma_num(n: Int, pad: Boolean = false): String =
    if (n < 1000 && pad)
      "%03d".format(n)
    else if (n < 1000)
      "" + n
    else
      comma_num(n / 1000, pad) + "," + comma_num(n % 1000, pad = true)
  def comma_num_big(n: BigInt, pad: Boolean = false): String =
    if (n < 1000 && pad)
      "%03d".format(n)
    else if (n < 1000)
      "" + n
    else
      comma_num_big(n / 1000, pad) + "," + comma_num_big(n % 1000, pad = true)

  // print HH:MM:SS
  def time_num(total: Double): String = {
    val hours = math.floor(total / 3600).toInt
    val minutes = math.floor((total - 3600.0 * hours) / 60).toInt
    val seconds = math.floor(total - (3600.0 * hours) - (60.0 * minutes)).toInt
    return "%02d:%02d:%02d".format(hours, minutes, seconds)
  }

  @elidable(ASSERTION) def assert_eq(a: Any, b: Any) = assert(a == b, a + " != " + b)
  @elidable(ASSERTION) def assert_ne(a: Any, b: Any) = assert(a != b, a + " == " + b)
  @elidable(ASSERTION) def assert_gt(a: Double, b: Double) = assert(a > b, a + " <= " + b)
  @elidable(ASSERTION) def assert_ge(a: Double, b: Double) = assert(a >= b, a + " < " + b)
  @elidable(ASSERTION) def assert_lt(a: Double, b: Double) = assert(a < b, a + " >= " + b)
  @elidable(ASSERTION) def assert_le(a: Double, b: Double) = assert(a <= b, a + " > " + b)

  def process_args(raw_args: Array[String]): Simulation = {
    // First argument must be the map/scenario/savestate
    val load = raw_args.head
    val args = raw_args.tail

    if (args.size % 2 != 0) {
      // TODO better usage
      Util.log("Command-line parameters must be pairs of key => value")
      sys.exit
    }

    // TODO write with 'partition'
    val keys = args.zipWithIndex.filter(p => p._2 % 2 == 0).map(p => p._1)
    val vals = args.zipWithIndex.filter(p => p._2 % 2 == 1).map(p => p._1)

    for ((key, value) <- keys.zip(vals)) {
      // TODO can't detect unknown args yet. :(
      Flags.set(key, value)
    }

    if (load.startsWith("scenarios/savestate_")) {
      return Simulation.unserialize(reader(load))
    } else if (load.startsWith("scenarios/")) {
      val sim = Scenario.load(load).make_sim()
      sim.setup()
      return sim
    } else if (load.startsWith("maps/")) {
      val g = Graph.load(load)
      val sim = Scenario.default(load, g).make_sim(g)
      sim.setup()
      return sim
    } else {
      throw new Exception(s"First parameter must be a savestate, scenario, or map.")
    }
  }

  def diff[T](a: T, b: T, header: String = ""): Option[String] =
    if (a == b)
      None
    else
      Some(s"$header ($a -> $b)")

  def mkdir(path: String) {
    (new File(path)).mkdir()
  }

  def writer(fn: String) = new BinaryStateWriter(fn)
  def reader(fn: String) = new BinaryStateReader(fn)
}

class RNG(seed: Long = System.currentTimeMillis) extends Serializable {
  private val rng = new Random(seed)

  def serialize(w: StateWriter) {
    // TODO cant debug with string state RWs...
    w.obj(this)
  }

  def double(min: Double, max: Double): Double =
    if (min > max)
      throw new Exception("rand(" + min + ", " + max + ") requested")
    else if (min == max)
      min
    else
      min + rng.nextDouble * (max - min)
  def int(min: Int, max: Int) = double(min, max).toInt
  def choose[T](from: Seq[T]): T = from(rng.nextInt(from.length))
  // return true 'p'% of the time. p is [0.0, 1.0]
  def percent(p: Double) = double(0.0, 1.0) < p
  // for making a new RNG
  def new_seed = rng.nextLong
}

object RNG {
  def unserialize(r: StateReader): RNG = r.obj.asInstanceOf[RNG]
}

// Plumbing some stuff everywhere is hard, so share here sometimes. Plus,
// convenience methods.
object Common {
  // TODO make it easier to set the current active sim.
  var sim: utexas.aorta.sim.Simulation = null

  def scenario = sim.scenario
  def tick = sim.tick
  def timer(name: String) = new Timer(name)

  var stats_log: StatsListener = null
  def record(item: Measurement) {
    if (stats_log == null) {
      stats_log = Flags.string("--log") match {
        case Some(value) => new StatsRecorder(value)
        case None => new StatsListener()
      }
    }
    stats_log.record(item)
  }

  sys.ShutdownHookThread({
    if (sim != null) {
      sim.terminate()
    }
  })
}

object Flags {
  val values = new MutableMap[String, String]()

  def set(name: String, value: String) {
    values(name) = value
  }

  def string(name: String): Option[String] = values.get(name)
  def string(name: String, default: String): String = string(name).getOrElse(default)
  def int(name: String): Option[Int] = values.get(name).map(_.toInt)
  def int(name: String, default: Int): Int = int(name).getOrElse(default)
  def double(name: String): Option[Double] = values.get(name).map(_.toDouble)
  def double(name: String, default: Double): Double = double(name).getOrElse(default)
  def boolean(name: String): Option[Boolean] = values.get(name).map(is_true(_))
  def boolean(name: String, default: Boolean): Boolean = boolean(name).getOrElse(default)

  private def is_true(value: String) = value.toLowerCase == "true"
}

class AgentID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class RoadID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class VertexID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class TurnID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class EdgeID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class DirectedRoadID(val int: Int) extends AnyVal {
  override def toString = int.toString
}

// TODO value classes for durations, times, distance, speed...
