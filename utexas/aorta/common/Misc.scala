// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.common

import java.io.{FileWriter, Serializable, File, PrintWriter}
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import java.awt.Color
import scala.collection.{mutable, immutable}
import scala.sys.process._
import Function.tupled

import utexas.aorta.map.Graph
import utexas.aorta.sim.{Simulation}
import utexas.aorta.sim.make.Scenario

object Util {
  private var indent_log = 0
  def log_push() {
    indent_log += 1
  }
  def log_pop() {
    indent_log -= 1
  }
  def indent = "  " * indent_log
  def log(msg: String) = println(indent + msg)

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
      val sim = Scenario.default(load).make_sim()
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

  def unique_id = System.currentTimeMillis.toString
  def output(fn: String) = new PrintWriter(new FileWriter(new File(fn)), true /* autoFlush */)
  def blockingly_run(argv: Seq[String]) = argv.!
  def bool2binary(value: Boolean) = if (value) 1.0 else 0.0

  def sorted_set[T <: Ordered[T]](stuff: Iterable[T]) = immutable.SortedSet.empty[T] ++ stuff
  def sorted_map[K <: Ordered[K], V](stuff: Iterable[(K, V)]) = immutable.SortedMap.empty[K, V] ++ stuff
}

object Flags {
  val values = new mutable.HashMap[String, String]()

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

// Ironically, using timers in tight loops has caused up to 3x slowdown before.
// Java profilers might be safer.

// One use
class Timer(val msg: String = "") {
  private val start = System.nanoTime

  def so_far = (System.nanoTime - start) / 1000000000.0

  def stop() {
    Util.log("\"" + msg + "\": " + so_far + "s")
  }
}

object Timer {
  def apply(name: String) = new Timer(name)
}

// Multi-use
class Stopwatch(val name: String) {
  private var from: Long = 0
  var seconds: Double = 0.0

  @elidable(elidable.ASSERTION) def start(): Stopwatch = {
    from = System.nanoTime
    return this
  }

  @elidable(elidable.ASSERTION) def stop() {
    val now = System.nanoTime
    seconds += (now - from) / 1000000000.0
  }

  def time[A](thunk: () => A): A = {
    try {
      start
      thunk()
    } finally {
      stop
    }
  }

  def describe() {
    Util.log(s"$name took $seconds seconds")
  }
}

abstract class MathVector[T <: MathVector[T]](val value: Array[Double]) {
  protected def produce(vector: Array[Double]): T

  def size = value.size
  def +(other: MathVector[T]): T = {
    Util.assert_eq(size, other.size)
    return produce(value.zip(other.value).map(tupled((c1, c2) => c1 + c2)))
  }
  def dot(other: MathVector[T]): Double = {
    Util.assert_eq(size, other.size)
    return value.zip(other.value).map(tupled((c1, c2) => c1 * c2)).sum
  }
}

trait Publisher[T] {
  //////////////////////////////////////////////////////////////////////////////
  // State

  private val listeners = new mutable.ListBuffer[(String, T => Any)]()

  //////////////////////////////////////////////////////////////////////////////
  // Actions

  def publish(ev: T) = listeners.foreach(l => l._2(ev))
  def listen(tag: String, subscriber: T => Any) = {
    listeners += ((tag, subscriber))
  }
  def unlisten(tag: String) = {
    listeners --= listeners.filter(l => l._1 != tag)
  }
}

class BinnedHistogram(width: Double) {
  // What's the frequency of each bin?
  val bin_counts = new mutable.HashMap[Int, Int]().withDefault((_) => 0)

  def add(n: Double) {
    bin_counts((n / width).toInt) += 1
  }
  def bins = bin_counts.keys
  def apply(bin: Int) = bin_counts(bin)
}

class AgentID(val int: Int) extends AnyVal {
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
class RoadID(val int: Int) extends AnyVal {
  override def toString = int.toString
}
class ZoneID(val int: Int) extends AnyVal with Ordered[ZoneID] {
  override def toString = int.toString
  override def compare(other: ZoneID) = int.compare(other.int)
}
class ValueOfTime(val time_per_cost: Double) extends AnyVal {
  override def toString = time_per_cost.toString
  // TODO return Time type
  //def time(cost: Price) = time_per_cost * cost.dollars
}
class Price(val dollars: Double) extends AnyVal {
  override def toString = dollars.toString
}

// TODO value classes for durations, times, distance, speed...
