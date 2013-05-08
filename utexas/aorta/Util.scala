// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta

import java.util.Random // TODO use scala one when its serializable in 2.11
import java.io.{FileWriter, Serializable}
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.io.Source

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream, PrintWriter, BufferedReader, FileReader}

import utexas.aorta.map.Graph
import utexas.aorta.sim.{Simulation, Scenario}
import utexas.aorta.analysis.{Stats, Timer}

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
    // First argument must be the map/scenario
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
      key match {
        case "--log" => { Stats.setup_logging(value) }
        case "--time_limit" => { Common.time_limit = value.toDouble }
        case _ => { Util.log("Unknown argument: " + key); sys.exit }
      }
    }

    val sim = Scenario.load_or_default_sim(load)
    sim.setup()
    return sim
  }

  def serialize(obj: Any, fn: String) = {
    val out = new ObjectOutputStream(new FileOutputStream(fn))
    out.writeObject(obj)
    out.close
  }

  def unserialize(fn: String): Any = {
    val in = new ObjectInputStream(new FileInputStream(fn))
    val obj = in.readObject
    in.close
    return obj
  }

  def diff[T](a: T, b: T, header: String = ""): Option[String] =
    if (a == b)
      None
    else
      Some(s"$header ($a -> $b)")
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

// TODO override with cmdline too
// TODO grab alt cfg file from cmdline
object cfg {
  private val cfg_fn = "default.cfg"
  private val params = load_config()

  def load_config(): Map[String, String] =
    Source.fromFile(cfg_fn).getLines().flatMap(line => kv(line)).toMap

  private def kv(line: String): Option[(String, String)] = {
    val cleaned = line.split("#").head.trim.replaceAll("""\s+""", " ")
    if (cleaned.isEmpty) {
      return None
    } else {
      val Array(key, value) = cleaned.split("=")
      return Some((key.trim, value.trim))
    }
  }

  private def bool(x: String) =
    if (x == "true")
      true
    else if (x == "false")
      false
    else
      throw new Exception(s"Bad boolean in config: $x")

  val dt_s = params("dt_s").toDouble
  val epsilon = params("epsilon").toDouble
  val end_threshold = params("end_threshold").toDouble
  val follow_dist = params("follow_dist").toDouble
  val max_accel = params("max_accel").toDouble
  val signal_duration = params("signal_duration").toInt
  val lane_width = params("lane_width").toDouble
  val lanechange_dist = lane_width * params("lanechange_dist_rate").toDouble
  
  val army_size = params("army_size").toInt
  val policy = params("policy")
  val ordering = params("ordering")
  val route = params("route")
  val wallet = params("wallet")

  val antialias = bool(params("antialias"))
  val draw_cursor = bool(params("draw_cursor"))
  val draw_lane_arrow = bool(params("draw_lane_arrow"))
  val dash_center = bool(params("dash_center"))
  val zoom_threshold = params("zoom_threshold").toDouble
  val max_lanes = params("max_lanes").toInt
}

// Plumbing some stuff everywhere is hard, so share here sometimes. Plus,
// convenience methods.
object Common {
  // Because turns don't directly reference edges to break a serialization cycle
  var edges: Array[utexas.aorta.map.Edge] = null
  var sim: utexas.aorta.sim.Simulation = null
  var scenario: utexas.aorta.sim.Scenario = null
  var time_limit = -1.0

  def tick = sim.tick
  def timer(name: String) = new Timer(name)

  sys.ShutdownHookThread({
    if (sim != null) {
      sim.agents.foreach(a => a.terminate(interrupted = true))
    }
  })
}

// All these assume cfg.max_accel, which will be per-car in the future.
object Physics {
  // Capped when speed goes negative.
  def dist_at_constant_accel(accel: Double, time: Double, initial_speed: Double): Double = {
    // Don't deaccelerate into going backwards, just cap things off.
    val actual_time = if (accel >= 0)
                        time
                      else
                        math.min(time, -1 * initial_speed / accel)
    return (initial_speed * actual_time) + (0.5 * accel * (actual_time * actual_time))
  }

  // TODO this gets a bit more conservative when cars have different
  // accelerations. This is hinged on the fact that lookahead works. Agents
  // can't enter e faster than its speed limit, so we have to reason about how
  // far they could possibly go.
  def worst_entry_dist(lim: Double): Double = {
    val accel = cfg.max_accel
    val stopping_dist = dist_at_constant_accel(-accel, lim / accel, lim)
    return (lim * cfg.dt_s) + stopping_dist
  }

  // stopping time comes from v_f = v_0 + a*t
  // negative accel because we're slowing down.
  def stopping_distance(speed: Double) = dist_at_constant_accel(
    -cfg.max_accel, speed / cfg.max_accel, speed
  )

  // We'll be constrained by the current edge's speed limit and maybe other
  // stuff, but at least this speed lim.
  def max_next_accel(speed: Double, limit: Double) =
    math.min(cfg.max_accel, (limit - speed) / cfg.dt_s)
  def max_next_speed(speed: Double, limit: Double) =
    speed + (max_next_accel(speed, limit) * cfg.dt_s)
  def max_next_dist(speed: Double, limit: Double) =
    dist_at_constant_accel(max_next_accel(speed, limit), cfg.dt_s, speed)
  def max_next_dist_plus_stopping(speed: Double, limit: Double) =
    max_next_dist(speed, limit) + stopping_distance(max_next_speed(speed, limit))
  def max_lookahead_dist(speed: Double, limit: Double) =
    max_next_dist_plus_stopping(speed, limit)

  def min_next_speed(speed: Double) =
    math.max(0.0, speed + (cfg.dt_s * -cfg.max_accel))
  def min_next_dist(speed: Double) =
    dist_at_constant_accel(-cfg.max_accel, cfg.dt_s, speed)
  def min_next_dist_plus_stopping(speed: Double) =
    min_next_dist(speed) + stopping_distance(min_next_speed(speed))
  
  def accel_to_achieve(target_speed: Double, speed: Double) =
    (target_speed - speed) / cfg.dt_s

  // d = (v_i)(t) + (1/2)(a)(t^2), solved for a
  def accel_to_cover(dist: Double, speed: Double) =
    (2 * (dist - (speed * cfg.dt_s)) / (cfg.dt_s * cfg.dt_s))

  // To stop in one time-step, that is. From v_f = v_i + at
  def accel_to_stop(speed: Double) = (-1 * speed) / cfg.dt_s
}

// TODO make stats, maps, scenarios -- everything -- use these.
abstract class StateWriter(fn: String) {
  def done()
  def int(x: Int)
  def double(x: Double)
  def string(x: String)
  def bool(x: Boolean)
  def obj(x: Any)   // TODO remove.

  // TODO a macro to do lots of these in one line?
}

class BinaryStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new ObjectOutputStream(new FileOutputStream(fn))
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.writeInt(x)
  }
  def double(x: Double) {
    out.writeDouble(x)
  }
  def string(x: String) {
    out.writeUTF(x)
  }
  def bool(x: Boolean) {
    out.writeBoolean(x)
  }
  def obj(x: Any) {
    // TODO dont use this.
    out.writeObject(x)
  }
}

class StringStateWriter(fn: String) extends StateWriter(fn) {
  private val out = new PrintWriter(fn)
  def done() {
    out.close()
  }

  def int(x: Int) {
    out.println(x)
  }
  def double(x: Double) {
    out.println(x)
  }
  def string(x: String) {
    out.println(x)
  }
  def bool(x: Boolean) {
    out.println(x)
  }
  def obj(x: Any) {
    // TODO dont use this.
    out.println(x)
  }
}

abstract class StateReader(fn: String) {
  def int: Int
  def double: Double
  def string: String
  def bool: Boolean
  def obj: Any    // TODO remove
}

class BinaryStateReader(fn: String) extends StateReader(fn) {
  private val in = new ObjectInputStream(new FileInputStream(fn))
  def int = in.readInt
  def double = in.readDouble
  def string = in.readUTF
  def bool = in.readBoolean
  def obj = in.readObject   // TODO dont use this.
}

class StringStateReader(fn: String) extends StateReader(fn) {
  private val in = new BufferedReader(new FileReader(fn))
  def int = in.readLine.toInt
  def double = in.readLine.toDouble
  def string = in.readLine
  def bool = in.readLine.toBoolean
  def obj: Any = {
    in.readLine
    return null   // TODO dont use this.
  }
}
