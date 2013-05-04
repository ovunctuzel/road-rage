// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta

import java.util.Random // TODO use scala one when its serializable in 2.11
import java.io.FileWriter
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import java.io.{ObjectOutputStream, FileOutputStream, ObjectInputStream,
                FileInputStream}

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

    val cfg_prefix = """--cfg_(\w+)""".r
    for ((key, value) <- keys.zip(vals)) {
      key match {
        case "--log" => { Stats.setup_logging(value) }
        case "--time_limit" => { Common.time_limit = value.toDouble }
        case cfg_prefix(param) => cfg.get_param_method(param) match {
          case Some(method) => {
            val param_type = method.getParameterTypes.head
            if (param_type == classOf[Int]) {
              method.invoke(cfg, value.toInt: java.lang.Integer)
            } else if (param_type == classOf[Double]) {
              method.invoke(cfg, value.toDouble: java.lang.Double)
            } else if (param_type == classOf[Boolean]) {
              method.invoke(cfg, (value == "1"): java.lang.Boolean)
            } else {
              method.invoke(cfg, value: java.lang.String)
            }
          }
          case None => { Util.log(s"No config param $param"); sys.exit }
        }
        case _ => { Util.log("Unknown argument: " + key); sys.exit }
      }
    }

    return Scenario.load_or_default_sim(load)
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

class RNG(seed: Long = System.currentTimeMillis) {
  private val rng = new Random(seed)

  def serialize(w: StateWriter) {
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

object cfg {
  // TODO marking things as configurable or not from the UI.
  val bools = List(
    ("antialias",       true,  "Draw nicer lines more slowly?"),
    ("draw_cursor",     false, "Draw the epsilon bubble around the cursor?"),
    ("draw_lane_arrow", true,  "Draw arrows to show lane orientation?"),
    ("dash_center",     true,  "Dash the center yellow line?")
  ).map({c => c._1 -> new Bool_Cfgable(c._2, c._3)}).toMap

  // all distances should be in meters
  val doubles = List(
    ("lane_width",      0.5, "Width of a lane in meters", 0.01, 0.1),
    ("zoom_threshold",  5.0, "How close to zoom in before drawing details",    1.0, 15.0),
    ("epsilon",       1E-10, "What do we take as zero due to FP imprecision?", 0.0,  1.0),
    ("dt_s",            1.0, "The only dt in seconds an agent can experience", 0.1,  3.0),
    // account for crosswalks, vehicle length...
    ("end_threshold",   5.0, "The end of a traversable is its length - this",  0.1,  3.0),
    // this kind of gives dimension to cars, actually
    ("follow_dist",     5.0, "Even if stopped, don't get closer than this", 0.1, 1.0),
    ("max_accel",       2.7, "A car's physical capability",                  0.5, 5.0)
  ).map({c => c._1 -> new Double_Cfgable(c._2, c._3, c._4, c._5)}).toMap

  val ints = List(
    ("max_lanes",  50,  "The max total lanes a road could have", 1, 30),
    ("army_size",  5000, "How many agents to spawn by default", 1, 10000),
    ("signal_duration",  60, "How long for a traffic signal to go through all of its cycles", 4, 1200)
  ).map({c => c._1 -> new Int_Cfgable(c._2, c._3, c._4, c._5)}).toMap

  val strings = List(
    ("policy", "StopSign"),
    ("ordering", "FIFO"),
    ("route", "Path"),
    ("wallet", "Fair")
  ).map({c => c._1 -> new String_Cfgable(c._2)}).toMap

  // TODO val colors = ... (I'm not kidding)
  // TODO and of course, a way to save the settings.

  // TODO i wanted to use reflection to generate methods, but that won't work...
  // jvm limitations.
  // TODO these shortcuts take a bit extra memory, but avoid lots of hash
  // lookups. real solution is to not have methods and then also objects...

  var antialias = false
  var draw_cursor = false
  var draw_lane_arrow = false
  var dash_center = false

  var lane_width = 0.0
  var zoom_threshold = 0.0
  var epsilon = 0.0
  var end_threshold = 0.0
  var follow_dist = 0.0
  var max_accel = 0.0
  var dt_s = 0.0

  var max_lanes = 0
  var army_size = 0
  var signal_duration = 0

  var policy = ""
  var ordering = ""
  var route = ""
  var wallet = ""

  // Do this early.
  init_params

  def lanechange_dist = lane_width * 5.0

  def get_param_method(name: String) =
    cfg.getClass.getDeclaredMethods.find(_.getName == name + "_$eq")

  // Set all the config stuff from the hashes.
  def init_params() = {
    for ((key, value) <- bools) {
      get_param_method(key).get.invoke(cfg, value.value: java.lang.Boolean)
    }
    for ((key, value) <- doubles) {
      get_param_method(key).get.invoke(cfg, value.value: java.lang.Double)
    }
    for ((key, value) <- ints) {
      get_param_method(key).get.invoke(cfg, value.value: java.lang.Integer)
    }
    for ((key, value) <- strings) {
      get_param_method(key).get.invoke(cfg, value.value: java.lang.String)
    }
  }
}

// couldn't quite the OO work out to bundle these
class Bool_Cfgable(default: Boolean, descr: String) {
  var value = default
}

class Double_Cfgable(default: Double, descr: String, min: Double, max: Double) {
  var value = default
}

class Int_Cfgable(default: Int, descr: String, min: Int, max: Int) {
  var value = default
}

class String_Cfgable(default: String) {
  var value = default
}

// Plumbing some stuff everywhere is hard, so share here sometimes. Plus,
// convenience methods.
object Common {
  // Because turns don't directly reference edges to break a serialization cycle
  var edges: Array[utexas.aorta.map.Edge] = null

  var sim: utexas.aorta.sim.Simulation = null
  def tick = sim.tick
  var time_limit = -1.0

  def timer(name: String) = new Timer(name)

  var scenario: utexas.aorta.sim.Scenario = null

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
class StateWriter(fn: String) {
  private val out = new ObjectOutputStream(new FileOutputStream(fn))

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

  // TODO dont use this.
  def obj(x: Any) {
    out.writeObject(x)
  }

  def done() {
    out.close()
  }
}

class StateReader(fn: String) {
  private val in = new ObjectInputStream(new FileInputStream(fn))

  def int = in.readInt

  def double = in.readDouble

  def string = in.readUTF

  def bool = in.readBoolean

  // TODO dont use this.
  def obj = in.readObject
}
