package utexas

import scala.util.Random
import java.io.FileWriter

object Util {
  def timer(msg: String) = new Timer(msg)
  def stopwatch = new Stopwatch
  
  private var indent_log = 0
  def log_push = { indent_log += 1 }
  def log_pop =  { indent_log -= 1 }
  def indent = "  " * indent_log
  def log(msg: String) = println(indent + msg)

  private var rng: Random = null  // icky...

  var seed: Long = -1

  def init_rng(s: Long) = {
    seed = s
    rng = new Random(seed)
    Util.log("RNG seed: " + seed)
  }

  // Convenient to see this at the very end if it was a long log.
  scala.sys.ShutdownHookThread({
    Util.log("\nRNG seed: " + Util.seed)
    Stats.shutdown
  })

  def rand_double(min: Double, max: Double): Double = {
    if (min > max) {
      throw new Exception("rand(" + min + ", " + max + ") requested")
    } else if (min == max) {
      return min
    } else {
      return min + rng.nextDouble * (max - min)
    }
  }
  def rand_int(min: Int, max: Int) = rand_double(min, max).toInt
  def choose_rand[T](from: Seq[T]): T = from(rng.nextInt(from.length))
  // return true 'p'% of the time. p is [0.0, 1.0]
  def percent(p: Double) = rand_double(0.0, 1.0) < p

  // due to internationalization, printf doesn't have a way of adding commas
  // after every 3 orders of mag
  def comma_num(n: Int, pad: Boolean = true): String = {
    return if (n < 1000 && pad)
             "%03d".format(n)
           else if (n < 1000)
             "" + n
           else
             comma_num(n / 1000, pad = false) + "," + comma_num(n % 1000)
  }

  // to meters/sec, that is. SI units.
  def mph_to_si(r: Double) = r * 0.44704

  // All in SI units
  def dist_at_constant_accel(accel: Double, time: Double, initial_speed: Double)
    = (initial_speed * time) + (0.5 * accel * (time * time))
  def accel_to_achieve(cur_speed: Double, target_speed: Double)
    = (target_speed - cur_speed) / cfg.dt_s
  // accelerate first, then cruise at constant
  def two_phase_time(speed_i: Double = 0, speed_f: Double = 0, dist: Double = 0,
                     accel: Double = 0): Double =
  {
    // v_f = v_i + a*t
    val time_to_cruise = (speed_f - speed_i) / accel
    val dist_during_accel = dist_at_constant_accel(accel, time_to_cruise, speed_i)

    if (dist_during_accel < dist) {
      // so then the remainder happens at constant cruisin speed
      return time_to_cruise + ((dist - dist_during_accel) / speed_f)
    } else {
      // We spent the whole time accelerating
      // solve dist = a(t^2) + (v_i)t
      val discrim = math.sqrt((speed_i * speed_i) + (4 * accel * dist))
      val time = (-speed_i + discrim) / (2 * accel) // this is the positive root
      assert(time >= 0)   // make sure we have the right solution to this
      return time
    }
  }
}

// use nanoseconds for both; it's nice and sensitive
class Timer(msg: String) {
  val start = System.nanoTime

  def so_far = (System.nanoTime - start) / 1000000000.0

  def stop = {
    Util.log("\"" + msg + "\": " + so_far + "s")
  }
}

class Stopwatch() {
  var from: Long = System.nanoTime
  var seconds: Double = 0.0

  def start = {
    from = System.nanoTime
  }

  def stop = {
    val now = System.nanoTime
    seconds += (now - from) / 1000000000.0
  }

  def reset = {
    seconds = 0.0
    start
  }
}

object cfg {
  // TODO marking things as configurable or not from the UI.
  val bools = List(
    ("antialias",       true,  "Draw nicer lines more slowly?"),
    ("draw_cursor",     false, "Draw the epsilon bubble around the cursor?"),
    ("draw_lane_arrow", true,  "Draw arrows to show lane orientation?"),
    ("dash_center",     true,  "Dash the center yellow line?")
  ) map {c => c._1 -> new Bool_Cfgable(c._2, c._3)} toMap

  // all distances should be in meters
  val doubles = List(
    ("lane_width",      0.5, "Width of a lane in meters", 0.01, 0.1),
    ("zoom_threshold",  5.0, "How close to zoom in before drawing details",    1.0, 15.0),
    ("epsilon",     0.00001, "What do we take as zero due to FP imprecision?", 0.0,  1.0),
    ("dt_s",            0.1, "The only dt in seconds an agent can experience", 0.1,  3.0),
    // account for crosswalks, vehicle length...
    ("end_threshold",   5.0, "The end of a traversable is its length - this",  0.1,  3.0),
    // this kind of gives dimension to cars, actually
    ("follow_dist",     20.0, "Even if stopped, don't get closer than this", 0.1, 1.0),
    ("max_accel",       2.7, "A car's physical capability",                  0.5, 5.0),
    ("pause_at_stop",   1.0, "Wait this long at a stop sign before going",   0.5, 5.0),
    ("min_lane_length", 0.1, "It's unreasonable to have anything shorter",   0.0, 1.0),
    ("thruput_stat_time", 60.0, "Record throughput per this time",   0.5, 5.0)
  ) map {c => c._1 -> new Double_Cfgable(c._2, c._3, c._4, c._5)} toMap

  val ints = List(
    ("max_lanes",  20,  "The max total lanes a road could have", 1, 30),
    ("army_size",  500, "How many agents to spawn by default", 1, 10000),
    ("signal_duration",  60, "How long for a traffic signal to go through all of its cycles", 4, 1200)
  ) map {c => c._1 -> new Int_Cfgable(c._2, c._3, c._4, c._5)} toMap

  // TODO val colors = ... (I'm not kidding)
  // TODO and of course, a way to save the settings.

  // TODO i wanted to use reflection to generate methods, but that won't work...
  // jvm limitations.
  def antialias       = bools("antialias").value
  def draw_cursor     = bools("draw_cursor").value
  def draw_lane_arrow = bools("draw_lane_arrow").value
  def dash_center     = bools("dash_center").value

  def lane_width      = doubles("lane_width").value
  def zoom_threshold  = doubles("zoom_threshold").value
  def epsilon         = doubles("epsilon").value
  def end_threshold   = doubles("end_threshold").value
  def follow_dist     = doubles("follow_dist").value
  def max_accel       = doubles("max_accel").value
  def pause_at_stop   = doubles("pause_at_stop").value
  def min_lane_length = doubles("min_lane_length").value
  def thruput_stat_time = doubles("thruput_stat_time").value

  def max_lanes       = ints("max_lanes").value
  def army_size       = ints("army_size").value
  def signal_duration    = ints("signal_duration").value
  
  def dt_s            = doubles("dt_s").value
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

sealed trait Measurement {}
final case class Wasted_Time_Stat(agent: Int, intersection: Int, lag: Double) extends Measurement {
  override def toString = "s1 %d %d %.2f".format(agent, intersection, lag)
}
final case class Trip_Time_Stat(agent: Int, time: Double) extends Measurement {
  override def toString = "s2 %d %.2f".format(agent, time)
}
final case class Intersection_Throughput_Stat(intersection: Int, requests: Int,
                                              entered: Int, timespan: Double)
  extends Measurement
{
  override def toString = "s3 %d %d %d %.1f".format(intersection, requests, entered, timespan)
}

object Stats {
  var log: FileWriter = null
  var use_log = false
  var use_print = false

  def record(item: Measurement) = {
    if (use_log) {
      if (log == null) {
        log = new FileWriter("stats_log")   // TODO use right buffering
      }
      log.write(item.toString + "\n")
    }
    if (use_print) {
      Util.log("Stat: " + item)
    }
  }

  // flush any logs we were writing
  def shutdown() = {
    if (log != null) {
      log.close
    }
  }
}
