package utexas

object Util {
  def timer(msg: String) = new Timer(msg)
  
  private var indent_log = 0
  def log_push = { indent_log += 1 }
  def log_pop =  { indent_log -= 1 }
  def indent = "  " * indent_log
  def log(msg: String) = println(indent + msg)
}

class Timer(msg: String) {
  val start = System.currentTimeMillis
  def stop = {
    val now = System.currentTimeMillis
    Util.log("\"" + msg + "\": " + ((now - start) / 1000.0) + "s")
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

  val doubles = List(
    ("lane_width",     0.05, "Width of a lane", 0.01, 0.1),
    ("zoom_threshold", 5.0,  "How close to zoom in before drawing details", 1.0, 15.0),
    ("epsilon", 0.00001,  "What do we take as zero due to FP imprecision?", 0.0, 1.0)
  ) map {c => c._1 -> new Double_Cfgable(c._2, c._3, c._4, c._5)} toMap

  val ints = List(
    ("max_lanes",  20, "The max total lanes a road could have", 1, 30)
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
  def epsilon  = doubles("epsilon").value

  def max_lanes       = ints("max_lanes").value
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
