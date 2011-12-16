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
