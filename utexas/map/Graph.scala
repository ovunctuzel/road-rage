package utexas.map

import scala.collection.mutable.{HashMap, PriorityQueue}

import utexas.map.make.Reader

import utexas.Util

class Graph(val roads: List[Road], val edges: List[Edge],
            val vertices: List[Vertex], val wards: List[Ward],
            val special_ward: Ward, val width: Double, val ht: Double,
            val xoff:Double, val yoff:Double, val sc:Double)
{
  val height: Double = Graph.setHeight(ht)
  val xOff: Double = Graph.setXOff(xoff)
  val yOff: Double = Graph.setYOff(yoff)
  val scale: Double = Graph.setScale(sc) //TODO  Is there a better way to "set static variables"?
  
  val turns = vertices.foldLeft(List[Turn]())((l, v) => v.turns.toList ++ l)

  // Tell road about their ward
  for (w <- special_ward :: wards) {
    w.roads.foreach(r => r.ward = w)
  }

  // Pre-compute all-pairs shortest path
  private val t = Util.timer("floyd marshall")
  floyd_warshall
  t.stop

  def traversables() = edges ++ turns

  // TODO eventually make this very general and reusable
  // Returns the sequence of both edges and turns. Lane-changes are implicit
  // (two edges without a turn in between). First step is NOT 'from', but last
  // step is 'to'.
  def pathfind_astar(from: Edge, to: Edge): List[Traversable] = {
    // This is only used internally right now
    class Step(val on: Traversable, val dist: Double) extends Ordered[Step] {
      // This orders small distances first
      def compare(other: Step) = other.dist.compare(dist)
    }

    // If this is requested, presumably they don't want us to return an empty
    // path
    val loop = from == to
    // Used for heuristic
    val goal_pt = to.start_pt
    // encodes where we've visited and the backreferences for getting there
    val visited = new HashMap[Traversable, Traversable]()
    val open = new PriorityQueue[Step]()

    def collect_path(at: Traversable): List[Traversable] = at match {
      case null => Nil
      case _    => {
        // clean as we go to break loops
        val prev = visited.remove(at)
        prev match {
          case Some(step) => collect_path(step) ++ List(at)
          case _          => List(at)   // the start of a looped path
        }
      }
    }

    // Start
    open.enqueue(new Step(from, 0))
    visited(from) = null  // some way of encoding the start

    // Main loop
    var first = true
    while (!open.isEmpty) {
      val step = open.dequeue

      // Are we there yet?
      if (step.on == to && !first) {
        // Don't return the first step as 'from'
        val path = collect_path(step.on)
        assert(path.head == from)
        return path.tail
      }

      // Where can we go next?
      for (next <- step.on.leads_to) {
        if ((loop && next == from) || !visited.contains(next)) {
          // TODO do we have to relax / handle finding a shorter path?
          // TODO there are probably better heuristics than euclid
          val heuristic = next.start_pt.euclid_dist(goal_pt)
          open.enqueue(new Step(next, step.dist + next.length + heuristic))
          visited(next) = step.on
        }
      }

      first = false
    }

    // We didn't find the way?! The graph is connected!
    throw new Exception("Couldn't A* from " + from + " to " + to)
  }

  def floyd_warshall() = {
    // Interpret our graph as lanes being vertices and turns being edges. A bit
    // odd, but it makes sense for this.

    // Initialize the quadratic arrays.
    Util.log("tabulating 1")
    val costs = Array.tabulate(edges.size, edges.size)(
      (a, b) => edges(a).turn_to(edges(b)) match {
        case Some(turn) => edges(a).length + turn.length
        case None       => Double.MaxValue
      }
    )
    Util.log("tabulating 2")
    // Start this one knowing nothing.
    val steps = Array.tabulate(edges.size, edges.size)((a, b) => -1)

    Util.log("tabulating 3")
    // Main magic
    for (k <- (0 until edges.size)) {
      Util.log("floyd marshall " + k + " of " + edges.size)
      for (i <- (0 until edges.size)) {
        for (j <- (0 until edges.size)) {
          costs(i)(j) = math.min(costs(i)(j), costs(i)(k) + costs(k)(j))
          steps(i)(j) = k    // remember the intermediate step
        }
      }
    }
  }
}

object Graph {
  var scale: Double = 1.0
  var xOff, yOff, height: Double = 0.0
  def setScale(sc: Double): Double = {scale = sc; return scale;}
  def setXOff(xoff: Double): Double = {xOff = xoff; return xOff;}
  def setYOff(yoff: Double): Double = {yOff = yoff; return yOff;}
  def setHeight(ht: Double): Double = {height = ht; return height;}
  def load(fn: String) = (new Reader(fn)).load_map
  def worldToGPS(pt: Coordinate): Coordinate =
    new Coordinate((pt.x / scale) - xOff,((height - pt.y) / scale) - yOff)
  def gpsToWorld(pt: Coordinate): Coordinate = 
    new Coordinate((pt.x + xOff) * scale, (pt.y + yOff) * scale)
}
