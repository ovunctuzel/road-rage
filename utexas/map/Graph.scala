package utexas.map

import utexas.map.make.Reader

import utexas.Util

class Graph(val roads: List[Road], val edges: List[Edge],
            val vertices: List[Vertex], val wards: List[Ward],
            val special_ward: Ward, val width: Double, val height: Double)
{
  // Tell road about their ward
  for (w <- special_ward :: wards) {
    w.roads.foreach(r => r.ward = w)
  }

  // also fixed constraints: residential types and decent length
  // Note this one of those scary things that might not return
  def random_edge_except(except: Set[Edge]): Edge = {
    val min_len = 1.0 // TODO cfg. what unit is this in?
    val e = Util.choose_rand(edges)
    if (!except.contains(e) && e.road.road_type == "residential" && e.length > min_len) {
      return e
    } else {
      return random_edge_except(except)
    }
  }
}

object Graph {
  def load(fn: String) = (new Reader(fn)).load_map
}
