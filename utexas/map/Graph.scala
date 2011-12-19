package utexas.map

//import scala.collection.mutable.HashMap
//import scala.collection.mutable.MutableList

import utexas.map.make.Reader

// TODO I can haz named parameters? :(
class Graph(val roads: List[Road], val edges: List[Edge],
            val vertices: List[Vertex], val width: Double, val height: Double,
            val xOff: Double, val yOff: Double, val scale: Double)
{
}

object Graph {
  def load(fn: String) = (new Reader(fn)).load
}
