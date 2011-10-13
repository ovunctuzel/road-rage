package map

//import scala.collection.mutable.HashMap
//import scala.collection.mutable.MutableList

import map.make.Reader

// TODO I can haz named parameters? :(
class Graph(val roads: List[Road], val edges: List[Edge],
            val vertices: List[Vertex], val width: Double, val height: Double,
            val xOff: Double, val yOff: Double, val scale: Double)
{

  // it's important to dump vertices first, since roads ened them. and edges
  // need roads, so the order works out well.
  // this is particularly useful as a consistency test of the OSM->map and
  // deserialization code
  def to_xml() =
    <graph width={width.toString} height={height.toString}
           xoff={xOff.toString} yoff={yOff.toString} scale={scale.toString}
           roads={roads.length.toString} edges={edges.length.toString}
           verts={vertices.length.toString}>
      {vertices.map(v => v.to_xml)}
      {roads.map(r => r.to_xml)}
      {edges.map(e => e.to_xml)}
    </graph>
}

object Graph {
  def load(fn: String) = (new Reader(fn)).load
}
