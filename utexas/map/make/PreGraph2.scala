package utexas.map.make

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import utexas.map.Coordinate

import utexas.Util

class PreGraph2(old_graph: PreGraph1) {
  // (v1, v2, road name) to the edge. used for detecting cul-de-sacs easily.
  private val edge_lookup = new HashMap[(Coordinate, Coordinate, String), PreEdge2]

  // find true edges between adjacent vertices
  Util.log("Splitting " + old_graph.edges.length + " roads into edges between intersections")
  var edges = old_graph.edges flatMap split_road

  def split_road(road: PreEdge1): List[PreEdge2] = {
    // Walk the list of points in this edge, discovering chains between
    // vertices

    // TODO I think this could potentially be some awesome recursion
    // TODO or otherwise written in essentially one line ;)
    val split_edges = new ListBuffer[PreEdge2]

    var start = 0   // first point in an edge is guaranteed to be a vertex
    // List.range is [lower, upper)
    for (i <- List.range(0, road.points.length)) {
      if (start != i && old_graph.is_vert(road.points(i))) {
        // so we have an edge from start to i
        // slice is [from, till), hence the +1
        val e = find_or_make_edge(road.points.slice(start, i + 1), road)
        if (e != null) {
          split_edges += e
          // TODO yield?
        }
        start = i
      }
    }
    //assert(start == road.points.length - 1);

    // maybe Nil, if so, flatMap doesn't care
    // honey badger doesn't give a fuck
    return split_edges.toList
  }

  // null if it's already there
  def find_or_make_edge(points: MutableList[Coordinate], edge_dat: PreEdge1): PreEdge2 = {
    val v1 = points.head
    val v2 = points.last

    // do we already have an edge from v1->v2 or v2->v1?
    // this handling mainly needs to deal with cul-de-sacs

    // TODO do the ternary if thingy
    if (edge_lookup.contains((v1, v2, edge_dat.name)) ||
        edge_lookup.contains((v2, v1, edge_dat.name)))
    {
      return null
    } else {
      return new PreEdge2(v1, v2, points, edge_dat)
    }
    // TODO any need to make sure point data matches up if we matched?
    // TODO in fact, when do we already match?
  }
}

// TODO another glorified struct?
class PreEdge2(val from: Coordinate, val to: Coordinate,
               val points: MutableList[Coordinate], val dat: PreEdge1) {}
