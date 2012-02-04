package utexas.map.make

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

import utexas.map.{Coordinate, Line}

import utexas.Util

class PreGraph2(old_graph: PreGraph1) {
  // (v1, v2, road name) to the edge. used for detecting cul-de-sacs easily.
  private val edge_lookup = new HashMap[(Coordinate, Coordinate, String), PreEdge2]

  // find true edges between adjacent vertices
  Util.log("Splitting " + old_graph.edges.length + " roads into edges between intersections")
  var edges = old_graph.edges.flatMap(split_road)

  // This is slow, but simpler to reason about, since we don't have to maintain
  // it.
  def verts(of: Coordinate) = edges.filter(e => e.from == of || e.to == of)

  def split_road(road: PreEdge1): List[PreEdge2] = {
    // Walk the list of points in this edge, discovering chains between
    // vertices

    // TODO this is essentially a 'split at vertices'
    val split_edges = new ListBuffer[Option[PreEdge2]]

    var start = 0   // first point in an edge is guaranteed to be a vertex
    // List.range is [lower, upper)
    for (i <- List.range(0, road.points.length)) {
      if (start != i && old_graph.is_vert(road.points(i))) {
        // so we have an edge from start to i
        // slice is [from, till), hence the +1
        split_edges += find_or_make_edge(road.points.slice(start, i + 1).toList, road)
        start = i
      }
    }
    assert(start == road.points.length - 1);

    // maybe Nil, if so, flatMap doesn't care
    // honey badger doesn't give a fuck
    return split_edges.toList.flatten
  }

  // None if it's already there
  def find_or_make_edge(points: List[Coordinate], edge_dat: PreEdge1): Option[PreEdge2] =
  {
    val v1 = points.head
    val v2 = points.last

    // TODO it's too hard to deal with these when fusing vertices.
    if (v1 == v2) {
      Util.log("Removing cul-de-sac " + edge_dat.name)
      return None
    }

    // do we already have an edge from v1->v2 or v2->v1?
    // this handling mainly needs to deal with cul-de-sacs
    // TODO figure it out again and make it work, because we certainly seem to
    // hit multi-edges or dupes or something...

    if (edge_lookup.contains((v1, v2, edge_dat.name)) ||
        edge_lookup.contains((v2, v1, edge_dat.name)))
    {
      //Util.log("well, finally! already exists " + edge_dat.name)
      // TODO make a new edge if the points dont match?
      return None
    } else {
      assert(v1 == points.head)
      assert(v2 == points.last)
      val e = new PreEdge2(points, edge_dat)
      //edge_lookup((v1, v2, edge_dat.name)) = e
      return Some(e)
    }
  }
}

// Mutable because fiddling with clones while fusing is confusing.
class PreEdge2(var points: List[Coordinate], val dat: PreEdge1)
{
  def from = points.head
  def to   = points.last

  // TODO re-check every time we change?
  assert(from != to)  // TODO no cul-de-sacs yet

  val length = points.zip(points.tail).map(p => new Line(p._1, p._2)).foldLeft(0.0)(
    (a, b) => a + b.length
  )
}
