package map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer

import map.{Road, Edge, Vertex, Turn, TurnType, Line, Coordinate}

class Pass3(old_graph: PreGraph2) {
  println("Multiplying and directing " + old_graph.edges.length + " edges")
  val graph = new PreGraph3(old_graph)
  val roads_per_vert = new HashMap[Vertex, MutableSet[Road]] with MultiMap[Vertex, Road]
  // for tarjan's
  val visited = new HashSet[Vertex]
  // TODO or operate on a wrapper structure.
  val v_idx = new HashMap[Vertex, Int]
  val v_low = new HashMap[Vertex, Int]
  val v_stack = new Stack[Vertex]
  var dfs = 0   // counter numbering

  def run(show_dead: Boolean): PreGraph3 = {
    // TODO two loops (make edge, process it). meh?
    for (r <- graph.roads) {
      roads_per_vert.addBinding(r.v1, r)
      roads_per_vert.addBinding(r.v2, r)

      // pre-compute lines constituting the edges
      for (e <- r.pos_lanes) {
        e.lines = for ((from, to) <- r.pairs_of_points)
                  yield shift_line(e.lane_offset, from, to)
      }
      for (e <- r.neg_lanes) {
        e.lines = for ((from, to) <- r.pairs_of_points)
                  yield shift_line(e.lane_offset, to, from)
        // TODO inefficient just because i wanted a two-liner?
        e.lines = e.lines.reverse
      }
    }

    println("Connecting the dots...")
    // TODO return the mapping in the future?
    for (v <- graph.vertices) {
      // TODO not sure why this is the case yet
      if (roads_per_vert.contains(v)) {
        connect_vertex(v, roads_per_vert(v))
      } else {
        //println("nothing refs vert " + v)
      }
    }

    // TODO adjust terminal line segments somehow
    // look at the vertex an edge hits, then find the perpendicular(ish) road
    // and trim everything back. but everything? not quite, cause you could have
    // a |--- situation.

    // finally, use Tarjan's to locate all SCC's in the graph. ideally we'd just
    // have one, but crappy graphs, weird reality, and poor turn heuristics mean
    // we'll have disconnected portions.
    var sccs = new ListBuffer[List[Vertex]]

    for (v <- graph.vertices) {
      if (!visited(v)) {
        sccs ++= tarjan(v);
      }
    }

    // TODO take a command line option
    // deal with all edges of all but the largest SCC by either
    // 1) coloring them so we can manually inspect
    // 2) or removing the associated roads, vertices, edges

    if (show_dead) {
      // this sort is ascending, so drop the highest scc -- it's the valid map.
      for (scc <- sccs.sortBy(scc => scc.size).dropRight(1)) {
        // for now, by coloring it with an angry road type
        for (vert <- scc; road <- vert.roads) {
          road.road_type = "doomed"
        }
      }
      println("Doomed " + (sccs.size - 1) + " disconnected SCC's from the graph")
    } else {
      // This is a cheap trick, and it absolutely works.
      val doomed_verts = HashSet() ++ sccs.sortBy(scc => scc.size).dropRight(1).flatten
      val doomed_roads = HashSet() ++ doomed_verts.flatMap(v => v.roads)
      val doomed_edges = HashSet() ++ doomed_roads.flatMap(r => r.all_lanes)
      println("Removing " + doomed_verts.size + " disconnected vertices, "
              + doomed_roads.size + " roads, and "
              + doomed_edges.size + " edges from graph\n")
      // Yes, it really is this easy (and evil)
      graph.vertices = graph.vertices.filter(v => !doomed_verts(v))
      graph.edges = graph.edges.filter(e => !doomed_edges(e))
      graph.roads = graph.roads.filter(r => !doomed_roads(r))
      // Although we do have to clean up IDs
      // TODO a better way, and one without reassigning to 'val' id?
      var id = 0
      for (v <- graph.vertices) {
        v.id = id
        id += 1
      }
      id = 0
      for (r <- graph.roads) {
        r.id = id
        id += 1
      }
      id = 0
      for (e <- graph.edges) {
        e.id = id
        id += 1
      }
    }

    return graph
  }

  // fill out an intersection with turns
  private def connect_vertex(v: Vertex, roads: MutableSet[Road]) = {
    // TODO cfg
    val cross_thresshold = math.Pi / 10 // allow 18 degrees

    // if all edges belong to the same road, this is a dead-end
    if (roads.size == 1) {
      // link corresponding lane numbers
      val r = roads.toList(0)  // TODO ugly way to get the only road?
      for ((from, to) <- r.incoming_lanes(v) zip r.outgoing_lanes(v)) {
        v.turns += new Turn(from, TurnType.UTURN, to)
      }
    }

    // To account for one-ways, we actually want to reason about roads that are
    // incoming to or outgoing from this vert.
    val incoming_roads = roads filter (_.incoming_lanes(v).length != 0)
    val outgoing_roads = roads filter (_.outgoing_lanes(v).length != 0)

    // TODO do we need equality on roads? go by id.
    // this is a Cartesian product.
    for (r1 <- incoming_roads; r2 <- outgoing_roads if r1 != r2) {
      val from_edges = r1.incoming_lanes(v)  
      val to_edges = r2.outgoing_lanes(v)  

      // we want the angle to go from a 'from' edge to a 'to' edge
      val from_angle = from_edges.head.last_line.angle
      val to_angle = to_edges.head.first_line.angle

      // smallest angle of rotation, from "Agony" on gamedev TODO cite
      val angle_btwn = ((from_angle - to_angle + 3 * (math.Pi)) % (2 * math.Pi)) - math.Pi

      if (r1.osm_id == r2.osm_id || math.abs(angle_btwn) <= cross_thresshold) {
        // a crossing!
        // essentially zip the from's to the to's, but handle merging:
        // x -> x + n, make the n leftmost lead to the leftmost
        // x + n -> x, make the n rightmost lead to the rightmost

        val lane_diff = to_edges.length - from_edges.length

        // TODO doesnt work :(
        //def cross_turn(from: Edge, to: Edge) = new Turn(from, TurnType.CROSS, to)

        // TODO combo logic somehow? :P
        // TODO also, i'm sure theres off-by-ones here.
        // TODO scalaisms.
        if (lane_diff == 0) {
          for ((from, to) <- from_edges zip to_edges) {
            v.turns += new Turn(from, TurnType.CROSS, to)
          }
          // TODO doesnt work
          //v.turns += (from_edges zip to_edges) map cross_turn
        } else if (lane_diff < 0) {
          // more to less. the leftmost destination gets many sources.
          val (mergers, regulars) = from_edges splitAt (from_edges.length + lane_diff)
          for (from <- mergers) {
            v.turns += new Turn(from, TurnType.CROSS_MERGE, to_edges.last)
          }
          for ((from, to) <- regulars zip to_edges.tail) {
            v.turns += new Turn(from, TurnType.CROSS, to)
          }

          //v.turns += mergers map new Turn(_, TurnType.CROSS_MERGE, to_edges.last)
          //v.turns += (regulars zip to_edges.tail) map turn_factory(TurnType.CROSS)
        } else if (lane_diff > 0) {
          // less to more. the rightmost gets to pick many destinations.
          val (many_sourced, regulars) = to_edges splitAt lane_diff
          for (to <- many_sourced) {
            v.turns += new Turn(from_edges.head, TurnType.CROSS, to)
          }

          //v.turns += many_sourced map new Turn(from_edges.head, TurnType.CROSS, _)
          //v.turns += (from_edges.tail zip regulars) map turn_factory(TurnType.CROSS)
        }
      } else if (angle_btwn < 0) {
        // leftmost = highest lane-num
        v.turns += new Turn(from_edges.last, TurnType.LEFT, to_edges.last)
      } else {
        // rightmost = lowest lane-num
        v.turns += new Turn(from_edges.head, TurnType.RIGHT, to_edges.head)
      }
    }

    // here's how to diagnose SCC's.
    for (in <- incoming_roads; src <- in.incoming_lanes(v)
         if v.turns_from(src).length == 0)
    {
      println("GRR: nowhere to go after " + src)
    }
    for (out <- outgoing_roads; dst <- out.outgoing_lanes(v)
         if v.turns_to(dst).length == 0)
    {
      println("GRR: nothing leads to " + dst)
    }
  }

  private def shift_line(l: Int, pt1: Coordinate, pt2: Coordinate): Line = {
    // This used to be much more complex, generating two lines and seeing which
    // one was 'less' wrong from a point we projected. Apparently it's MUCH
    // simpler than that.

    val width = 0.5   // TODO maplane-width cfg

    // just move in the direction of the road (as given by the ordering of the
    // points) plus 90 degrees clockwise
    val road_line = new Line(pt1.x, pt1.y, pt2.x, pt2.y)
    // TODO why does the angle() that respects inversion fail?
    val theta = road_line.broken_angle + (math.Pi / 2)
    val dx = l * width * math.cos(theta)
    val dy = l * width * math.sin(theta)

    return new Line(pt1.x + dx, pt1.y + dy, pt2.x + dx, pt2.y + dy)
  }

  // flood from v
  private def tarjan(v: Vertex): ListBuffer[List[Vertex]] = {
    visited += v
    v_idx(v) = dfs
    v_low(v) = dfs
    dfs += 1
    v_stack.push(v)

    var sccs = new ListBuffer[List[Vertex]]

    // what vertices can we reach from here?
    for (next <- v.out_verts) {
      if (!visited(next)) {
        // TODO the recursion is expensive. can we rewrite iteratively, or make
        // it tail recursion? (tail seems REALLY unlikely)
        sccs ++= tarjan(next)
        v_low(v) = math.min(v_low(v), v_low(next))
      } else if (v_stack.contains(next)) {
        // here's a back-edge
        v_low(v) = math.min(v_low(v), v_idx(next))
      }
    }

    // are we a 'root'?
    if (v_low(v) == v_idx(v)) {
      // pop stack and stop when we hit v
      // these all make an scc
      var member : Vertex = null
      // TODO a functional way? :P
      var cnt = 0
      var scc = new ListBuffer[Vertex]
      do {
        member = v_stack.pop
        scc += member
      } while (v != member)
      // TODO it'd be awesome to keep the list in sorted order as we build it
      sccs += scc.toList
    }

    return sccs
  }
}
