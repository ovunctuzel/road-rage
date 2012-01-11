package utexas.map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer

import utexas.map.{Road, Edge, Vertex, Turn, TurnType, Line, Coordinate, Ward}

import utexas.Util

class Pass3(old_graph: PreGraph2) {
  Util.log("Multiplying and directing " + old_graph.edges.length + " edges")
  val graph = new PreGraph3(old_graph)
  val roads_per_vert = new HashMap[Vertex, MutableSet[Road]] with MultiMap[Vertex, Road]
  // for tarjan's
  val visited = new HashSet[Vertex]
  // TODO or operate on a wrapper structure.
  val v_idx = new HashMap[Vertex, Int]
  val v_low = new HashMap[Vertex, Int]
  val v_stack = new Stack[Vertex]
  var dfs = 0   // counter numbering
  var turn_cnt = -1

  def run(show_dead: Boolean): PreGraph3 = {
    for (r <- graph.roads) {
      roads_per_vert.addBinding(r.v1, r)
      roads_per_vert.addBinding(r.v2, r)

      // pre-compute lines constituting the edges
      if (r.is_oneway) {
        val num = r.oneway_lanes.size
        // These always make the oneway lanes center around the road's center
        val offsets = if (num % 2 == 0)
                        // even
                        (-0.5 - ((num / 2) - 1)) until (0.5 + (num / 2)) by 1.0
                      else
                        // odd
                        -(num / 2).toDouble until ((num / 2) + 1).toDouble by 1.0

        // reverse because the most positive shift should be the rightmost lane
        for ((e, offset) <- r.oneway_lanes.zip(offsets.reverse)) {
          e.lines = for ((from, to) <- r.pairs_of_points)
                    yield shift_line(offset, from, to)
        }
      } else {
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

      // force line segments to meet up on the inside
      for (e <- r.all_lanes; (l1, l2) <- e.lines zip e.lines.tail) {
        adjust_lines(l1, l2, true)
      }
    }

    Util.log("Connecting the dots...")
    Util.log_push
    // TODO return the mapping in the future?
    for (v <- graph.vertices) {
      if (roads_per_vert.contains(v)) {
        connect_vertex(v, roads_per_vert(v))
      } else {
        // TODO not sure I see this happen ever
        Util.log("WARNING nothing refs vert " + v)
      }
    }
    Util.log_pop

    // use Tarjan's to locate all SCC's in the graph. ideally we'd just
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
      Util.log("Doomed " + (sccs.size - 1) + " disconnected SCC's from the graph")
    } else {
      // This is a cheap trick, and it absolutely works.
      val doomed_verts = HashSet() ++ sccs.sortBy(scc => scc.size).dropRight(1).flatten
      val doomed_roads = HashSet() ++ doomed_verts.flatMap(v => v.roads)
      val doomed_edges = HashSet() ++ doomed_roads.flatMap(r => r.all_lanes)
      Util.log("Removing " + doomed_verts.size + " disconnected vertices, "
          + doomed_roads.size + " roads, and "
          + doomed_edges.size + " edges from graph")
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

    Util.log("Tidying up geometry...")
    // Do this work per edge, for now.
    graph.edges.foreach(e => adjust_segments(e))

    Util.log("Dividing the map into wards...")
    Util.log_push
    // Mike has "super-edges" in between wards, and Dustin has the "highway". All
    // roads belong to this special ward.
    //val (wards, special_ward) = Ward.construct_mikes_wards(graph)
    val (wards, special_ward) = Ward.construct_dustins_wards(graph)
    graph.wards = wards
    graph.special_ward = special_ward
    // Tell road about their ward
    for (w <- special_ward :: wards) {
      w.roads.foreach(r => r.ward = w)
    }
    Util.log("The map has " + (wards.size + 1) + " wards")
    Util.log_pop

    return graph
  }

  def next_id(): Int = {
    turn_cnt += 1
    return turn_cnt
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
        v.turns += new Turn(next_id, from, TurnType.UTURN, to)
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

      // choose arbitrary representatives so we can make queries
      val from = from_edges.head
      val to   = to_edges.head

      // we want the angle to go from any 'from' edge to any 'to' edge
      val from_angle = from_edges.head.last_road_line.angle
      val to_angle = to_edges.head.first_road_line.angle

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
            v.turns += new Turn(next_id, from, TurnType.CROSS, to)
          }
          // TODO doesnt work
          //v.turns += (from_edges zip to_edges) map cross_turn
        } else if (lane_diff < 0) {
          // more to less. the leftmost destination gets many sources.
          val (mergers, regulars) = from_edges splitAt (from_edges.length + lane_diff)
          for (from <- mergers) {
            v.turns += new Turn(next_id, from, TurnType.CROSS_MERGE, to_edges.last)
          }
          for ((from, to) <- regulars zip to_edges.tail) {
            v.turns += new Turn(next_id, from, TurnType.CROSS, to)
          }

          //v.turns += mergers map new Turn(_, TurnType.CROSS_MERGE, to_edges.last)
          //v.turns += (regulars zip to_edges.tail) map turn_factory(TurnType.CROSS)
        } else if (lane_diff > 0) {
          // less to more. the rightmost gets to pick many destinations.
          val (many_sourced, regulars) = to_edges splitAt lane_diff
          for (to <- many_sourced) {
            v.turns += new Turn(next_id, from_edges.head, TurnType.CROSS, to)
          }

          //v.turns += many_sourced map new Turn(from_edges.head, TurnType.CROSS, _)
          //v.turns += (from_edges.tail zip regulars) map turn_factory(TurnType.CROSS)
        }
      } else if (angle_btwn < 0) {
        v.turns += new Turn(next_id, from.leftmost_lane, TurnType.LEFT, to.leftmost_lane)
      } else {
        v.turns += new Turn(next_id, from.rightmost_lane, TurnType.RIGHT, to.rightmost_lane)
      }
    }

    // here's how to diagnose SCC's.
    for (in <- incoming_roads; src <- in.incoming_lanes(v)
         if v.turns_from(src).length == 0)
    {
      Util.log("GRR: nowhere to go after " + src)
    }
    for (out <- outgoing_roads; dst <- out.outgoing_lanes(v)
         if v.turns_to(dst).length == 0)
    {
      Util.log("GRR: nothing leads to " + dst)
    }
  }

  private def shift_line(off: Double, pt1: Coordinate, pt2: Coordinate): Line = {
    // This used to be much more complex, generating two lines and seeing which
    // one was 'less' wrong from a point we projected. Apparently it's MUCH
    // simpler than that.

    val width = 0.5   // TODO maplane-width cfg

    // just move in the direction of the road (as given by the ordering of the
    // points) plus 90 degrees clockwise
    val road_line = new Line(pt1.x, pt1.y, pt2.x, pt2.y)
    // TODO why does the angle() that respects inversion fail?
    val theta = road_line.broken_angle + (math.Pi / 2)
    val dx = off * width * math.cos(theta)
    val dy = off * width * math.sin(theta)

    return new Line(pt1.x + dx, pt1.y + dy, pt2.x + dx, pt2.y + dy)
  }

  private def adjust_segments(e: Edge) = {
    // When we shift lines from the road's center to draw lanes, we end up with
    // lines that protrude too far into the intersection. It's not clear how far
    // back to trim these in general, so one solution is to find all the other
    // line segments of adjacent edges, determine intersection points, and trim
    // both lines back to that point.
    // TODO investigate a general "scale length by 10%" policy
    // TODO multiple 'right roads' are untested? :P

    // We want our rightmost lane to match whatever is counter-clockwise to it. 
    // TODO this is flawed in the presence of oneways:
    // ^
    // |
    // V <======  the ccw lane relevant is incoming, not outgoing
    // ^
    // |

    // next_counterclockwise_to returns parallel lanes, but we actually want the
    // edge that's going to cross us, so that's why we force rightmost lane
    // Not adjusting the other line if we're not the rightmost lane... TODO does
    // this help?
    e.rightmost_lane.next_counterclockwise_to match {
      case Some(ccw) => adjust_lines(e.lines.last, ccw.lines.head, e.is_rightmost)
      case _ => {}
    }
  }

  private def adjust_lines(l1: Line, l2: Line, both: Boolean) = {
    // expect them to collide.
    l1.intersection(l2) match {
      case Some(pt) => {
        // With multiple lanes, this could happen multiple times. Either force
        // far-to-near order so lines only ever decrease, or just only permit
        // changes when it makes lines shorter...
        if ((new Line(l1.x1, l1.y1, pt.x, pt.y)).length < l1.length) {
          l1.x2 = pt.x
          l1.y2 = pt.y
        }
        if (both) {
          if ((new Line(pt.x, pt.y, l2.x2, l2.y2)).length < l2.length) {
            l2.x1 = pt.x
            l2.y1 = pt.y
          }
        }
      }
      case None => {} // don't worry about parallel lines
    }
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
