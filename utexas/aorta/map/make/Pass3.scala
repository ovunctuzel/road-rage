// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map.make

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import scala.collection.mutable.MultiMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList

import utexas.aorta.map.{Road, Edge, Vertex, Turn, TurnType, Line, Coordinate, Ward,
                   Traversable}

import utexas.aorta.{Util, cfg}

class Pass3(old_graph: PreGraph2) {
  Util.log("Multiplying and directing " + old_graph.edges.length + " edges")
  val graph = new PreGraph3(old_graph)
  val roads_per_vert = new HashMap[Vertex, MutableSet[Road]] with MultiMap[Vertex, Road]
  var turn_cnt = -1

  // for tarjan's
  val visited = new HashSet[Traversable]
  val t_idx = new HashMap[Traversable, Int]
  val t_low = new HashMap[Traversable, Int]
  val in_stack = new HashSet[Traversable]  // yay linear-time Tarjan's
  val t_stack = new Stack[Traversable]
  var dfs = 0   // counter numbering

  // for intersection grouping
  val seen = new MutableHashSet[Vertex]()

  def run(show_dead: Boolean): PreGraph3 = {
    for (r <- graph.roads) {
      roads_per_vert.addBinding(r.v1, r)
      roads_per_vert.addBinding(r.v2, r)

      // pre-compute lines constituting the edges
      // the -0.5 lets there be nice lane lines between lanes
      for (e <- r.pos_lanes) {
        e.set_lines(for ((from, to) <- r.pairs_of_points)
                    yield new Line(from, to).shift_line(e.lane_offset - 0.5))
      }
      for (e <- r.neg_lanes) {
        val ls = for ((from, to) <- r.pairs_of_points)
                 yield new Line(to, from).shift_line(e.lane_offset - 0.5)
        // TODO inefficient just because i wanted a two-liner?
        e.set_lines(ls.reverse)
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
      assert(roads_per_vert.contains(v))
      connect_vertex(v, roads_per_vert(v))
    }
    Util.log_pop

    // use Tarjan's to locate all SCC's in the graph. ideally we'd just
    // have one, but crappy graphs, weird reality, and poor turn heuristics mean
    // we'll have disconnected portions.
    val sccs = new ListBuffer[List[Traversable]]

    for (t <- graph.traversables) {
      if (!visited(t)) {
        tarjan_body(t, sccs)
      }
    }

    // deal with all edges of all but the largest SCC by either
    // 1) coloring them so we can manually inspect
    // 2) or removing the associated roads, vertices, edges

    // Collect all the bad edges and turns.
    val bad_edges = new ListBuffer[Edge]
    val bad_turns = new ListBuffer[Turn]

    sccs.sortBy(scc => scc.size).toList.reverse match {
      case (biggest :: doomed_sccs) => {
        for (scc <- doomed_sccs; trav <- scc) {
          trav match {
            case e: Edge => bad_edges += e
            case t: Turn => bad_turns += t
          }
        }
      }
      case Nil => Util.log("Map was empty?!") // shouldn't happen
    }

    Util.log(bad_edges.size + " bad edges, " + bad_turns.size + " bad turns")
    val doomed_edges = bad_edges.toSet

    if (show_dead) {
      // Mark all roads involving bad edges.
      for (r <- graph.roads) {
        if (r.all_lanes.find(l => doomed_edges(l)).isDefined) {
          r.road_type = "doomed"
        }
      }
      
      // And also mark exactly the bad edges.
      doomed_edges.foreach(e => e.doomed = true)
    } else {
      // As a note, all of these steps that completely delete a structure are
      // safe -- anything else referring to them will also be deleted, thanks to
      // Mr. Tarjan.

      // Deal with bad edges by finding roads only containing bad edges (doom
      // them too), and filter out the bad edges from the rest.
      val keep_roads = new ListBuffer[Road]

      for (r <- graph.roads) {
        val pos_lanes = r.pos_lanes.filter(l => !doomed_edges(l))
        val neg_lanes = r.neg_lanes.filter(l => !doomed_edges(l))

        // We can completely nix this road otherwise
        if (pos_lanes.size != 0 || neg_lanes.size != 0) {
          // This will end up looking weird (gaps in roads), but just get rid of
          // the bad lanes. Gotta clean up lane numbers right now, IDs later.
          r.pos_lanes.clear
          r.pos_lanes ++= pos_lanes
          for ((lane, idx) <- r.pos_lanes.zipWithIndex) {
            lane.lane_num = idx
          }
          r.neg_lanes.clear
          r.neg_lanes ++= neg_lanes
          for ((lane, idx) <- r.neg_lanes.zipWithIndex) {
            lane.lane_num = idx
          }

          keep_roads += r
        }
      }

      // Now clean up each vertex similarly by filtering out bad turns and
      // possibly nixing the vertex completely
      val doomed_turns = bad_turns.toSet
      val keep_verts = new ListBuffer[Vertex]
      for (v <- graph.vertices) {
        val turns = v.turns.filter(t => !doomed_turns(t))
        // If there are no turns left, nix this vertex completely
        if (turns.size != 0) {
          v.turns.clear
          v.turns ++= turns
          keep_verts += v
        }
      }

      // Now get rid of all the bad stuff
      Util.log((graph.roads.size - keep_roads.size) + " roads and " +
               (graph.vertices.size - keep_verts.size) + " vertices were bad too")

      graph.roads.clear
      graph.roads ++= keep_roads
      graph.edges = graph.edges.filter(e => !doomed_edges(e))
      graph.vertices.clear
      graph.vertices ++= keep_verts

      // clean up ids of all edges, roads, verts.
      // TODO a better way, and one without reassigning to 'val' id? if we just
      // clone each structure and have a mapping from old to new... worth it?
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

    // Discover groups of nearby intersections (with abysmally short roads
    // between them)
    for (v <- graph.vertices) {
      if (!seen.contains(v)) {
        val clump = flood_clump(v)
        if (!clump.isEmpty) {
          //Util.log("clump of verts: " + clump)
        }
      }
    }

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
      val r = roads.head
      for ((from, to) <- r.incoming_lanes(v) zip r.outgoing_lanes(v)) {
        v.turns += new Turn(next_id, from, TurnType.UTURN, to)
      }
    }

    // To account for one-ways, we actually want to reason about roads that are
    // incoming to or outgoing from this vert.
    val incoming_roads = roads filter (_.incoming_lanes(v).length != 0)
    val outgoing_roads = roads filter (_.outgoing_lanes(v).length != 0)

    // this is a Cartesian product.
    for (r1 <- incoming_roads; r2 <- outgoing_roads if r1 != r2) {
      val from_edges = r1.incoming_lanes(v)
      val to_edges = r2.outgoing_lanes(v)

      // choose arbitrary representatives so we can make queries
      val from_rep = from_edges.head
      val to_rep   = to_edges.head

      // we want the angle to go from any 'from' edge to any 'to' edge
      val from_angle = from_rep.last_road_line.angle
      val to_angle = to_rep.first_road_line.angle

      // smallest angle of rotation, from "Agony" on gamedev TODO cite
      val angle_btwn = ((from_angle - to_angle + 3 * (math.Pi)) % (2 * math.Pi)) - math.Pi

      if (r1.osm_id == r2.osm_id || math.abs(angle_btwn) <= cross_thresshold) {
        // TODO possibly worry about when just osm id matches and angle is
        // huge... is it really a cross?
        
        // a crossing!
        // essentially zip the from's to the to's, but handle merging:
        // x -> x + n, make the 1 leftmost  lead to the n leftmost
        // x + n -> x, make the n rightmost lead to the 1 rightmost

        // TODO these rules are hard to generalize. when should we have
        // left/right-turn only lanes and stuff?

        def cross_turn(pair: (Edge, Edge)) = new Turn(next_id, pair._1, TurnType.CROSS, pair._2)
        val lane_diff = to_edges.length - from_edges.length

        if (lane_diff == 0) {
          // exact 1:1 mapping
          v.turns ++= from_edges.zip(to_edges).map(cross_turn)
        } else if (lane_diff < 0) {
          // more to less. the rightmost will all have to merge.
          // we have 'to_edges.length - 1' regular dsts.
          val (mergers, regulars) = from_edges.splitAt(from_edges.length - (to_edges.length - 1))
          assert(regulars.length == to_edges.length - 1)

          v.turns ++= mergers.map(from => new Turn(next_id, from, TurnType.CROSS_MERGE, to_edges.head))
          v.turns ++= regulars.zip(to_edges.tail).map(cross_turn)
        } else if (lane_diff > 0) {
          // less to more. the leftmost gets to pick many destinations.
          val lucky_src = from_edges.last
          var regular_srcs = from_edges.dropRight(1)

          val (regular_dsts, choices) = to_edges.splitAt(to_edges.size - lane_diff - 1)
          assert(regular_srcs.size == regular_dsts.size)
          
          v.turns ++= regular_srcs.zip(regular_dsts).map(cross_turn)
          v.turns ++= choices.map(to => new Turn(next_id, lucky_src, TurnType.CROSS, to))
        }
      } else if (angle_btwn < 0) {
        // no multiple turn lanes supported yet. it's just too hard to know when
        // this is the case.
        v.turns += new Turn(next_id, from_rep.leftmost_lane, TurnType.LEFT, to_rep.leftmost_lane)
      } else {
        v.turns += new Turn(next_id, from_rep.rightmost_lane, TurnType.RIGHT, to_rep.rightmost_lane)
      }
    }

    // here's how to diagnose SCC's.
    for (in <- incoming_roads; src <- in.incoming_lanes(v)
         if v.turns_from(src).length == 0)
    {
      Util.log("Warning: nowhere to go after " + src)
    }
    for (out <- outgoing_roads; dst <- out.outgoing_lanes(v)
         if v.turns_to(dst).length == 0)
    {
      Util.log("Warning: nothing leads to " + dst)
    }
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

        // But some changes, for some reason, make some lines MUCH shorter, so
        // detect and avoid trimming those.
        val max_trim = 50.0  // TODO cfg. this should be meters now.

        val possible1 = new Line(l1.x1, l1.y1, pt.x, pt.y)
        if (possible1.length < l1.length && l1.length - possible1.length <= max_trim) {
          l1.x2 = pt.x
          l1.y2 = pt.y
        }
        if (both) {
          val possible2 = new Line(pt.x, pt.y, l2.x2, l2.y2)
          if (possible2.length < l2.length && l2.length - possible2.length <= max_trim) {
            l2.x1 = pt.x
            l2.y1 = pt.y
          }
        }
      }
      case None => {} // don't worry about parallel lines
    }
  }

  // call at the beginning of the recursion to do work on 't' just once. returns
  // the list of traversables to look at next.
  private def tarjan_head(t: Traversable): List[Traversable] = {
    visited += t
    t_idx(t) = dfs
    t_low(t) = dfs
    dfs += 1
    t_stack.push(t)
    in_stack += t

    return t.leads_to
  }

  private def tarjan_body(orig_trav: Traversable, sccs: ListBuffer[List[Traversable]]): Unit =
  {
    // tuple is ('t', our trav 'backref' so we can do t_low, then list of
    // connected travss left to process)
    val work = new Stack[(Traversable, Traversable, List[Traversable])]

    // seed with original work
    work.push((orig_trav, null, tarjan_head(orig_trav)))

    while (!work.isEmpty) {
      val (t, backref, next_steps) = work.pop

      if (!next_steps.isEmpty) {
        val next = next_steps.head

        // either way, there's more work to do -- push it on BEFORE other work
        // we might add.
        work.push((t, backref, next_steps.tail))

        if (!visited(next)) {
          // here's where we "recurse"
          work.push((next, t, tarjan_head(next)))
        } else if (in_stack(next)) {
          // here's a back-edge. keeping this map above is the trick that makes
          // Tarjan's O(n)
          t_low(t) = math.min(t_low(t), t_idx(next))
        }

      } else {
        // done with processing all the vert's connections...

        // are we a 'root'?
        if (t_low(t) == t_idx(t)) {
          // pop stack and stop when we hit t
          // these all make an scc
          var member: Traversable = null
          // TODO a functional way? :P
          var cnt = 0
          var scc = new ListBuffer[Traversable]
          do {
            member = t_stack.pop
            in_stack -= member
            scc += member
          } while (t != member)
          // TODO it'd be awesome to keep the list in sorted order as we build it
          sccs += scc.toList
        }

        // this is normally where we'd return and pop out of the recursion, so
        // do that work here...

        // should only be null for orig_trav
        if (backref != null) {
          t_low(backref) = math.min(t_low(backref), t_low(t))
        }
      }
    }
  }

  val max_len = 50.0 // meters. TODO cfg

  def flood_clump(from: Vertex): Set[Vertex] = {
    val this_clump = new MutableHashSet[Vertex]()
    var queue: List[Vertex] = List(from)

    // a DFS to find intersections clumped together
    while (!queue.isEmpty) {
      val cur = queue.head
      queue = queue.tail
      seen += cur
      this_clump += cur
      for (r <- cur.roads if r.length <= max_len) {
        val next = r.other_vert(cur)
        if (!seen.contains(next)) {
          queue = next :: queue
        }
      }
    }
    return if (this_clump.size > 1)
             this_clump.toSet
           else
             Nil.toSet
  }
}
