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

import utexas.aorta.map.{Road, Edge, Vertex, Turn, Line, Coordinate,
                         Traversable, DirectedRoad}

import utexas.aorta.{Util, Common, cfg}

// TODO split this file up

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

  def run(): PreGraph3 = {
    Common.edges = graph.edges.toArray

    for (r <- graph.roads) {
      roads_per_vert.addBinding(r.v1, r)
      roads_per_vert.addBinding(r.v2, r)

      // pre-compute lines constituting the edges
      // the -0.5 lets there be nice lane lines between lanes
      for (e <- r.pos_lanes) {
        e.set_lines(for ((from, to) <- r.pairs_of_points)
                    yield new Line(from, to).perp_shift(e.lane_offset - 0.5))
      }
      for (e <- r.neg_lanes) {
        val ls = for ((from, to) <- r.pairs_of_points)
                 yield new Line(to, from).perp_shift(e.lane_offset - 0.5)
        // TODO inefficient just because i wanted a two-liner?
        e.set_lines(ls.reverse)
      }

      // force line segments to meet up on the inside
      for (e <- r.all_lanes; (l1, l2) <- e.lines.zip(e.lines.tail)) {
        l1.segment_intersection(l2) match {
          case Some(pt) => {
            l1.x2 = pt.x
            l1.y2 = pt.y
            l2.x1 = pt.x
            l2.y1 = pt.y
          }
          case _ =>
        }
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

    Util.log("Removing disconnected chunks of the network")
    var changed = true
    Util.log_push
    while (changed) {
      // first find edges with no predecessors/successors, remove them, and
      // flood that effect out...
      val change1 = clean_half_edges
      // then finds SCCs and removes all but the largest
      val change2 = clean_disconnected()
      // since each phase potentially affects the other, repeat both until
      // nothing changes
      changed = change1 || change2

      // reset state for tarjan's
      visited.clear
      t_idx.clear
      t_low.clear
      in_stack.clear
      t_stack.clear
      dfs = 0
    }
    Util.log_pop

    Util.log("Collapsing degenerate vertices with 2 roads...")
    collapse_degenerate()

    Util.log("Transforming small roads into longer turns")
    Util.log_push
    // TODO when to run?
    // TODO order of merging these matters, could be weird
    val road_min_len = 50.0  // TODO cfg
    for (r <- graph.roads if r.length < road_min_len) {
      merge_short_road(r)
    }
    Util.log_pop

    Util.log("Tidying up geometry...")
    graph.vertices.foreach(v => adjust_lines(v))

    // Recalculate length. TODO temporary approach.
    graph.traversables.foreach(t => {
      t.lines.foreach(l => l.recompute_length)
      t.set_lines(t.lines)
    })

    return graph
  }

  def next_id(): Int = {
    turn_cnt += 1
    return turn_cnt
  }

  // fill out an intersection with turns
  private def connect_vertex(v: Vertex, roads: MutableSet[Road]) = {
    // TODO return the turns or something so we can more efficiently set them
    // TODO cfg
    val cross_thresshold = math.Pi / 10 // allow 18 degrees

    // if all edges belong to the same road, this is a dead-end
    if (roads.size == 1) {
      // link corresponding lane numbers
      val r = roads.head
      for ((from, to) <- r.incoming_lanes(v).zip(r.outgoing_lanes(v))) {
        v.turns = new Turn(next_id, from.id, to.id) :: v.turns
      }
    }

    // To account for one-ways, we actually want to reason about roads that are
    // incoming to or outgoing from this vert.
    val incoming_roads = roads filter (_.incoming_lanes(v).length != 0)
    val outgoing_roads = roads filter (_.outgoing_lanes(v).length != 0)

    def make_turn(pair: (Edge, Edge)) = new Turn(next_id, pair._1.id, pair._2.id)

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
        // a crossing!
        // essentially zip the from's to the to's, but handle merging:
        // x -> x + n, make the 1 leftmost  lead to the n leftmost
        // x + n -> x, make the n rightmost lead to the 1 rightmost

        // TODO these rules are hard to generalize. when should we have
        // left/right-turn only lanes and stuff?

        val lane_diff = to_edges.length - from_edges.length

        if (lane_diff == 0) {
          // exact 1:1 mapping
          v.turns ++= from_edges.zip(to_edges).map(make_turn)
        } else if (lane_diff < 0) {
          // more to less. the rightmost will all have to merge.
          // we have 'to_edges.length - 1' regular dsts.
          val (mergers, regulars) = from_edges.splitAt(from_edges.length - (to_edges.length - 1))
          Util.assert_eq(regulars.length, to_edges.length - 1)

          v.turns ++= mergers.map(from => make_turn(from, to_edges.head))
          v.turns ++= regulars.zip(to_edges.tail).map(make_turn)
        } else if (lane_diff > 0) {
          // less to more. the leftmost gets to pick many destinations.
          val lucky_src = from_edges.last
          var regular_srcs = from_edges.dropRight(1)

          val (regular_dsts, choices) = to_edges.splitAt(to_edges.size - lane_diff - 1)
          Util.assert_eq(regular_srcs.size, regular_dsts.size)
          
          v.turns ++= regular_srcs.zip(regular_dsts).map(make_turn)
          v.turns ++= choices.map(to => make_turn(lucky_src, to))
        }
      } else if (angle_btwn < 0) {
        // no multiple turn lanes supported yet. it's just too hard to know when
        // this is the case.
        v.turns = make_turn(from_rep.leftmost_lane, to_rep.leftmost_lane) :: v.turns
      } else {
        v.turns = make_turn(from_rep.rightmost_lane, to_rep.rightmost_lane) :: v.turns
      }
    }
  }

  // Returns true if any edges are removed
  private def clean_half_edges(): Boolean = {
    val orig_edges = graph.edges.size
    val orig_verts = graph.vertices.size
    val orig_roads = graph.roads.size

    Util.log("Using fixpoint algorithm to prune half-edges")

    // fixpoint algorithm: find half-edges till there are none
    // TODO flooding would be faster.
    var done = false
    var any_changes = false
    while (!done) {
      graph.edges.partition(e => e.doomed) match {
        case (bad, good) => {
          // TODO cant pattern match nil for mutable list :(
          if (bad.isEmpty) {
            done = true
          } else {
            graph.edges = good
            graph.remove_turns_to_bad_edges(bad.toSet)
            any_changes = true
          }
        }
      }
    }

    if (any_changes) {
      graph.fix_map

      Util.log(
        s"$orig_edges -> ${graph.edges.size} edges, $orig_verts -> " +
        s"${graph.vertices.size} vertices, $orig_roads -> " + 
        s"${graph.roads.size} roads"
      )
    }
    return any_changes
  }

  // Returns true if any edges are removed
  private def clean_disconnected(): Boolean = {
    // use Tarjan's to locate all SCC's in the graph. ideally we'd just
    // have one, but crappy graphs, weird reality, and poor turn heuristics mean
    // we'll have disconnected portions.
    // TODO simplify by flooding DirectedRoads, not turns and lanes
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
      // TODO dont return inside here
      case (biggest :: Nil) => return false // Good, just one SCC
      case (biggest :: doomed_sccs) => {
        for (scc <- doomed_sccs; trav <- scc) {
          trav match {
            case e: Edge => bad_edges += e
            case t: Turn => bad_turns += t
          }
        }
      }
      case Nil => throw new Exception("Tarjan saw empty map!")
    }

    Util.log(
      s"${bad_edges.size} edges, ${bad_turns.size} turns belonging to small SCC"
    )
    val doomed_edges = bad_edges.toSet
    // As a note, all of these steps that completely delete a structure are
    // safe -- anything else referring to them will also be deleted, thanks to
    // Mr. Tarjan.

    // TODO write these at set differences..
    graph.edges = graph.edges.filter(e => !doomed_edges.contains(e))
    val doomed_turns = bad_turns.toSet
    for (v <- graph.vertices) {
      v.turns = v.turns.filter(t => !doomed_turns.contains(t))
    }

    graph.fix_map
    return true
  }

  private def collapse_degenerate(): Unit = {
    // TODO nowhere near ready yet
    return

    // Degenerate vertices have exactly 2 roads, with matching numbers of lanes
    // and with one straight turn connecting each.
    def compatible_vert(v: Vertex) = v.roads.toList match {
      case r1 :: r2 :: Nil =>
        ((r1.pos_lanes.size == r2.pos_lanes.size) &&
         (r1.neg_lanes.size == r2.neg_lanes.size) &&
         (!r1.all_lanes.find(e => e.next_turns.size != 1).isDefined) &&
         (!r2.all_lanes.find(e => e.next_turns.size != 1).isDefined)
        )
      case _ => false
    }

    var changed = true
    while (changed) {
      graph.vertices.find(compatible_vert) match {
        case Some(v) => {
          val Array(r1, r2) = v.roads.toArray
          // Arbitrarily decide to keep and extend r1, and nix r2.
          
        }
        case None => changed = false
      }
    }
  }

  private def adjust_lines(v: Vertex) = {
    val shortest_line = new HashMap[Edge, Line]
    for (in <- v.in_edges) {
      for (out <- v.out_edges) {
        // Correct for shifting the UI does by preventing intersections when
        // shifted over. Has a side effect of making cars stop a bit far back,
        // which is fine.
        val l1 = in.lines.last.perp_shift(0.5)
        val l2 = out.lines.head.perp_shift(0.5)

        // Just to be safe, don't allow ourselves to ever extend a line
        if (!shortest_line.contains(in)) {
          shortest_line(in) = l1
        }
        if (!shortest_line.contains(out)) {
          shortest_line(out) = l2
        }

        l1.segment_intersection(l2) match {
          case Some(pt) => {
            val possible1 = new Line(l1.x1, l1.y1, pt.x, pt.y)
            val possible2 = new Line(pt.x, pt.y, l2.x2, l2.y2)

            // This line will intersect many -- store the one that gets
            // trimmed the most.
            if (!shortest_line.contains(in) || possible1.length < shortest_line(in).length) {
              shortest_line(in) = possible1
            }
            if (!shortest_line.contains(out) || possible2.length < shortest_line(out).length) {
              shortest_line(out) = possible2
            }
          }
          case _ =>
        }
      }
    }

    // Go back and mod the line to its shortest length.
    for ((e, best_line) <- shortest_line) {
      val l = if (e.from == v)
                e.lines.head
              else
                e.lines.last
      val use = best_line.perp_shift(-0.5)
      l.x1 = use.x1
      l.y1 = use.y1
      l.x2 = use.x2
      l.y2 = use.y2
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

    while (work.nonEmpty) {
      val (t, backref, next_steps) = work.pop

      if (next_steps.nonEmpty) {
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

  def merge_short_road(r: Road): Unit = {
    // TODO needs more tuning
    return

    // TODO this spot is hokey.
    if (r.name == "Bernwood Drive") {
      return
    }

    /*Util.log(s"$r is short, removing...")
    // Find all turns leading to each edge of this bad road
    for (e <- r.all_lanes) {
      Util.log("  gonna end up doing " + (e.prev_turns.size * e.next_turns.size) +
               " new turns for " + e)
      for (orig_turn <- e.prev_turns) {
        // Extend that turn to cover this road
        e.from.turns = e.from.turns.filter(t => t != orig_turn)
        for (tail_turn <- e.next_turns) {
          // TODO turn ids dont seem to need to be contiguous.
          e.from.turns = Turn(
            next_id, orig_turn.from, tail_turn.to,
            // TODO i dont even think this is appropriate anymore
            orig_turn.length + tail_turn.length
          ) :: e.from.turns
        }
      }
    }
    // Remove the road and the edges.
    // TODO ever nix the vertices?
    graph.roads = graph.roads.filter(road => road != r)
    graph.edges = graph.edges.filter(e => e.road != r)
    graph.fix_ids*/
  }
}
