// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.map

// TODO combo these lines!
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import scala.collection.mutable.MutableList
import scala.collection.mutable.{Stack => MutableStack}
import scala.collection.mutable.{HashMap => MutableMap}

import utexas.aorta.map.make.PreGraph3 // TODO we can actually operate just as easily on a grap...

import utexas.aorta.Util

// note that ID does NOT give the ward list that graph has any ordering!
// TODO var id so we can get deterministic ordering ;)
class Ward(var id: Int, val roads: Set[Road]) {
  override def toString = "Ward with " + roads.size + " roads"

  private def find_center(): Coordinate = {
    val verts = roads.flatMap(r => List(r.v1.location, r.v2.location)).toSet
    val sum = verts.foldLeft(new Coordinate(0, 0))((a, b) => a + b)
    return new Coordinate(sum.x / verts.size, sum.y / verts.size)
  }

  // find the roads that lead out of the ward, and return those vertices
  private def find_frontier(): Set[Vertex] = {
    val set = new MutableHashSet[Vertex]
    for (v <- roads.flatMap(r => List(r.v1, r.v2))) {
      if (v.roads.filter(r => !roads(r)).size != 0) {
        set += v
      }
    }
    return set.toSet
  }

  val center = find_center

  // The vertices that lead to the ward
  val frontier = find_frontier
}

object Ward {
  private var id_counter = -1
  def next_id:Int = {
    id_counter += 1
    return id_counter
  }

  // Mike's idea: flood out from vertices as long as there's a small distance
  // along the edges. This finds vertices that are clustered "densely." Roads
  // completely inside one of these floods constitute a Ward, and those along
  // the border form "super-roads" that link Wards.
  def construct_mikes_wards(g: PreGraph3): (List[Ward], Ward) = {
    val max_dist = 200  // between intersections, in meters

    // This assigns wards to vertices. We'll map that to roads later.
    val vert_wards = new MutableList[MutableSet[Vertex]]()

    // these are waiting to go in any ward
    val pending_verts = new MutableHashSet[Vertex]()
    pending_verts ++= g.vertices
    while (pending_verts.nonEmpty) {
      // make a new ward...
      val cur_ward = new MutableHashSet[Vertex]
      val evaluating = new MutableStack[Vertex]
      val start_with = pending_verts.head
      evaluating.push(start_with)
      cur_ward += start_with

      // add all vertices a fixed distance away from the one we're evaluating,
      // meaning this goes by density and not total distance
      while (evaluating.nonEmpty) {
        val from_v = evaluating.pop
        for (e <- from_v.edges if e.length <= max_dist) {
          val adj_v = e.other_vert(from_v)
          if (!cur_ward.contains(adj_v)) {
            cur_ward += adj_v
            evaluating.push(adj_v)
          }
        }
      }

      // get ready for the next ward
      pending_verts --= cur_ward
      vert_wards += cur_ward
    }

    // now determine roads from vertices.
    val wards = new MutableList[Ward]
    val super_roads = new MutableHashSet[Road]  // in between wards
    for (verts <- vert_wards) {
      val ward_roads = new MutableHashSet[Road]
      for (v <- verts; road <- v.roads) {
        // if both vertices of a road are in the same ward, then that road is in
        // the ward. Otherwise it's a super road.
        if (verts.contains(road.v1) && verts.contains(road.v2)) {
          ward_roads += road
        } else {
          super_roads += road
        }
      }
      // we have a ward!
      wards += new Ward(Ward.next_id, ward_roads.toSet)
    }

    return (wards.toList, new Ward(Ward.next_id, super_roads.toSet))
  }

  // Dustin's idea: Wards are the area physically surrounded by "major" roads.
  def construct_dustins_wards(g: PreGraph3): (List[Ward], Ward) = {
    // for now, two classifications: major and minor roads
    val major = new MutableHashSet[Road]
    val minor = new MutableHashSet[Road]

    // trust OSM first
    // TODO this could be a groupBy
    for (road <- g.roads) {
      road.road_type match {
        case "residential" => minor += road
        case _             => major += road
      }
    }

    // and then don't. How many roads was an OSM way split into? swap the top
    // percent of those to being major
    val osm_origs = g.roads.groupBy(r => r.osm_id).toList.sortBy(_._2.size)
    val percent = 0.05  // TODO cfg

    val candidates = osm_origs.reverse.slice(0, (osm_origs.size * percent).toInt).flatMap(_._2)
    minor --= candidates
    major ++= candidates

    // These majors determine boundaries for containing wards, but we'll still
    // add more fake major roads from wards with only 1 road in them.
    val orig_majors = major.toSet

    // Now take each minor road and flood, stopping at major roads. That'll
    // discover the clusters of minor roads in between major roads. Additionally
    // it gives us connectivity properties within the wards -- IF there aren't
    // wacky one-ways.
    val wards = new MutableList[Ward]
    while (minor.nonEmpty) {
      val representative = minor.head
      val roads = flood_roads(representative, orig_majors)
      if (roads.size == 1) {
        // A ward with only one road is useless... just add it to the highway
        // instead.
        major ++= roads
      } else {
        wards += new Ward(Ward.next_id, roads)
      }

      minor --= roads
    }

    // Since we use hashes during construction, force determinism by sorting
    // (Yes, I realize it's possible for two wards to wind up with the same
    // center. Screw the pathological case.)
    var i = 0
    for (w <- wards.toList.sortBy(_.center)) {
      w.id = i
      i += 1
    }
    val major_ward = new Ward(i, major.toSet)

    return (wards.toList.sortBy(w => w.center), major_ward)
  }

  def flood_roads(start: Road, major: Set[Road]): Set[Road] = {
    val set = new MutableHashSet[Road]
    val stack = new MutableStack[Road]

    def consider(ls: Set[Road]) = {
      // & is intersection, if unclear
      if ((ls & major.toSet).size == 0) {
        for (r <- ls if !set(r)) {
          stack.push(r)
        }
        set ++= ls
      }
    }

    stack.push(start)
    while (stack.nonEmpty) {
      val from = stack.pop
      set += from
      consider(from.v1.roads)
      consider(from.v2.roads)
    }
    return set.toSet
  }
}
