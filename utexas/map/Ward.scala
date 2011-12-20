package utexas.map

// TODO combo these lines!
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.mutable.{HashSet => MutableHashSet}
import scala.collection.mutable.MutableList
import scala.collection.mutable.{Stack => MutableStack}

class Ward(val roads: Set[Road]) {
  override def toString = "Ward with " + roads.size + " roads"
}

object Ward {
  // Mike's idea: flood out from vertices as long as there's a small distance
  // along the edges. This finds vertices that are clustered "densely." Roads
  // completely inside one of these floods constitute a Ward, and those along
  // the border form "super-roads" that link Wards.
  def construct_mikes_wards(g: Graph): (List[Ward], Ward) = {
    val max_dist = 200  // between intersections, in meters

    // This assigns wards to vertices. We'll map that to roads later.
    val vert_wards = new MutableList[MutableSet[Vertex]]()

    // these are waiting to go in any ward
    val pending_verts = new MutableHashSet[Vertex]()
    pending_verts ++= g.vertices
    while (pending_verts.size != 0) {
      // make a new ward...
      val cur_ward = new MutableHashSet[Vertex]
      val evaluating = new MutableStack[Vertex]
      val start_with = pending_verts.head
      evaluating.push(start_with)
      cur_ward += start_with

      // add all vertices a fixed distance away from the one we're evaluating,
      // meaning this goes by density and not total distance
      while (evaluating.size != 0) {
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
      wards += new Ward(ward_roads.toSet)
    }

    return (wards.toList, new Ward(super_roads.toSet))
  }

  // Dustin's idea: Wards are the area physically surrounded by "major" roads.
  def construct_dustins_wards(g: Graph): (List[Ward], Ward) = {
    // for now, two classifications: major and minor roads
    val major = new MutableHashSet[Road]
    val minor = new MutableHashSet[Road]

    // TODO but osm's idea of residential seems wrong in so many cases... so
    // next, count how many vertices an osm_id is a part of, and add candidates
    // from that.
    for (road <- g.roads) {
      road.road_type match {
        case "residential" => minor += road
        case _             => major += road
      }
    }

    return (List(new Ward(minor.toSet)), new Ward(major.toSet))
  }
}
