/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import static utexas.helper.Log.dbug;
import static utexas.helper.Log.pop;
import static utexas.helper.Log.push;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import utexas.helper.MultiMap;
import utexas.helper.Pair;
import utexas.helper.Util;
import utexas.map.Coordinate;
import utexas.map.Edge;
import utexas.map.Road;
import utexas.map.Turn;
import utexas.map.Vertex;
import utexas.map.Vertex.TURN_TYPE;

// transform undirected graph into a directed graph by making individual lanes
public class Pass3 {
    // Store these temporarily
    MultiMap<Vertex, Edge> incoming = new MultiMap<Vertex, Edge>();
    MultiMap<Vertex, Edge> outgoing = new MultiMap<Vertex, Edge>();

    public PreGraph3 make_lanes (PreGraph2 graph, double xoff, double yoff, double scale) {
        PreGraph3 g = new PreGraph3(graph.width, graph.height, xoff, yoff, scale);

        // Pretty much just copy the vertex locations first...
        for (PreGraph2.TmpVertex2 v : graph.vertices) {
            g.addV(v.location, v.id);
        }

        // Now create directed edges
        for (PreGraph2.TmpEdge2 oldEdge : graph.edges) {
            // vert ordering here isn't correct yet. (wait, what? i have no idea what i
            // meant)
            Vertex v1 = g.getVertex(oldEdge.v1);
            Vertex v2 = g.getVertex(oldEdge.v2);
            Road r = g.addR(oldEdge.name, oldEdge.type, oldEdge.points, oldEdge.osmID, v1, v2);

            // TODO choose number of lanes by road type. problem is, practically speaking,
            // the osm maps I'm looking at suck and are inconsistent. we gotta gather our
            // own data eventually.
            int lanes = 2;
            if (oldEdge.type.equals("residential")) {
                // TODO this _isn't_ right. in a map of Baton Rouge, most of goodwood blvd
                // is marked this way, but parts of it definitely have 2 lanes. osm is
                // ambiguous and wrong, so consider this somewhat arbitrary.
                lanes = 1;
            } else if (oldEdge.type.equals("motorway_link")) {
                // These should be one-way always; merge ramps onto big highways
                lanes = 1;
            } else if (oldEdge.type.equals("service")) {
                // I don't know what the hell these are supposed to be
                lanes = 1;
            }

            addEdges(g, r, lanes, oldEdge.oneway);
        }

        finalize(g);

        return g;
    }

    public void addEdges (PreGraph3 g, Road r, int lanes, boolean oneway) {
        // v1 -> v2 lanes
        for (int l = 0; l < lanes; l++) {
            Edge e = g.addE(r, Road.DIR.POS);   // v1 -> v2 matches r
            outgoing.put(r.v1, e);
            incoming.put(r.v2, e);
        }

        // If we're oneway, only do v1 -> v2 -- or whatever the original osm order was.
        if (!oneway) {
            // v2 -> v1 lanes
            for (int l = 0; l < lanes; l++) {
                Edge e = g.addE(r, Road.DIR.NEG);   // v2 -> v1 matches r
                outgoing.put(r.v2, e);
                incoming.put(r.v1, e);
            }
        }
    }

    public void finalize (PreGraph3 g) {
        // Need to know how many lanes there are total in a road, so defer this as late as possible
        for (Edge e : g.getEdges()) {
            buildLines(e);
        }

        // now take from and to list of edges and map them.
        dbug("Mapping vertex links...");
        push();
        for (Vertex v : g.getVertices()) {
            mapEdges(v, incoming.get(v), outgoing.get(v));
        }
        pop();

        dbug("Adusting lanes' terminal line segments...");
        push();
        for (Edge e : g.getEdges()) {
            fix_connections_rights(e);
        }
        for (Edge e : g.getEdges()) {
            fix_connections_others(e);
        }
        pop();
    }

    // Build and store directed lines constituting this edge, in order.
    private void buildLines (Edge e) {
        List<Coordinate> pts = e.road.getPoints();
        LinkedList<Edge.Line> lines = new LinkedList<Edge.Line>();
        for (int i = 1; i < pts.size(); i++) {
            Coordinate from = pts.get(i - 1);
            Coordinate to = pts.get(i);
            
            //Edge.Line l;
            if (e.direxn == Road.DIR.POS) {
                lines.add(getOffset(e.getLaneOffset(), from, to));
            } else {
                // makes sense, pos is v1->v2 and now there's ordering on points
                lines.addFirst(getOffset(e.getLaneOffset(), to, from));
            }
        }

        // Now post-proc the lines, forcing them to meet up on "inner" segments. Where
        // these guys hit lines of other edges... well that's a different story.
        for (int i = 0; i < lines.size() - 1; i++) {
            Edge.Line l1 = lines.get(i);
            Edge.Line l2 = lines.get(i + 1);

            // end of l1 should match with start of l2
            Coordinate hit = l1.intersection(l2);
            // Do we get parallel lanes often?
            if (hit != null) {
                // allowed to extend.
                l1.x2 = l2.x1 = hit.x;
                l1.y2 = l2.y1 = hit.y;
            } else {
                //dbug("parallel intra-lane");
            }
        }

        e.lines.addAll(lines);
    }

    // the real construction bit. which edges lead to which other edges?
    private void mapEdges (Vertex v, List<Edge> inEdges, List<Edge> outEdges) {
        // TODO this has something to do with one-ways
        if (inEdges == null) {
            dbug("%s has no in edges", v);
            //dbug("outs: " + outEdges);
            return;
        }
        if (outEdges == null) {
            dbug("%s has no out edges", v);
            //dbug("in: " + inEdges);
            return;
        }

        // for each in, probably want every out that doesnt involve that in?
        // then if the road ids/names match, that's probably like crossing the intersection
        // else turning right or left.
        MultiMap<Edge, Pair<TURN_TYPE, Edge>> map = v.getMap();

        // Are we a dead-end? partition I/O edges into roads and see how many we have.
        Set<Road> roads  = new HashSet<Road>();
        for (Edge e : Util.loopAll(inEdges, outEdges)) {
            roads.add(e.getRoad());
        }
        if (roads.size() == 1) {
            List<Edge> outLanes = outEdges.get(0).getLanes();
            // link corresponding lane numbers
            for (Edge e : inEdges) {
                map.put(e, new Pair<TURN_TYPE, Edge>(TURN_TYPE.UTURN, outLanes.get(e.laneNum)));
            }
            return;
        }

        /* technically, its straight even if at a weird angle. see landwood dr X thornwood
           dr in BTR. what matters is the cross streets. */

        for (Edge from : inEdges) {
            for (Edge to : outEdges) {
                if (from.road != to.road) {   // no super U-turns
                    double fromAngle = from.getLastVector().angle();
                    double toAngle = to.getFirstVector().angle();
                    final double cross_threshold = Math.PI / 10; // 18 degrees off
                    boolean straight = Math.abs( Util.angleRot(fromAngle, toAngle) ) <= cross_threshold;


                    if (from.road.getOsmId() == to.road.getOsmId() || straight) {
                        // crossing the intersection... rely on OSM's idea of road
                        // continuations. or try to use angle.

                        /* right now, assume right lane matches up for each road? till we
                           get real data, who knows. well, till we get real data, lanes
                           will probably always match up.
                        */

                        int diff = from.numLanes() - to.numLanes();
                        if (diff == 0) {
                            // lanes match up perfectly. don't change lanes while crossing.
                            if (from.laneNum == to.laneNum) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.CROSS, to));
                            }
                        } else if (diff > 0) {
                            // n -> n - m lanes; merge
                            // match up each lane, then the leftmost has to merge
                            if (from.laneNum == to.laneNum) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.CROSS, to));
                            } else if (from.laneNum >= diff && to.isLeftmost()) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.CROSS_MERGE, to));
                            }
                        } else if (diff < 0) {
                            // n -> n + m lanes; get to choose
                            // match up each lane, then link the leftmost to the rest
                            if (from.laneNum == to.laneNum) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.CROSS, to));
                            } else if (from.isLeftmost() && to.laneNum > from.laneNum) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.CROSS, to));
                            }
                        }
                    } else if (from.laneNum == to.laneNum || to.numLanes() == 1 || from.numLanes() == 1) {
                        // or there's only one lane where we're headed, or where we're
                        // headed from...

                        // clockwise (negative) or counter-clockwise (positive) angle
                        // rotations?

                        if (Util.angleRot(fromAngle, toAngle) < 0) {
                            // can only right turn if we're the right lane
                            if (from.isRightmost()) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.RIGHT, to));                      
                            }
                        } else {
                            if (from.isLeftmost()) {
                                map.put(from, new Pair<TURN_TYPE, Edge>(TURN_TYPE.LEFT, to));                       
                            }
                        }
                    }
                }
            }
        }

        // Sanity check. Everything should be connected...
        for (Edge from : inEdges) {
            if (v.nextEdges(from).size() == 0) {
                dbug("GRR: nowhere to go after %s", from);
            }
        }
        for (Edge to : outEdges) {
            if (v.prevEdges(to).size() == 0) {
                dbug("GRR: nothing leads to %s", to);
            }
        }
    }

    // Adjust the endpoints of each lane's line segments to properly meet at intersections.
    // This is just for right turns.
    private void fix_connections_rights (Edge e) {
        // TODO We're gonna try fixing one case first.
        if (!e.isRightmost()) {
            return;
        }

        // Let's assume right turns always hit.
        for (Turn link : e.getDestinations()) {
            if (link.type == Vertex.TURN_TYPE.RIGHT) {
                Edge.Line l1 = e.lines.get(e.lines.size() - 1);
                Edge.Line l2 = link.to.lines.get(0);
                Coordinate hit = l1.intersection(l2);

                //dbug("%s (%s) and %s (%s) collide at %s\n", l1, e, l2, link.to, hit);

                // TODO Maybe whine if we have no hit?
                if (hit != null) {
                    l1.adjustEnd(hit);
                    l2.adjustStart(hit);
                } else {
                    //dbug("parallel right turn btwn %s and %s", e, link.to);
                }
            }
        }
    }

    // Adjust the endpoints of each lane's line segments to properly meet at intersections.
    // This is for everything else!
    private void fix_connections_others (Edge e) {
        // TODO We're gonna try fixing one case first.
        if (e.isRightmost()) {
            return;
        }

        // So find the lane that we cross...
        Edge.Line cross_ln = null;
        for (Turn link : e.getRightmost().getDestinations()) {
            if (link.type == Vertex.TURN_TYPE.RIGHT) {
                cross_ln = link.to.lines.get(0);
                break;
            }
        }

        if (cross_ln == null) {
            //dbug("Couldn't find cross street 1 for poor %s", e);
        } else {
            Edge.Line us = e.lines.get( e.lines.size() - 1 );
            Coordinate hit = us.intersection(cross_ln);
            // TODO Maybe whine if we have no hit?
            if (hit != null) {
                us.adjustEnd(hit);
            } else {
                //dbug("parallel... wait what?!");
            }
        }

        // So find the lane that we right-turn from...
        cross_ln = null;
        for (Turn link : e.getRightmost().getSources()) {
            if (link.type == Vertex.TURN_TYPE.RIGHT) {
                cross_ln = link.from.lines.get( link.from.lines.size() - 1 );
                break;
            }
        }

        if (cross_ln == null) {
            //dbug("Couldn't find cross street 2 for poor %s", e);
        } else {
            Edge.Line us = e.lines.get(0);
            Coordinate hit = us.intersection(cross_ln);

            // TODO Maybe whine if we have no hit?
            if (hit != null) {
                us.adjustStart(hit);
            } else {
                //dbug("2 parallel... wait what?!");
            }
        }
    }

    // Geometry for edges. We just compute it once, so do it here.

    private Coordinate getGeneralOffset (Coordinate pt1, Coordinate pt2) {
        double width = Util.cfg_double("maplane-width");
        
        // Draw these as lines parallel to the center using the perpendicular bisector.
        double theta = Util.arctan(pt2.x - pt1.x, pt2.y - pt1.y) + (Math.PI / 2);
        double dx = width * Math.cos(theta);
        double dy = width * Math.sin(theta);
        
        /*
        // Don't quite try to be perfectly parallel. Instead, re-copy the center
        // line but translate it slightly either horizontally or vertically.

        double dx = 0;
        double dy = 0;
        
        if (Math.abs(pt1.x - pt2.x) >= Math.abs(pt1.y - pt2.y)) {
            dy = width;
        } else {
            dx = width;
        }
        */
        
        return new Coordinate(dx, dy);
    }

    // How much do we translate this specific lane
    // we do return a directed edge; arguments impose pt1 -> pt2
    private Edge.Line getOffset (int l, Coordinate pt1, Coordinate pt2) {
        // l is the lane offset
        //int l = getLaneOffset();
        Coordinate off = getGeneralOffset(pt1, pt2);

        // First compute naively all ways
        Coordinate from1 = new Coordinate(pt1.x + (l * off.x), pt1.y + (l * off.y));
        Coordinate to1   = new Coordinate(pt2.x + (l * off.x), pt2.y + (l * off.y));
        Edge.Line l1 = new Edge.Line(from1.x, from1.y, to1.x, to1.y);

        Coordinate from2 = new Coordinate(pt1.x - (l * off.x), pt1.y - (l * off.y));
        Coordinate to2   = new Coordinate(pt2.x - (l * off.x), pt2.y - (l * off.y));
        Edge.Line l2 = new Edge.Line(from2.x, from2.y, to2.x, to2.y);

        // TODO if we still have a bug, then need to normalize the road line.
        // So we calculate the midpt of the center road line, then turn clockwise 90
        // degrees and project a point out. We _should_ hit the midpt of one of the lines.
        Edge.Line roadLine = new Edge.Line(pt1.x, pt1.y, pt2.x, pt2.y);
        Coordinate midRoad = roadLine.midPt();
        double theta = roadLine.angle() - (Math.PI / 2);
        double width = Util.cfg_double("maplane-width");
        Coordinate projectedPt = new Coordinate(
            midRoad.x + l * width * Math.cos(theta),
            midRoad.y - l * width * Math.sin(theta) // y inversion
        );

        Coordinate mid1 = l1.midPt();
        Coordinate mid2 = l2.midPt();

        double diff1 = mid1.difference(projectedPt);
        double diff2 = mid2.difference(projectedPt);

        if (mid1.equals(projectedPt)) {
            return l1;
        } else if (mid2.equals(projectedPt)) {
            return l2;
        } else {
            /*
            dbug("Didn't project to either line! Aaargh. " + toString());
            push();
            dbug("We want to hit " + projectedPt);
            dbug("L1: " + mid1);
            dbug("L2: " + mid2);
            dbug("Errors: " + diff1 + " vs " + diff2);
            pop();
            */
            if (diff1 < diff2) {
                //dbug("Chose l1");
                return l1;
            } else {
                //dbug("Chose l2");
                return l2;
            }
        }
    }
}
