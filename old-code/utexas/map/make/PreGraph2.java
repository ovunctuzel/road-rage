/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import utexas.map.Coordinate;

import static utexas.helper.Log.*;

import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

// Another intermediate structure used during building
public class PreGraph2 {
    // TODO possibly want to change to linked lists, since we're deleting from the middle
    // during SCC management
    public List<TmpEdge2> edges = new ArrayList<TmpEdge2>();
    public List<TmpVertex2> vertices = new ArrayList<TmpVertex2>();
    public Map<Coordinate, TmpVertex2> vertLookup = new HashMap<Coordinate, TmpVertex2>();
    public double width, height;

    PreGraph2 (Double w, Double h) {
        width = w;
        height = h;
    }

    public void addEdge (Coordinate from, Coordinate to, String name, String type, boolean oneway, List<Coordinate> pts, int origID) {
        TmpVertex2 v1 = getV(from);
        TmpVertex2 v2 = getV(to);

        // Do we already have an edge from v1->v2 or v2->v1?
        boolean newEdge = false;
        TmpEdge2 e = v1.findEdgeTo(v2, name);
        if (e == null) {
            e = v2.findEdgeTo(v1, name);
        }
        if (e == null) {
            // Fine, make it.
            newEdge = true;
            e = new TmpEdge2(v1, v2, name, type, oneway, pts, origID);
        } else {
            // Make sure the data matches up.
            boolean ptsMatch = e.points.equals(pts);
            if (!ptsMatch) {
                // We might be looking at the TmpEdge2 in reverse
                // TODO does this mangle oneway?
                Collections.reverse(pts);
                ptsMatch = e.points.equals(pts);
                if (!ptsMatch) {
                    dbug("Points data for edge %s still disagrees, forcing new edge...", e.name);
                    //dbug("  existing pts: " + e.points);
                    //dbug("  new pts: " + pts);
                    // TODO not sure if this is right; osm is just WRONG in these cases.
                    newEdge = true;
                    e = new TmpEdge2(v1, v2, name, type, oneway, pts, origID);
                } else {
                    dbug("mangling the points actually helped for %s!", name);
                }
            }
            if (!e.name.equals(name) || !e.type.equals(type)) {
                dbug("Edge data still fails: %s vs %s, %s vs %s", e.name, name, e.type, type);
            }
        }
        v1.roads.add(e);
        v2.roads.add(e);
        if (newEdge) {
            edges.add(e);
        }
    }

    private TmpVertex2 getV (Coordinate at) {
        if (vertLookup.containsKey(at)) {
            return vertLookup.get(at);
        } else {
            TmpVertex2 v = new TmpVertex2(at, new LinkedList<TmpEdge2>());
            vertices.add(v);
            vertLookup.put(at, v);
            return v;
        }
    }

    // Helper classes

    private static int wayId = 0;
    public class TmpEdge2 {
        public String name = null, type = null;
        public List<Coordinate> points;
        public int id, orig_id;
        public int v1, v2;
        public int osmID;
        public boolean oneway;

        TmpEdge2 (TmpVertex2 from, TmpVertex2 to, String roadName, String roadType, boolean oneWay, List<Coordinate> pts, int origID) {
            id = wayId++;
            orig_id = id;   // store a copy!
            name = roadName;
            type = roadType;
            oneway = oneWay;
            points = pts;
            v1 = from.id;
            v2 = to.id;
            osmID = origID;
        }

        public List<Integer> getIntersections () {
            List<Integer> ls = new LinkedList<Integer>();
            ls.add(v1);
            ls.add(v2);
            return ls;
        }

        public int hashCode () {
            return id;
        }

        public boolean equals (Object rhs) {
            if (rhs == null || getClass() != rhs.getClass()) {
                return false;
            } else {
                TmpEdge2 e2 = (TmpEdge2) rhs;
                return e2.id == id;
            }
        }

        // Now yields -1 if there's no match at all
        public int other (TmpVertex2 notMe) {
            if (v1 == notMe.id) {
                return v2;
            } else if (v2 == notMe.id) {
                return v1;
            } else {
                return -1;
            }
        }
    }

    private static int vertId = 0;
    public class TmpVertex2 {
        public Coordinate location;
        public List<TmpEdge2> roads;
        public int id, orig_id;
        public int idx, low;    // For Tarjan's algorithm
        public boolean inStack = false;

        TmpVertex2 (Coordinate where, List<TmpEdge2> edges) {
            id = vertId++;
            orig_id = id;   // store a copy!
            location = where;
            roads = edges;
        }

        public int hashCode () {
            return id;
        }

        public boolean equals (Object rhs) {
            if (rhs == null || getClass() != rhs.getClass()) {
                return false;
            } else {
                TmpVertex2 v2 = (TmpVertex2) rhs;
                return v2.id == id;
            }
        }

        // We are now a multigraph when we have cul-de-sacs! So check road names.
        public TmpEdge2 findEdgeTo (TmpVertex2 to, String edgeName) {
            for (TmpEdge2 e : roads) {
                if (e.other(this) == to.id) {
                    if (e.name.equals(edgeName)) {
                        return e;
                    } else {
                        //dbug("NOTE: Cul-de-sac near " + e.name + " and " + edgeName);
                    }
                }
            }
            return null;
        }
    }
}
