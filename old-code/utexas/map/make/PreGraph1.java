/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import utexas.map.Coordinate;

import utexas.helper.Util;
import static utexas.helper.Log.*;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;

// Intermediate structure used during building
public class PreGraph1 {
    // Used only temporarily during construction to normalize coordinates. and later if we
    // want to dump to GPS.
    public double xOff, yOff, scale;

    public List<TmpEdge1> edges      = new ArrayList<TmpEdge1>();
    public List<TmpVertex1> vertices = new ArrayList<TmpVertex1>();

    public Map<Coordinate, TmpVertex1> vertLookup = new HashMap<Coordinate, TmpVertex1>();

    public double width, height;

    public PreGraph1 () {}

    // Returns the edge's UID
    public int addEdge (String name, String type, boolean oneway, List<Coordinate> pts, int origID) {
        TmpEdge1 e = new TmpEdge1(name, type, oneway, pts, origID);
        edges.add(e);
        return e.id;
    }

    public void addVertex (Coordinate where, List<Integer> edgeIds) {
        if (vertLookup.containsKey(where)) {
            dbug("duplicate vertex!!!" + where);
        }

        List<TmpEdge1> edgeLs = new LinkedList<TmpEdge1>();
        for (int id : edgeIds) {
            edgeLs.add( edges.get(id) );
        }
        TmpVertex1 v = new TmpVertex1(where, edgeLs);
        vertices.add(v);
        vertLookup.put(where, v);
        for (TmpEdge1 e : v.roads) {
            e.intersections.add(v.id);
        }
    }

    // Call me after all edges and vertices have been added.
    public void constructGraph () {
        // Calculate bounds of the map from only the nodes we use
        Double minX = null, maxX = null, minY = null, maxY = null;
        for (TmpEdge1 e : edges) {
            for (Coordinate pt : e.points) {
                if (minX == null || pt.x < minX) {
                    minX = pt.x;
                }

                if (minY == null || pt.y < minY) {
                    minY = pt.y;
                }

                if (maxX == null || pt.x > maxX) {
                    maxX = pt.x;
                }

                if (maxY == null || pt.y > maxY) {
                    maxY = pt.y;
                }
            }
        }
        dbug("Bounds: %f .. %f,  %f .. %f", minX, maxX, minY, maxY);

        // So to make (xMin, yMin) the new origin...
        xOff = 0 - minX;
        yOff = 0 - minY;

        // Scale everything up by fixed ratio...
        scale = Util.cfg_double("mapmake-scale");
        width = (maxX + xOff) * scale;
        height = (maxY + yOff) * scale;

        //dbug("offset %f, %f to make %f, %f map", xOff, yOff, width, height);

        // Normalize all coordinates.
        for (TmpVertex1 v : vertices) {
            normalize(v.location);
        }
        for (TmpEdge1 e : edges) {
            for (Coordinate pt : e.points) {
                normalize(pt);
            }
        }
    }

    // Scale and offset things and correct for origin
    private void normalize (Coordinate pt) {
        pt.x = (pt.x + xOff) * scale;
        pt.y = (pt.y + yOff) * scale;
        // Longitude, Latitude origin is bottom-left; we draw from top-left
        pt.y = height - pt.y;

        if (pt.x < 0 || pt.x > width || pt.y < 0 || pt.y > height) {
            dbug("OOB coord after normalizing! %f, %f", pt.x, pt.y);
        }
    }

    // Helper classes

    private static int wayId = 0;

    public class TmpEdge1 {
        public String name = null, type = null;
        public boolean oneway;
        public List<Coordinate> points;
        public int id;
        public List<Integer> intersections = new LinkedList<Integer>();
        public int osmID;

        TmpEdge1 (String roadName, String roadType, boolean oneWay, List<Coordinate> pts, int origID) {
            id = wayId++;
            name = roadName;
            type = roadType;
            oneway = oneWay;
            points = pts;
            osmID = origID;
        }
    }

    private static int vertId = 0;
    public class TmpVertex1 {
        public Coordinate location;
        public List<TmpEdge1> roads;
        public int id;

        TmpVertex1 (Coordinate where, List<TmpEdge1> edges) {
            id = vertId++;
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
                TmpVertex1 v2 = (TmpVertex1) rhs;
                return v2.id == id;
            }
        }
    }
}
