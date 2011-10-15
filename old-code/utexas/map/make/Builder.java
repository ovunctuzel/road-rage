/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import static utexas.helper.Log.dbug;
import static utexas.helper.Log.pop;
import static utexas.helper.Log.push;

// Handles the entire construction of a graph from an osm.
public class Builder {
    public static void main (String[] args) {
        String fn = null;
        boolean gpsmode = false;
        if (args.length > 0) {
            fn = args[0];
            if (args.length == 2) {
                gpsmode = true;
            }
        }
        if (fn == null || fn.equals("")) {
            fn = "dat/btr.osm";
        }

        // osm -> pg1 (undirected graph, vertex intersections, edges are entire roads)
        dbug("Pass 1 initiating...");
        push();
        PreGraph1 g1 = (new Pass1()).parse(fn);
        pop();
        dbug("Pass 1: %d vertices, %d edges", g1.vertices.size(), g1.edges.size());

        double scale = g1.scale;
        double xo = g1.xOff;
        double yo = g1.yOff;

        // pg1 -> pg2 (undirected multigraph, vertex intersections, edges are between vertices)
        dbug("Pass 2 initiating...");
        push();
        PreGraph2 g2 = (new Pass2()).separate(g1);
        pop();
        dbug("Pass 2: now %d intersections, %d roads", g2.vertices.size(), g2.edges.size());

        // pg2 -> g (directed multigraph, smart vertex intersections, edges are lanes)
        dbug("Pass 3 initiating...");
        push();
        PreGraph3 g = (new Pass3()).make_lanes(g2, xo, yo, scale);
        pop();
        dbug(
            "Pass 3: finally %d intersections, %d roads, and %d edges/lanes",
            g.getVertices().size(), g.getRoads().size(), g.getEdges().size()
        );

        dbug("Saving...");
        push();
        g.write_xml("dat/test.map", scale, xo, yo, gpsmode);
        pop();
        dbug("Done!");
    }
}
