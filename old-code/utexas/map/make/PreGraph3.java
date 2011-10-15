/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import static utexas.helper.Log.dbug;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

import utexas.helper.Pair;
import utexas.map.Coordinate;
import utexas.map.Edge;
import utexas.map.Graph;
import utexas.map.Road;
import utexas.map.Vertex;

// The final form of the map, but with some baggage for the pass3 construction
public class PreGraph3 extends Graph {
    public PreGraph3 (double w, double h, double xoff, double yoff, double scale) {
        super(w, h, xoff, yoff, scale, 0, 0, 0);   // do not start with anything
    }

    public Vertex addV (Coordinate loc, int id) {
        Vertex v = new Vertex(id, loc);
        vertices.add(v);
        if (id != vertices.size() - 1) {
            dbug("Vertex ID doesn't fit! %d given, need %d", id, vertices.size() - 1);
        }
        return v;
    }

    public Road addR (String name, String type, List<Coordinate> pts, int osmId, Vertex v1, Vertex v2) {
        Road r = new Road(roads.size(), name, type, pts, osmId);
        roads.add(r);

        // Impose consistent ordering on roads to force lanes to match up
        // so actually don't, it messes up OSM's oneway ordering. all this does is make
        // orientation arrows wacky colors, but POS/NEG orientation dont matter, so who
        // cares.
        /*
        if (pts.get(pts.size() - 1).lessThan( pts.get(0) )) {
            //dbug("Reversing points for " + toString());
            Collections.reverse(pts);
            // TODO should reflect in r.points as well!
        }
        */
        
        // And we should do it to the vertices as well
        if (pts.get(0).equals(v1.location)) {
            r.v1 = v1;
            r.v2 = v2;
        } else {
            r.v1 = v2;
            r.v2 = v1;
        }
        
        if (!pts.get(0).equals(r.v1.location) || !pts.get( pts.size() - 1).equals(r.v2.location)) {
            dbug("WARNING: Road order backwards! BAD!!!");
            dbug("  want 1st: %s = %s  and 2nd: %s = %s", r.v1.location, pts.get(0),
                 r.v2.location, pts.get( pts.size() - 1 )
                );
        }

        return r;
    }

    // MUST be called in right-to-left order for lanes. (possibly still issues with this)
    public Edge addE (Road r, Road.DIR dir) {
        Edge e = new Edge(edges.size(), dir);
        e.road = r;
        edges.add(e);

        // Add the lane to the road. first is defined as rightmost
        if (e.direxn == Road.DIR.POS) {
            r.getPosLanes().add(e);
            e.laneNum = r.getPosLanes().size() - 1;
        } else {
            r.getNegLanes().add(e);
            e.laneNum = r.getNegLanes().size() - 1;
        }

        return e;
    }

    // So the XML writers suck, sorry.
    // TODO the yaml writer doesn't look too horrible.
    // java xml writers seem to all require external dependencies. cheat.
    public void write_xml (String fn, double s, double xo, double yo, boolean gps) {
        scale = s;
        xOff = xo;
        yOff = yo;
        dumpGPS = gps;

        PrintStream out;

        try {
            out = new PrintStream(new FileOutputStream(fn));
        } catch (IOException e) {
            dbug(e.toString());
            return;
        }

        // Start the graph
        out.println(String.format(
            "<graph width=\"%f\" height=\"%f\" xoff=\"%f\" yoff=\"%f\" scale=\"%f\" roads=\"%d\" edges=\"%d\" verts=\"%d\">",
            width, height, xOff, yOff, scale, roads.size(), edges.size(), vertices.size()
        ));

        dbug("Dumping %d roads...", roads.size());
        // Dump roads
        for (Road r : roads) {
            out.println(String.format(
                "  <road name=\"%s\" type=\"%s\" osmid=\"%d\" id=\"%d\" v1=\"%d\" v2=\"%d\">",
                clean(r.getName()), clean(r.getType()), r.getOsmId(), r.id, r.v1.id, r.v2.id
            ));
            for (Coordinate pt : r.getPoints()) {
                out.println(String.format("    <pt x=\"%f\" y=\"%f\"/>", denormX(pt.x), denormY(pt.y)));
            }
            // Note: lanes reference the roads, so don't store twice. Just make sure idx is
            // right later.
            out.println("  </road>");
        }

        dbug("Dumping %d edges...", edges.size());
        // Dump edges
        for (Edge e : edges) {
            out.println(String.format(
                "  <edge id=\"%d\" road=\"%d\" dir=\"%c\" laneNum=\"%d\">",
                e.id, e.road.id, e.direxn == Road.DIR.POS ? '+' : '-', e.laneNum
            ));
            for (Edge.Line ln : e.lines) {
                out.println(String.format(
                    "    <line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\"/>",
                    denormX(ln.x1), denormY(ln.y1), denormX(ln.x2), denormY(ln.y2)
                ));
            }
            out.println("  </edge>");
        }

        dbug("Dumping %d vertices...", vertices.size());
        // Dump vertices
        for (Vertex v : vertices) {
            out.println(String.format(
                "  <vertex id=\"%d\" x=\"%f\" y=\"%f\">",
                v.id, denormX(v.location.x), denormY(v.location.y)
            ));
            for (Edge from : v.getMap().keySet()) {
                for (Pair<Vertex.TURN_TYPE, Edge> to : v.getMap().get(from)) {
                    out.println(String.format(
                        "    <link from=\"%d\" to=\"%d\" type=\"%c\"/>",
                        from.id, to.get2().id, to.get1().code()
                    ));
                }
            }
            out.println("  </vertex>");
        }

        out.println("</graph>");
        out.close();
    }

    // Escape characters as cheaply as possible
    private String clean (String dirty) {
        // I'm trusting
        // http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references#Predefined_entities_in_XML
        return dirty.replaceAll("\"", "&quot;").replaceAll("&", "&amp;").replaceAll("'", "&apos;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
    }

    // so some targets want the raw GPS coordinates. I tried storing those and normalizing
    // upon reserialization in my stage only, but it broke nearly everything, so forget
    // that! Be lazy.

    private boolean dumpGPS = false;
    private double scale, xOff, yOff;

    private double denormX (double x) {
        if (dumpGPS) {
            return (x / scale) - xOff;
        } else {
            return x;
        }
    }

    private double denormY (double y) {
        if (dumpGPS) {
            return ((height - y) / scale) - yOff;
        } else {
            return y;
        }
    }
}
