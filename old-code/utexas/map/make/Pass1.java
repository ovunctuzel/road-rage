/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import utexas.map.Coordinate;

import utexas.helper.MultiMap;

import static utexas.helper.Log.*;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Set;
import java.util.HashSet;

import java.io.FileReader;

// sax doesn't eat memory, jdom does
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.DefaultHandler;

// Takes an OSM (XML) file to a preliminary graph.
public class Pass1 extends DefaultHandler {
    // Things to ignore in an osm way
    final static Set<String> ignoreMe = new HashSet<String>();
    static {
        ignoreMe.add("boundary");   // edge of city
        ignoreMe.add("railway");    // tracks would be too easy
        ignoreMe.add("amenity");    // no stops on this trip
        ignoreMe.add("aeroway");    // we're not cl0ud
        ignoreMe.add("landuse");    // we don't want to use it
        ignoreMe.add("natural");    // naturally useless to us
        ignoreMe.add("waterway");   // we don't swim
        ignoreMe.add("building");   // we try not to drive through these
        ignoreMe.add("foot");       // we don't have feet
        ignoreMe.add("man_made");   // man-made things tend to suck
        ignoreMe.add("crossing");   // TODO dunno?
        ignoreMe.add("path");
        ignoreMe.add("cycleway");
        ignoreMe.add("footway");
        ignoreMe.add("bridleway");
        ignoreMe.add("steps");
        ignoreMe.add("pedestrian");
        ignoreMe.add("bus_guideway");
        // TODO cemeteries in Houston, real roads in BTR, alleys in ATX. they cause
        // problems. so discard when they have no name.
        //ignoreMe.add("service");
    }

    Map<Integer, Coordinate> nodes = new HashMap<Integer, Coordinate>();
    // map node IDs to list of road IDs using it
    MultiMap<Integer, Integer> sharedNodes = new MultiMap<Integer, Integer>();
    // Oh yeah, that thing we really want.
    PreGraph1 graph;

    public PreGraph1 parse (String in) {
        graph = new PreGraph1();

        try {
            XMLReader xr = XMLReaderFactory.createXMLReader();
            xr.setContentHandler(this);
            xr.setErrorHandler(this);
            xr.parse( new InputSource( new FileReader(in) ) );
        } catch (Exception e) {
            cmt("ERR: " + e.toString());
        }

        // Locate intersections
        for (int nodeId : sharedNodes.keySet()) {
            List<Integer> roadLs = sharedNodes.get(nodeId);
            if (roadLs.size() > 1) {
                Coordinate location = new Coordinate( nodes.get(nodeId) );  // copy it
                graph.addVertex(location, roadLs);
            }
        }

        // We also have vertices at the ends of roads, unless that's already a vertex.
        for (PreGraph1.TmpEdge1 e : graph.edges) {
            List<Coordinate> pts = e.points;
            Coordinate endA = new Coordinate( pts.get(0) ); // endA gets modded somewhere
            Coordinate endB = new Coordinate( pts.get( pts.size() - 1) );
            List<Integer> ls = new LinkedList<Integer>();
            ls.add(e.id);   // should be the only one

            if (!graph.vertLookup.containsKey(endA)) {
                graph.addVertex(endA, ls);
            }
            if (!graph.vertLookup.containsKey(endB)) {
                graph.addVertex(endB, ls);
            }
        }

        // Normalize coordinates and stuff
        graph.constructGraph();

        return graph;
    }

    // Note: The boundaries from the 'bounds' element are fail and wrong. Plus we base it
    // off only the nodes we use.

    // per way
    String name, type;
    boolean oneway = false;
    int id;
    boolean skipWay;
    List<Integer> refs;

    @Override public void startElement (String junk1, String junk2, String tag, Attributes attribs) {
        // Luckily the nodes almost always come before the ways
        if (tag.equals("node")) {
            String viz = attribs.getValue("visible");
            if (viz != null && !viz.equals("true")) {
                dbug("WARNING: invisible node in an osm");
                return;
            }
            // Some nodes have tags, meaning that they represent places or landmarks.
            // But we don't care -- the set of nodes is intermediate anyway.
            int id = Integer.parseInt( attribs.getValue("id") );
            double x = Double.parseDouble( attribs.getValue("lon") );
            double y = Double.parseDouble( attribs.getValue("lat") );

            nodes.put(id, new Coordinate(x, y));
        } else if (tag.equals("way")) {
            String viz = attribs.getValue("visible");
            if (viz != null && !viz.equals("true")) {
                dbug("WARNING: invisible way in an osm");
                return;
            }

            id = Integer.parseInt( attribs.getValue("id") );
            skipWay = false;
            name = null;
            type = null;
            oneway = false;
            refs = new LinkedList<Integer>();
        } else if (tag.equals("tag")) {
            if (skipWay) {
                return;
            }

            String key = attribs.getValue("k");
            String value = attribs.getValue("v");
            if (ignoreMe.contains(key)) {
                //dbug("WARNING: skipping " + key);
                skipWay = true;
                return;
            } else if (key.equals("name")) {
                name = value;
            } else if (key.equals("highway")) {
                if (value != null && ignoreMe.contains(value)) {
                    skipWay = true;
                    return;
                }
                type = value;
            } else if (key.equals("oneway")) {
                oneway = value.equals("yes");
            } else if (key.equals("area")) {
                // these are weird.
                skipWay = true;
                return;
            }
        } else if (tag.equals("nd")) {
            if (skipWay) {
                return;
            }

            int ref = Integer.parseInt( attribs.getValue("ref") );
            if (nodes.containsKey(ref)) {
                refs.add(ref);
            } else {
                dbug("WARNING: way references unknown node");
            }
        }
    }

    @Override public void endElement (String junk1, String junk2, String tag) {
        if (tag.equals("way")) {
            if (skipWay) {
                return;
            }

            if (name == null || type == null) {
                if (name == null && type != null && type.equals("service")) {
                    // but really questionable...
                    // TODO these are wacky. when they're valid, they seem to have a name.
                    // but junk the rest of them. alleys, cemetary paths, driveways.
                    return;
                }
                //dbug("WARNING: way lacks name or type: " + id);
                if (name == null) {
                    name = "NO-NAME (ID " + id + ")";
                }
                if (type == null) {
                    type = "null";
                }
            }

            LinkedList<Coordinate> points = new LinkedList<Coordinate>();
            if (type.equals("motorway") || type.equals("motorway_link") || type.equals("trunk")) {
                // according to http://wiki.openstreetmap.org/wiki/Key:oneway
                oneway = true;
            }
            int edgeId = graph.addEdge(name, type, oneway, points, id);

            // Get all the constituent nodes
            for (int ref : refs) {
                points.add( new Coordinate( nodes.get(ref) ) );   // copy it
                sharedNodes.put(ref, edgeId);
            }
        }
    }

    // Sweet, OSM doesn't store anything like this!
    @Override public void characters (char ch[], int start, int length) {}
}
