/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

// TODO embed in graph, then we dont need accessors!

package utexas.map.make;

import utexas.map.Graph;
import utexas.map.Road;
import utexas.map.Edge;
import utexas.map.Vertex;
import utexas.map.Coordinate;

import static utexas.helper.Log.*;
import utexas.helper.Pair;
import utexas.helper.Util;

import java.util.List;
import java.util.LinkedList;

import java.io.FileReader;

// sax doesn't eat memory, jdom does
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.DefaultHandler;

public class Reader extends DefaultHandler {
    Graph g;    // I think we want this
    int[] rdV1s, rdV2s, eRds, rPosLanes, rNegLanes; // store ids till later
    @SuppressWarnings("rawtypes")
	LinkedList[] vertLinkLs;         // these're really more ids too. also, java generics fail.

    double width, height, xoff, yoff, scale;   // of the graph

    // per road
    String rdName, rdType;
    int rdOsm, rdId, rdV1, rdV2;
    List<Coordinate> rdPts;

    // per edge
    int eId, eRd, eLane, eDir;
    List<Edge.Line> eLines;

    // per vertex
    class TmpLink {
        int from, to, type;
        TmpLink (int a, int b, int c) {
            from = a;
            to = b;
            type = c;
        }
    }

    int vId;
    double vX, vY;
    List<TmpLink> vLinks;
    
    // Emulates previous functionality of load(), which now supports
    // arbitrary input sources.
    public Graph load(String fn) {
	try {
	    return load(new InputSource(new FileReader(fn)));
	} catch(Exception e) {
	    cmt("ERR during XML: " + e.toString());
	    e.printStackTrace();
	    return null;
	}
    }

    @SuppressWarnings(value = "unchecked")
    public Graph load (InputSource isrc) {
        try {
            XMLReader xr = XMLReaderFactory.createXMLReader();
            xr.setContentHandler(this);
            xr.setErrorHandler(this);
            xr.parse(isrc);
        } catch (Exception e) {
            cmt("ERR during XML: " + e.toString());
            e.printStackTrace();
            return null;
        }

        // Set vertex references for roads
        for (Road r : g.getRoads()) {
            r.v1 = g.getVertices().get(rdV1s[r.id]);
            r.v2 = g.getVertices().get(rdV2s[r.id]);

            Util.inflateArray(r.getPosLanes(), rPosLanes[r.id]);
            Util.inflateArray(r.getNegLanes(), rNegLanes[r.id]);
        }

        // Set road references for edges (and the other way around as well)
        for (Edge e : g.getEdges()) {
            Road r = g.getRoads().get(eRds[e.id]);
            e.road = r;
            if (e.direxn == Road.DIR.POS) {
                r.getPosLanes().set(e.laneNum, e);
            } else {
                r.getNegLanes().set(e.laneNum, e);
            }
        }

        // Set edge references for vertices
        for (Vertex v : g.getVertices()) {
            // yaaaaaay generic type erasure.
            List<TmpLink> links = vertLinkLs[v.id];
            for (TmpLink link : links) {
                Edge from = g.getEdges().get(link.from);
                Edge to = g.getEdges().get(link.to);
                Vertex.TURN_TYPE type;
                switch (link.type) {
                    case 'C': type = Vertex.TURN_TYPE.CROSS;  break;
                    case 'L': type = Vertex.TURN_TYPE.LEFT;   break;
                    case 'R': type = Vertex.TURN_TYPE.RIGHT;  break;
                    case 'U': type = Vertex.TURN_TYPE.UTURN;  break;
                    // and just to shut the compiler up
                    default: type = Vertex.TURN_TYPE.CROSS;   break;
                }
                v.getMap().put(from, new Pair<Vertex.TURN_TYPE, Edge>(type, to));
            }
        }

        return g;
    }

    @Override public void startElement (String junk1, String junk2, String tag, Attributes attribs) {
        if (tag.equals("graph")) {
            width = Double.parseDouble( attribs.getValue("width") );
            height = Double.parseDouble( attribs.getValue("height") );
            xoff = Double.parseDouble( attribs.getValue("xoff") );
            yoff = Double.parseDouble( attribs.getValue("yoff") );
            scale = Double.parseDouble( attribs.getValue("scale") );

            int roads = Integer.parseInt( attribs.getValue("roads") );
            int edges = Integer.parseInt( attribs.getValue("edges") );
            int verts = Integer.parseInt( attribs.getValue("verts") );

            g = new Graph(width, height, xoff, yoff, scale, roads, edges, verts);
            rdV1s = new int[roads];
            rdV2s = new int[roads];
            eRds = new int[edges];
            rPosLanes = new int[roads];
            rNegLanes = new int[roads];
            // java generics fail
            //vertLinkLs = new LinkedList<TmpLink>[verts];
            vertLinkLs = new LinkedList[verts];
        } else if (tag.equals("road")) {
            rdName = attribs.getValue("name");
            rdType = attribs.getValue("type");
            rdOsm = Integer.parseInt( attribs.getValue("osmid") );
            rdId = Integer.parseInt( attribs.getValue("id") );
            rdV1 = Integer.parseInt( attribs.getValue("v1") );
            rdV2 = Integer.parseInt( attribs.getValue("v2") );

            rdPts = new LinkedList<Coordinate>();
        } else if (tag.equals("pt")) {
            double x = Double.parseDouble( attribs.getValue("x") );
            double y = Double.parseDouble( attribs.getValue("y") );
            rdPts.add( new Coordinate(x, y) );
        } else if (tag.equals("edge")) {
            eId = Integer.parseInt( attribs.getValue("id") );
            eRd = Integer.parseInt( attribs.getValue("road") );
            eDir = attribs.getValue("dir").charAt(0);
            eLane = Integer.parseInt( attribs.getValue("laneNum") );

            // arrays initialized to 0, this is fine
            if (eDir == '+') {
                rPosLanes[eRd]++;
            } else {
                rNegLanes[eRd]++;
            }

            eLines = new LinkedList<Edge.Line>();
        } else if (tag.equals("line")) {
            double x1 = Double.parseDouble( attribs.getValue("x1") );
            double y1 = Double.parseDouble( attribs.getValue("y1") );
            double x2 = Double.parseDouble( attribs.getValue("x2") );
            double y2 = Double.parseDouble( attribs.getValue("y2") );
            eLines.add( new Edge.Line(x1, y1, x2, y2) );
        } else if (tag.equals("vertex")) {
            vId = Integer.parseInt( attribs.getValue("id") );
            vX = Double.parseDouble( attribs.getValue("x") );
            vY = Double.parseDouble( attribs.getValue("y") );

            vLinks = new LinkedList<TmpLink>();
        } else if (tag.equals("link")) {
            int from = Integer.parseInt( attribs.getValue("from") );
            int to = Integer.parseInt( attribs.getValue("to") );
            int type = attribs.getValue("type").charAt(0);
            vLinks.add( new TmpLink(from, to, type) );
        }
    }
    
    @Override public void endElement (String junk1, String junk2, String tag) {
        if (tag.equals("road")) {
            // copy the rd points
        	Road r = new Road(rdId, rdName, rdType, new LinkedList<Coordinate>(rdPts), rdOsm);
        	r.setGraph(g);
            g.getRoads().set(rdId, r);
            rdV1s[rdId] = rdV1;
            rdV2s[rdId] = rdV2;

            rdName = rdType = null;
            rdOsm = rdId = rdV1 = rdV2 = -1;
            rdPts.clear();
        } else if (tag.equals("edge")) {
            Edge e = new Edge(eId, eDir == '+' ? Road.DIR.POS : Road.DIR.NEG);
            e.laneNum = eLane;
            g.getEdges().set(eId, e);
            e.lines = new LinkedList<Edge.Line>(eLines);    // copy
            eRds[eId] = eRd;

            eId = eRd = eLane = eDir = -1;
            eLines.clear();
        } else if (tag.equals("vertex")) {
            g.getVertices().set(vId, new Vertex(vId, new Coordinate(vX, vY)));
            vertLinkLs[vId] = new LinkedList<TmpLink>(vLinks);   // copy it

            vId = -1;
            vX = vY = -1;
            vLinks.clear();
        }
    }

    @Override public void characters (char ch[], int start, int length) {}
}
