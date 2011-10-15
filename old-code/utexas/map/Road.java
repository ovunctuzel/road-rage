/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import static utexas.helper.Log.dbug;

// Represents a group of edges (lanes). Store some data once.
public class Road {
    // orientations for lanes. arbitrary, but consistent.
    public enum DIR { POS, NEG };   // pos = v1 -> v2, neg = v2 -> v1
    public enum TYPE { MOTORWAY, MOTORWAY_LINK, TRUNK, TRUNK_LINK, PRIMARY, PRIMARY_LINK, SECONDARY,
    	SECONDARY_LINK, TERTIARY, RESIDENTIAL, UNCLASSIFIED, ROAD, LIVING_STREET, SERVICE, TRACK,
    	RACEWAY, SERVICES,
    	//These are not listed officially, but they get used apparently...
    	TERTIARY_LINK, NULL,
        // TODO this is a temp one used for testing
        DOOMED
    };

    private String name = null;
    private TYPE type = null;
    private int osmID;
    private List<Coordinate> points;    // osm waypoints
    private List<Edge> posLanes = new LinkedList<Edge>();
    private List<Edge> negLanes = new LinkedList<Edge>();
    public Vertex v1, v2;   // also, v1 = points.0 and v2 = points.last
    public int id;
    
    private Graph g; //We need a reference to the parent graph

    // pts[0] and from, pts[-1] and to should match. they will by construction; trust something
    // in map.make.* to handle it.
    public Road (int ourId, String roadName, String roadType, List<Coordinate> pts, int origID) {
        id = ourId;
        name = roadName;
        type = TYPE.valueOf(roadType.toUpperCase());
        points = pts;
        osmID = origID;
    }

    public String toString() {
        return name + "[" + osmID + "] (" + id + ")";
    }

    public String getName () {
        return name;
    }

    public String getType () {
        return type.toString().toLowerCase();
    }

    public int getOsmId () {
        return osmID;
    }

    public List<Coordinate> getPoints () {
        return points;
    }

    public List<Edge> getPosLanes () {
        return posLanes;
    }

    public List<Edge> getNegLanes () {
        return negLanes;
    }
    
    public Graph getGraph(){
    	return g;
    }
    
    public void setGraph(Graph g){
    	this.g = g;
    }

    public int hashCode () {
        return id;
    }

    public boolean equals (Object rhs) {
        if (rhs == null || getClass() != rhs.getClass()) {
            return false;
        } else {
            Road r2 = (Road) rhs;
            return r2.id == id;
        }
    }

    // according to OSM data, at least.
    public boolean sameRoad (Road other) {
        return osmID == other.osmID;
    }

    // Assumes e is in posLanes or negLanes
    public Vertex getFromV (Edge e) {
        if (e.direxn == DIR.POS) {
            return v1;
        } else {
            return v2;
        }
    }

    public Vertex getToV (Edge e) {
        if (e.direxn == DIR.POS) {
            return v2;
        } else {
            return v1;
        }
    }

    // How many total?
    public int numLanes () {
        return negLanes.size() + posLanes.size();
    }

    public Set<Road> adjacentRoads () {
        Set<Road> ls = v1.getRoads();
        ls.addAll( v2.getRoads() );
        return ls;
    }
    
    public double getSpeedLimit(){
    	int mph = getSpeedLimitMPH();
        return mph * 0.44704085774623461111111111111111;    // mph -> m/s
    }
    private int getSpeedLimitMPH(){
    	switch (type){
    	case MOTORWAY: return 80;
    	case MOTORWAY_LINK: return 35;
    	case TRUNK: return 70;
    	case TRUNK_LINK: return 35;
    	case PRIMARY: return 65;
    	case PRIMARY_LINK: return 35;
    	case SECONDARY: return 55;
    	case SECONDARY_LINK: return 35;
    	case TERTIARY: return 45;
    	case RESIDENTIAL: return 30;
    	case UNCLASSIFIED: return 40;
    	case ROAD: return 40;
    	case LIVING_STREET: return 20;
    	case SERVICE: return 10; //This is apparently parking-lots basically, not feeder roads
    	case TRACK: return 35;
    	case RACEWAY: return 250; //I feel the need.  The need for speed.
    	case SERVICES: return 10;
    	
    	case TERTIARY_LINK: return 35;
    	case NULL: return 30;
    	
    	default: return 30;  //Generally a safe speed, right?
    	}
    }

    public boolean isOneWay () {
        return posLanes.size() == 0 || negLanes.size() == 0;
    }

    public List<Edge> getOneWayLanes () {
        return posLanes.size() == 0 ? negLanes : posLanes;
    }
}
