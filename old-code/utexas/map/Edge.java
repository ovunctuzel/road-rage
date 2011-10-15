/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;

import static utexas.helper.Log.dbug;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import utexas.helper.Util;
import utexas.sim.Agent;

// Represents a single directed lane.
public class Edge {
    // Geometrically, consists of one or more directed lines
    public static class Line {  // static just means can be poked outside an edge instance
        public double x1, y1, x2, y2;

        public Line (double X1, double Y1, double X2, double Y2) {
            x1 = X1;
            y1 = Y1;
            x2 = X2;
            y2 = Y2;
        }

        public double angle () {
            // recall y inversion
            return Util.atan2(x2 - x1, y1 - y2);
        }

        //public double slope () {
        //    return (y2 - y1) / (x2 - x1);
        //}

        public Coordinate midPt () {
            return new Coordinate((x1 + x2) / 2, (y1 + y2) / 2);
        }

        public String toString () {
            return x1 + ", " + y1 + " ---> " + x2 + ", " + y2;
        }

        // Just between endpoints. in meters
        public double length (Graph g) {
            Coordinate c1 = g.worldToGPS(new Coordinate(x1,y1));
            Coordinate c2 = g.worldToGPS(new Coordinate(x2,y2));
            double len = Coordinate.getDistanceInMeters(c1, c2);
            if (Double.isNaN(len)) {
                //dbug("nan length. btwn %s and %s", new Coordinate(x1,y1), new Coordinate(x2,y2));
                return 0;
            } else {
                return len;
            }
        }

        private final static double epsilon = 0.00001;
        // Note: this is line intersection, not line segment. our usage right now possibly
        // invokes us after modifying us once...
        public Coordinate intersection (Line with) {
            double x3 = with.x1, x4 = with.x2, y3 = with.y1, y4 = with.y2;
            // Ripped from http://en.wikipedia.org/wiki/Line-line_intersection shamelessly
            double denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
            double numX = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4);
            double numY = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4);

            if (Math.abs(denom) <= epsilon) {
                // They're parallel.
                return null;
            }
            
            double x = numX / denom;
            double y = numY / denom;
            return new Coordinate(x, y);
        }

        public Coordinate startPt () {
            return new Coordinate(x1, y1);
        }

        public Coordinate endPt () {
            return new Coordinate(x2, y2);
        }

        // Only set if the given intersection point is on the current line segment. Only
        // shorten the line seg, in other words.
        public void adjustStart (Coordinate to) {
            if (containsPt(to)) {
                x1 = to.x;
                y1 = to.y;
            }
        }

        public void adjustEnd (Coordinate to) {
            if (containsPt(to)) {
                x2 = to.x;
                y2 = to.y;
            }
        }

        // line segment
        private boolean containsPt (Coordinate pt) {
            // find the slope btwn pt and x1,y1 and see if thats consistent...
            // thats just for a line though >_<
            double slope1 = (y2 - y1) / (x2 - x1);
            double slope2 = (y1 - pt.y) / (x1 - pt.x);

            if ((!(Double.isInfinite(slope1) && Double.isInfinite(slope2))) &&
                 Math.abs(slope1 - slope2) > epsilon)
            {
                return false;
            }

            double smallX = Math.min(x1, x2);
            double bigX   = Math.max(x1, x2);
            double smallY = Math.min(y1, y2);
            double bigY   = Math.max(y1, y2);

            // so now just make sure we're within the rectangle defined by our line points
            return pt.x >= smallX && pt.x <= bigX && pt.y >= smallY && pt.y <= bigY;
        }
    }

    public int id;
    public Road road;
    public Road.DIR direxn; // an arbitrary orientation, don't worry about it
    public int laneNum; // convention: 0 = rightmost lane
    public List<Line> lines = new LinkedList<Line>();   // physically, the road segments
    
    private LinkedList<Agent> agents; //Fancy Queue.  The beginning of the Queue is the beginning of the edge.

    public Edge (int ourId, Road.DIR dir) {
        id = ourId;
        direxn = dir;
        agents = new LinkedList<Agent>();
    }
    
    public Road getRoad(){
    	return road;
    }
    
    public LinkedList<Agent> getAgents(){
    	return agents;
    }
    /**
     * Use an Agent's progress to add it to an Edge at the correct point.
     * @param a The Agent to add
     * @throws IllegalStateException if the given Agent has a progress beyond the length of this edge
     * @return Position in the queue. Use to see if we're passing someone.
     */
    public int insertAgent(Agent a) {
        double us = a.getProgress();

        if (us < 0) {
            // this guy's trying to pick a place to spawn, ignore him.
            return -1;
        }

        // TODO should not happen anymore. we handle leftover progress and don't call this
        // till we get our final new edge.
        if (us > length()) {
            //throw new IllegalStateException("Tried to add an Agent with progress "+us+" to "+toString());
            dbug("Agent %d going to %s (length %f) with progress %f...", a.id, toString(), length(), us);
            //a.killMe = true;

            return -1;
        }

        for (int i = 0; i < agents.size(); i++) {
            if (us > agents.get(i).getProgress()) {
                agents.add(i, a);   // we're before them
                return i;
            }
        }
        agents.addLast(a); //If we're not before anyone, then we're at the end.
        return agents.size() - 1;
    }

    // returns where we used to be
    public int removeAgent(Agent a){
        // Sometimes we won't be in there, such as if we were added with a progress beyond
        // the length of the edge since the edge was short and we were fast and had lots of
        // leftover progress.
        int pos = agents.indexOf(a);
        if (pos == -1) {
            dbug("removing when we arent there");
            return -1;
        } else {
            agents.remove(pos);
            return pos;
        }
    }
    public Agent getNthAgent(int n){
    	if (n < 0 || n >= agents.size()) return null;
    	return agents.get(n);
    }
    public Agent getFrontAgent(){
    	if (agents.isEmpty()) return null;
    	return agents.getFirst();
    }
    public Agent getLastAgent(){
    	if (agents.isEmpty()) return null;
    	return agents.getLast();
    }
    /**
     * Get the closest Agent to a particular point on the Edge.
     * @param progress The distance from the beginning of the edge
     * @return The nearest Agent
     */
    public Agent getAgentNearestTo(double progress){
        //dbug("%d agents on the Q", agents.size());
    	if (agents.isEmpty()) return null;
    	Iterator<Agent> it = agents.iterator();
    	Agent next = null;
    	double dist = Math.abs((next = it.next()).getProgress() - progress);
    	Agent prev = next;
    	while (it.hasNext() && Math.abs((next = it.next()).getProgress()-progress)<dist){
    		dist = Math.abs(next.getProgress()-progress);
    		prev = next;
    	}
    	return prev;
    }
    /**
     * Get the closest Agent strictly beyond a particular point on the Edge
     * @param progress The edge-relative distance to search beyond
     * @return The nearest Agent
     */
    public Agent getNearestAgentAfter(double progress){
    	if (agents.isEmpty()) {
            return null;
        }
    	Iterator<Agent> it = agents.iterator();

    	double dist = 0;
    	Agent next = null;
        // Find first valid agent
    	while (it.hasNext() && dist <= 0) {
            next = it.next();
            dist = next.getProgress() - progress;
        }

        return dist <= 0 ? null : next;
    }
    /**
     * Get the closest Agent strictly before a particular point on the Edge
     * @param progress The edge-relative distance to search before
     * @return The nearest Agent
     */
    public Agent getNearestAgentBefore(double progress){
    	if (agents.isEmpty()) return null;
    	Iterator<Agent> it = agents.iterator();
    	Agent next = null;
    	if (progress - (next = it.next()).getProgress() <= 0) return null;
    	Agent prev = null;
    	while (it.hasNext() && progress - (next = it.next()).getProgress() < 0) prev = next;
    	return prev;
    }
    /**
     * Get the Agent exactly at a particular point on the Edge
     * @param progress The edge-relative distance to search at
     * @return The Agent with the given progress or null if there is none
     */
    public Agent getAgentAt(double progress){
    	if (agents.isEmpty()) return null;
    	for (Agent a : agents){
    		double prog;
    		if ((prog = a.getProgress()) == progress) return a;
    		else if (prog > progress) return null; //Early break if we find an Agent beyond progress next (since they're in order)
    	}
    	return null;
    }
    /**
     * Get the closest Agent at or before a particular point on the Edge.  This is inclusive of the point.
     * @see #getNearestAgentBefore(double) for an exclusive version.
     * @param progress The edge-relative distance to search before
     * @return The nearest Agent
     */
    public Agent getAgentBeforeOrAt(double progress){
    	Agent a = getAgentAt(progress);
    	return (a == null) ? getNearestAgentBefore(progress) : a;
    }
    /**
     * Get the closest Agent at or after a particular point on the Edge.  This is exclusive of the point.
     * @see #getNearestAgentAfter(double) for an exclusive version
     * @param progress The edge-relative distance to search after
     * @return The nearest Agent
     */
    public Agent getAgentAfterOrAt(double progress){
    	Agent a = getAgentAt(progress);
    	return (a == null) ? getNearestAgentAfter(progress) : a;
    }
    public void describeQueue(){
        for (Agent a : agents) {
            dbug("%s at progress %f", a.toString(), a.getProgress());
        }
    }

    public Vertex from () {
        return road.getFromV(this);
    }

    public Vertex to () {
        return road.getToV(this);
    }

    public int hashCode () {
        return id;
    }

    public boolean equals (Object rhs) {
        if (rhs == null || getClass() != rhs.getClass()) {
            return false;
        } else {
            Edge e2 = (Edge) rhs;
            return e2.id == id;
        }
    }

    // assumes we involve the one passed in...
    public Vertex other (Vertex notMe) {
        return to().id == notMe.id ? from() : to();
    }

    // where can we go from here?
    public List<Turn> getDestinations () {
        return to().nextEdges(this);
    }

    public List<Turn> getSources () {
        return from().prevEdges(this);
    }

    // include lane changes.
    public List<Edge> getAdjacentEdges () {
        List<Edge> next = new LinkedList<Edge>();
        Edge left = getLeft();
        Edge right = getRight();
        if (left != null) {
            next.add(left);
        }
        if (right != null) {
            next.add(right);
        }
        
        // normal intersections
        for (Turn nexts : getDestinations()) {
            next.add( nexts.to );
        }
        return next;
    }

    // What edge leads to us with the specified turn?
    // TODO there might be multiple!
    public Edge getSource (Vertex.TURN_TYPE match) {
        for (Turn turn : getSources()) {
            if (turn.type == match) {
                // TODO from or to? hrmm.
                return turn.from;
            }
        }
        return null;
    }

    public Edge getDest (Vertex.TURN_TYPE match) {
        for (Turn turn : getDestinations()) {
            if (turn.type == match) {
                return turn.to;
            }
        }
        return null;
    }

    // Other lanes
    public Edge getLeft () {
        try {
            return getLanes().get(laneNum + 1);
        } catch (IndexOutOfBoundsException e) {
            return null;
        }
    }

    public Edge getRight () {
        try {
            return getLanes().get(laneNum - 1);
        } catch (IndexOutOfBoundsException e) {
            return null;
        }
    }

    public boolean isLeftmost () {
        return getLeft() == null;
    }

    public boolean isRightmost () {
        return getRight() == null;
    }

    public Edge getLeftmost () {
        List<Edge> lanes = getLanes();
        return lanes.get( lanes.size() - 1);
    }

    public Edge getRightmost () {
        return getLanes().get(0);
    }

    // all of them
    public List<Edge> getLanes () {
        if (direxn == Road.DIR.POS) {
            return road.getPosLanes();
        } else {
            return road.getNegLanes();
        }
    }

    // all of them in the opposite direction
    public List<Edge> getOtherLanes () {
        if (direxn == Road.DIR.POS) {
            return road.getNegLanes();
        } else {
            return road.getPosLanes();
        }
    }

    // How many total for our direction?
    public int numLanes () {
        return getLanes().size();
    }

    // Choose a representative for debugging
    public boolean isCanonical () {
        return laneNum == 0 && direxn == Road.DIR.POS;
    }

    public String toString() {
        char sign = direxn == Road.DIR.POS ? '+' : '-';
        return "Lane " + sign + laneNum + " of " + road.getName() + " (" + id + ")";
    }

    public int getLaneOffset () {
        if (road.isOneWay()) {
            // The rightmost lane actually becomes the center line
            return numLanes() - laneNum - 1;
        } else {
            return numLanes() - laneNum;
        }
    }

    /////// Geometry. Just the few runtime things.

    // of the lane in the correct direction. shifted back.
    public Coordinate getStart () {
        Line l = getFirstVector();
        return shiftedPoint(l.x1, l.y1, l.angle());
    }

    public Coordinate getEnd () {
        Line l = getLastVector();
        return shiftedPoint(l.x2, l.y2, l.angle() + Math.PI); // reverse
    }

    // not shifted back.
    public Coordinate getTrueStart () {
        Line l = getFirstVector();
        return new Coordinate(l.x1, l.y1);
    }

    public Coordinate getTrueEnd () {
        Line l = getLastVector();
        return new Coordinate(l.x2, l.y2);
    }

    // Now the funky backing up. Just for UI turn drawing!
    private Coordinate shiftedPoint (double x, double y, double theta) {
        double mag = 1.0;   // how much to back up?
        double backX = mag * Math.cos(theta);
        double backY = -mag * Math.sin(theta);  // y inversion

        return new Coordinate(x + backX, y + backY);
    }

    // first line segment
    public Line getFirstVector () {
        return lines.get(0);
    }
   
    // last line segment
    public Line getLastVector () {
        return lines.get(lines.size() - 1);
    }

    // How long are we?
    public double length () {
        double sum = 0;
        for (Line l : lines) {
            sum += l.length(road.getGraph());
        }
        return sum;
    }

    // to spawn/laneshift
    public double minSpawnProgress() {
        return length() * Util.cfg_double("laneshift-min");
    }

    public double maxSpawnProgress() {
        return length() * Util.cfg_double("laneshift-max");
    }

    public double randomProgress () {
        return Util.rndDouble(minSpawnProgress(), maxSpawnProgress());
    }
}
