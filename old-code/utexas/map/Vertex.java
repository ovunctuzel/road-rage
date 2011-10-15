/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import utexas.helper.MultiMap;
import utexas.helper.Pair;
import utexas.sim.Timetable;
import utexas.sim.Timetable.SchedulingConflictException;

// Represents an intersection and maps edges/lanes to other edges/lanes.
public class Vertex {
    public static enum TURN_TYPE {
        CROSS ('C'), CROSS_MERGE ('M'), LEFT ('L'), RIGHT ('R'), UTURN ('U');

        private final char symbol;
        TURN_TYPE (char code) {
            symbol = code;
        }

        public char code () {
            return symbol;
        }
    };

    // I think geometrically location is the center of the intersection, but not sure. it's
    // whatever osm gives us.
    public Coordinate location;
    public int id;
    // a multimap from an incoming edge to all the other edges it leads to, tagged with the
    // type of turn/move
    private MultiMap<Edge, Pair<TURN_TYPE, Edge>> map = new MultiMap<Edge, Pair<TURN_TYPE, Edge>>();
    private Timetable timetable = new Timetable();

    public Vertex (int ourId, Coordinate where) {
        id = ourId;
        location = where;
    }

    public String toString () {
        return "Vert " + id;
    }

    public int hashCode () {
        return id;
    }

    public boolean equals (Object rhs) {
        if (rhs == null || getClass() != rhs.getClass()) {
            return false;
        } else {
            Vertex v2 = (Vertex) rhs;
            return v2.id == id;
        }
    }

    public double distTo (Vertex other) {
        return location.distTo(other.location);
    }

    // where's this one lead? maybe nowhere if we're a dead-end.
    public List<Turn> nextEdges (Edge from) {
        List< Pair<TURN_TYPE, Edge>> ls = map.get(from);
        List<Turn> turns = new LinkedList<Turn>();
        if (ls != null) {
            for (Pair<TURN_TYPE, Edge> turn : ls) {
                turns.add( new Turn(from, turn.get1(), turn.get2()) );
            }
        }
        return turns;
    }

    // what leads to this?
    @SuppressWarnings("unused")
	public List<Turn> prevEdges (Edge seek) {
        List<Turn> turns = new LinkedList<Turn>();
        FROM: for (Edge from : map.keySet()) {
            TO: for (Pair<TURN_TYPE, Edge> to : map.get(from)) {
                if (to.get2().equals(seek)) {
                    turns.add( new Turn(from, to.get1(), seek) );
                    // two turns from the same edge dont both lead to the same place
                    break TO;
                }
            }
        }
        return turns;
    }

    public Set<Edge> allEdges () {
        Set<Edge> orig = map.keySet();
        Set<Edge> set = new HashSet<Edge>(orig);    // copy it
        for (Edge from : orig) {
            for (Pair<TURN_TYPE, Edge> to : map.get(from)) {
                set.add( to.get2() );
            }
        }
        return set;
    }

    public Set<Edge> incomingEdges () {
        return map.keySet();
    }

    public Set<Edge> outgoingEdges () {
        Set<Edge> out = new HashSet<Edge>();
        for (Edge from : map.keySet()) {
            for (Pair<TURN_TYPE, Edge> to : map.get(from)) {
                out.add(to.get2());
            }
        }
        return out;
    }

    public MultiMap<Edge, Pair<TURN_TYPE, Edge>> getMap () {
        return map;
    }

    // Pick an outgoing edge on the road left/across/right of from
    public Edge left_from (Edge from) {
        for (Turn turn : nextEdges(from.getLeftmost())) {
            if (turn.type == Vertex.TURN_TYPE.LEFT) {
                return turn.to;
            }
        }
        return null;
    }

    public Edge across_from (Edge from) {
        for (Turn turn : nextEdges(from)) {
            if (turn.type == Vertex.TURN_TYPE.CROSS) {
                return turn.to;
            }
        }
        return null;
    }

    public Edge right_from (Edge from) {
        for (Turn turn : nextEdges(from.getRightmost())) {
            if (turn.type == Vertex.TURN_TYPE.RIGHT) {
                return turn.to;
            }
        }
        return null;
    }

    // What roads do we connect?
    public Set<Road> getRoads () {
        Set<Road> roads = new HashSet<Road>();
        for (Edge e : allEdges()) {
            roads.add(e.road);
        }
        return roads;
    }

    // Just delegate to our timetables
    public double request (double arrival, double duration, Turn turn) {
        return timetable.request(arrival, duration, turn);
    }

    public void schedule (double arrival, double duration, Turn turn) throws SchedulingConflictException {
        timetable.schedule(arrival, duration, turn);
    }
}
