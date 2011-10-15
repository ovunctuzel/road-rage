/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;

import static utexas.helper.Log.dbug;

import java.util.LinkedList;
import java.util.List;

import utexas.map.Vertex.TURN_TYPE;

public class Turn {
    public Edge from, to;
    public Vertex.TURN_TYPE type;

    public Turn (Edge e1, Vertex.TURN_TYPE t, Edge e2) {
        from = e1;
        type = t;
        to = e2;
    }
    
    public String toString () {
        return "TURN: " + from + " -> " + type + " -> " + to;
    }

    public boolean equals (Object other) {
        if (!(other instanceof Turn)) {
            return false;
        }
        Turn t = (Turn) other;
        if (from == null || to == null) {
            dbug("OY!!! we're a null turn: " + toString());
            return false;
        }
        if (t.from == null || t.to == null) {
            dbug("OY!!! they're a null turn: " + t.toString());
            return false;
        }
        return from.id == t.from.id && to.id == t.to.id && type == t.type;
    }

    // Here be some dragons. Pictures are good proof, right?
    // TODO a set, since our rightmost might equal our leftmost
    public List<Turn> getConflicts () {
        List<Turn> wrecks = new LinkedList<Turn>(); // Wrecks we're going to avoid, of course.
        try {
            // TODO we want a general way of getting the "near" and "far" cross streets.
            switch (type) {
                case CROSS: {
                    // First ignore which lane we are...
                    // Sorry, the names are from my diagram..
                    Edge tmp_l = from.getLeftmost().getDest(TURN_TYPE.LEFT);
                    if (tmp_l != null) {
                        Edge E_l = tmp_l.getSource(TURN_TYPE.CROSS);
                        if (E_l != null) {
                            wrecks.add( new Turn(E_l, TURN_TYPE.CROSS, E_l.getDest(TURN_TYPE.CROSS)) );
                            wrecks.add( new Turn(E_l, TURN_TYPE.LEFT, E_l.getDest(TURN_TYPE.LEFT)) );
                            Edge E_r = E_l.getRightmost();
                            if (E_r != null && !E_r.equals(E_l)) {
                                wrecks.add( new Turn(E_r, TURN_TYPE.CROSS, E_r.getDest(TURN_TYPE.CROSS)) );
                                if (from.isRightmost()) {
                                    // if we're the rightmost...
                                    wrecks.add( new Turn(E_r, TURN_TYPE.RIGHT, E_r.getDest(TURN_TYPE.RIGHT)) );
                                }
                            }
                        }

                        if (!tmp_l.getRoad().isOneWay()) {
                            Edge W_l = tmp_l.getOtherLanes().get(0).getLeftmost();
                            Edge W_r = tmp_l.getOtherLanes().get(0).getRightmost();
                            wrecks.add( new Turn(W_r, TURN_TYPE.CROSS, W_r.getDest(TURN_TYPE.CROSS)) );

                            if (!W_l.equals(W_r)) {
                                wrecks.add( new Turn(W_l, TURN_TYPE.CROSS, W_l.getDest(TURN_TYPE.CROSS)) );
                            }
                            if (from.isLeftmost()) {
                                // if we're the leftmost...
                                wrecks.add( new Turn(W_l, TURN_TYPE.LEFT, W_l.getDest(TURN_TYPE.LEFT)) );
                            }
                        }
                    }

                    Edge other_side = from.getDest(TURN_TYPE.CROSS);
                    if (other_side != null && !other_side.getRoad().isOneWay()) {
                        Edge N_l = other_side.getOtherLanes().get(0).getLeftmost();
                        Edge N_l_to = N_l.getDest(TURN_TYPE.LEFT);
                        if (N_l_to != null) {
                            wrecks.add( new Turn(N_l, TURN_TYPE.LEFT, N_l_to) );
                        }
                    }
                    break;
                }

                case LEFT: {
                    wrecks.add( new Turn(to.getSource(TURN_TYPE.CROSS), TURN_TYPE.LEFT, to) );
                    // get the cross street close to us...
                    Edge crossed_right = from.getRightmost().getDest(TURN_TYPE.RIGHT);
                    if (crossed_right != null) {
                        Edge cross_right = crossed_right.getSource(TURN_TYPE.CROSS);
                        if (cross_right != null) {
                            wrecks.add( new Turn(cross_right, TURN_TYPE.CROSS, crossed_right) );

                            Edge crossed_left = crossed_right.getLeftmost();
                            Edge cross_left = cross_right.getLeftmost();
                            wrecks.add( new Turn(cross_left, TURN_TYPE.CROSS, crossed_left) );
                            wrecks.add( new Turn(cross_left, TURN_TYPE.LEFT, cross_left.getDest(TURN_TYPE.LEFT)) );
                        }
                    }

                    // and the opposite...
                    // this is only SLIGHTLY convoluted...
                    Edge opposite = to.getRightmost().getSource(TURN_TYPE.RIGHT);
                    if (opposite != null) {
                        // all the straight turns here...
                        for (Edge e : opposite.getLanes()) {
                            wrecks.add( new Turn(e, TURN_TYPE.CROSS, e.getDest(TURN_TYPE.CROSS)) );
                        }
                    }
                    break;
                }

                case RIGHT: {
                    wrecks.add( new Turn(to.getSource(TURN_TYPE.CROSS), TURN_TYPE.CROSS, to) );
                    break;
                }
            }
        } catch (Exception e) { // null pointer or out of bounds index, usually
            // TODO we messed up somewhere.
            dbug("WARNING: turn conflicts botched up somewhere near %s", to);
            dbug("  " + e);
            e.printStackTrace();
            wrecks.clear();
        }

        List<Turn> wrecksReturn = new LinkedList<Turn>();
        // Filter out null turns; sometimes we don't get a hit for a certain turn type
        // leading into us

        // no in-place remove? :(
        for (Turn ouch : wrecks) {
            if (ouch.from != null && ouch.to != null) {
                wrecksReturn.add(ouch);
            }
        }

        return wrecksReturn;
    }

    public List<Edge.Line> getLine () {
        Coordinate ptA = from.lines.get( from.lines.size() - 1 ).endPt();
        Coordinate ptB = to.lines.get(0).startPt();
        List<Edge.Line> lines = new LinkedList<Edge.Line>();
        lines.add( new Edge.Line(ptA.x, ptA.y, ptB.x, ptB.y) );
        return lines;
    }

    public double length (Graph g) {
        return getLine().get(0).length(g);
    }

    public double timeToCross (Graph g, double veloc) {
        // make sure veloc > 0

        // TODO this'll change later based on type
        // TODO assumes we're gonna be going the 'current' velocity for the turn.. is this
        // correct?
        double dur = getLine().get(0).length(g) / veloc;
        // handle dur < 0
        if (dur == 0) {
            // TODO yay for right turns. not sure what the right thing to do is.
            dur = 0.000001;
        }
        return dur;
    }
}
