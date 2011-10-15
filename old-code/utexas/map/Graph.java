/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map;

import static utexas.helper.Log.dbug;
import static utexas.helper.Log.pop;
import static utexas.helper.Log.push;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import org.xml.sax.InputSource;

import utexas.helper.Pair;
import utexas.helper.Util;
import utexas.map.make.Reader;
import utexas.sim.Agent;
import utexas.sim.Timetable.SchedulingConflictException;

// The final form of the map.
public class Graph {
    /*
    public class Table {
        private Path[][] tbl;

        Table (Graph g) {
            int sz = g.getEdges().size();
            tbl = new Path[sz][sz];
        }

        public void add (Edge from, Edge to, List<Edge> path) {
            tbl[from.id][to.id] = new Path(path);
        }
    }
    */

    public static class Path {
        int[] edges;

        public Path (List<Edge> path) {
            edges = new int[path.size()];
            for (int i = 0; i < path.size(); i++) {
                edges[i] = path.get(i).id;
            }
        }
    }

    protected List<Road> roads;         // road = collection of edges
    protected List<Edge> edges;         // edge = directed lane
    protected List<Vertex> vertices;    // vertex = intersection
    //protected Table path_table = null;  // pre-computed routes.
    protected double width, height, xOff, yOff, scale;

    /////////////// Construction

    public Graph (double w, double h, double xoff, double yoff, double sc, int rSize, int eSize, int vSize) {
        width = w;
        height = h;
        xOff = xoff;
        yOff = yoff;
        scale = sc;

        roads = new ArrayList<Road>(rSize);
        Util.inflateArray(roads, rSize); //Does this actually do anything?  It's all nulls to start with anyway in Java

        edges = new ArrayList<Edge>(eSize);
        Util.inflateArray(edges, eSize);

        vertices =  new ArrayList<Vertex>(vSize);
        Util.inflateArray(vertices, vSize);

        // Note: I don't think we construct the table here, since Pass3 starts us off with
        // 0 for all the sizes...
    }
    
    public static Graph load (InputSource isrc) {
        return new Reader().load(isrc);
    }

    public static Graph load (String fn) {
        return (new Reader()).load(fn);
    }

    /////////////// Accessors

    public double getWidth () {
        return width;
    }

    public double getHeight () {
        return height;
    }

    public List<Road> getRoads () {
        return roads;
    }

    public List<Edge> getEdges () {
        return edges;
    }

    public List<Vertex> getVertices () {
        return vertices;
    }

    public Vertex getVertex (int id) {
        return vertices.get(id);
    }

    /*
    public Table getTable () {
        if (path_table != null) {
            return path_table;
        } else {
            path_table = new Table(this);
            return path_table;
        }
    }
    */

    public Edge getRandomEdge (Edge exceptMe) {
        while (true) {
            Edge e = Util.chooseRnd(edges);
            // don't spawn on highways or on nonexistantly small roads
            if (e.getRoad().getType().equals("residential") && e != exceptMe && e.length() >= 1.0) {
                return e;
            }
        }
    }

    public Coordinate worldToGPS (Coordinate pt) {
        return new Coordinate(
            (pt.x / scale) - xOff,
            ((height - pt.y) / scale) - yOff
        );
    }

    public Coordinate gpsToWorld (Coordinate pt) {
        return new Coordinate((pt.x + xOff) * scale, (pt.y + yOff) * scale);
    }

    /////////////// Simple pathfinding

    // Used internally when planning routes, and in post-proc too I guess
    public static abstract class WeightedStep implements Comparable<WeightedStep> {
        // these are general now, but still shared
        Double pri;     // when we start this + heuristic till reaching goal. sort by this.
        Double at;      // exact time when we start at the beginning of turn.to

        public int compareTo (WeightedStep o) {
            return pri.compareTo(o.pri);
        }

        public double when () {
            return at;
        }

        public abstract Edge getSource();
        public abstract Edge getDest();
        public abstract boolean isFirstStep();
        public abstract Pair<Double, Double> calcTime(double p); // Returns timeToTraverse and curVeloc
    }

    public static class WeightedTurnStep extends WeightedStep {
        Turn turn;
        Double turnAt;  // when do we start the turn?
		Double curVeloc; // how fast are we going when we start this turn

        WeightedTurnStep (Turn t, double turnTime, double arrival, double veloc, double priority) {
            turn = t;
            turnAt = turnTime;
            at = arrival;
            pri = priority;
			curVeloc = veloc;
        }

        public String toString () {
            return "Step at " + at + "s (Priority " + pri + "s): " + turn;
        }

        @Override public Edge getSource () {
            return turn.from;
        }

        @Override public Edge getDest () {
            return turn.to;
        }

        @Override public boolean isFirstStep () {
            return turn.from == null;
        }

        // Returns timeToTraverse and curVeloc
        @Override public Pair<Double, Double> calcTime (double progress) {
            /*
             * Physics time.  Yay Newton!
             * xf = xi + vi(t) + 1/2at^2
			 * vf = vi + at --> t1
			 * xf = xi + vi(t1) + 1/2a(t1)^2 --> xmid
			 * xf = xmid + vf(t) --> t2
			 * return vf, t1+t2 unless t2 is negative.  Then use quadratic formula
             */
            //Inputs: Agent.STD_ACCELERATION, curVel (initial velocity), dist (xf-xi), speed limit (vf)
            Edge edge = turn.to;
            double curVel = curVeloc;
            double dist = edge.length() - progress;
            double t1 = (edge.getRoad().getSpeedLimit() - curVel) / Agent.STD_ACCELERATION;
            double xmid = curVel*t1 + (1./2.)*Agent.STD_ACCELERATION*t1*t1;
            double t2 = (dist-xmid)/edge.getRoad().getSpeedLimit();
            t1 = (t2 > 0) ? t1 : (Math.sqrt(curVel*curVel - 2*(-1)*dist*Agent.STD_ACCELERATION)-curVel)/Agent.STD_ACCELERATION;
            curVel = (t2 > 0) ? edge.getRoad().getSpeedLimit() : curVel + Agent.STD_ACCELERATION*t1; //Update velocity

            // make sure curVel aint 0
            return new Pair<Double, Double>((t2 > 0) ? t1 + t2 : t1, curVel);
        }
    }

    // for lane changing
    public static class WeightedSideStep extends WeightedStep {
        Edge from, to;  // of the same road
        Pair<Double, Double> physics;
        // we plan no timing at all. :)

        WeightedSideStep (Edge lane1, Edge lane2, double sofar, double p, Pair<Double, Double> cache) {
            from = lane1;
            to = lane2;
            at = sofar;
            pri = p;
            physics = cache;
        }

        @Override public Edge getSource () {
            return from;
        }

        @Override public Edge getDest () {
            return to;
        }

        @Override public boolean isFirstStep () {
            return from == null;
        }

        // we just steal it from the turnstep that lead to us?
        @Override public Pair<Double, Double> calcTime (double p) {
            return physics;
        }
    }

    // A slim version for experimental pathfinding
    public static class WeightedStaticStep extends WeightedStep {
        Edge edge;

        public WeightedStaticStep (Edge e, double sofar, double p) {
            edge = e;
            at = sofar;
            pri = p;
        }

        // TODO kind of meaningless...
        @Override public Edge getSource () {
            return edge;
        }

        @Override public Edge getDest () {
            return edge;
        }

        // Meaningless, we don't store that much structure!
        @Override public boolean isFirstStep () {
            dbug("OY! this shouldnt get called!");
            return false;
        }

        // Meaningless, we don't store that much structure!
        @Override public Pair<Double, Double> calcTime (double p) {
            dbug("OY! this shouldnt get called!");
            return null;
        }
    }

    // we return these to actually give commands on what to do
    public static enum Action { ACT_TRAVERSE, ACT_TURN, ACT_LANECHANGE };   // TODO speed mods too
    public static class Move {
        public Action act;
        public double start;    // not sure what this means for lanechange yet
        // one or the other of the below is stored
        public Edge edge;
        public Turn turn;

        Move (Action a, Edge e, double when) {
            act = a;
            edge = e;
            turn = null;
            start = when;
        }

        Move (Turn t, double when) {
            act = Action.ACT_TURN;
            edge = null;
            turn = t;
            start = when;
        }
    }

    // experimental naive method that might work since there dont seem to be local minima...
    public List<Edge> pathfind_naive (Edge from, Edge to) {
        Coordinate goal = to.getEnd();
        //start_timer("total-beeline");
        LinkedList<Edge> path = new LinkedList<Edge>();
        Set<Edge> visited = new HashSet<Edge>();
        
        Edge cur = from;
        boolean first = true;
        SEARCH: while (true) {
            path.add(cur);

            if (cur == to && !first) {    // if from=to, find a loop and dont sit still.
                // found path!
                break SEARCH;
            }
            first = false;

            double best = -1;
            
            for (Edge next : cur.getAdjacentEdges()) {
                double heuristic = next.getTrueStart().distTo(goal);
                if ((best == -1 || heuristic < best) && !visited.contains(next)) {
                    best = heuristic;
                    cur = next;
                }
            }
            if (best == -1) {
                dbug("ERR: couldnt reach goal.");
                return path;
            }
            visited.add(cur);
        }

        //stop_timer("total-beeline");
        //dumpTimes();
        return path;
    }

    public List<Edge> pathfind_astar (Edge from, Edge to) {
        // TODO so now an order to make a loop might just change lanes a few times... which
        // isn't wrong...
        boolean loop = from == to;

        Coordinate goal = to.getTrueEnd();
        //start_timer("total-astar");
        Map<Edge, WeightedStaticStep> visited = new HashMap<Edge, WeightedStaticStep>();
        PriorityQueue<WeightedStaticStep> open = new PriorityQueue<WeightedStaticStep>();
        LinkedList<Edge> path = new LinkedList<Edge>();

        WeightedStaticStep first_step = new WeightedStaticStep(from, 0, 0);
        open.add(first_step);
        visited.put(from, null);

        boolean first = true;
        SEARCH: while (open.peek() != null) {
            WeightedStaticStep cur = open.poll();
            Edge cur_e = cur.edge;
            if (cur_e == to && !first) {    // if from=to, find a loop and dont sit still.
                // found path!
                WeightedStaticStep at = cur;
                while (true) {
                    path.addFirst(at.edge);

                    // clean up visited as we go so that loops are broken
                    at = visited.remove(at.edge);
                    if (at == null) {
                        break SEARCH;
                    }
                }
            }
            first = false;

            // start at cur.turn.to and see where we have to go...
//            double dist = cur_e.length();
//            double timeToTraverse = dist / cur_e.getRoad().getSpeedLimit();

            for (Edge next : cur_e.getAdjacentEdges()) {
                if ((loop && next == from) || !visited.containsKey(next)) {
                    // TODO shorter route to e? is this possible?

                    // TODO there are better heuristics. currently euclid distance from
                    // next to goal.
                    double heuristic = next.getStart().distTo(goal);

                    // rinse and repeat...
                    open.add( new WeightedStaticStep(next, cur.at, cur.at + heuristic) );
                    visited.put(next, cur);
                }
            }
        }

        return path;
    }

    // This is A*
    // @return null when we had a conflict and want to be called again with the current
    // start_t
    public List<Move> astar (Edge from, double initProg, Edge to, double start_t) {
        // Don't loop. Sorry axel.
        if (from == to) {
            dbug("WARNING requesting a loop");
            return new LinkedList<Move>();
        }

        Coordinate goal = to.getTrueEnd();
        //start_timer("total-astar");
        // a map from an edge to the movement leading to it, which gives timing. dont worry
        // about the priority.
        Map<Edge, WeightedStep> visited = new HashMap<Edge, WeightedStep>();
        // which step are we considering next?
        PriorityQueue<WeightedStep> open = new PriorityQueue<WeightedStep>();
        // what are we eventually figuring out and returning?
        LinkedList<Move> path = new LinkedList<Move>();

        // the first is admitting you're wrong...
        // the funky null-filled turn indicates we magically start at from
        WeightedStep first_step = new WeightedTurnStep(new Turn(null, null, from), start_t, start_t, 0, start_t);
        open.add(first_step);
        visited.put(from, first_step);

        //dbug("starting to a* from " + from + " to " + to);

        boolean first = true;
        SEARCH: while (open.peek() != null) {
            WeightedStep cur = open.poll();
            if (cur.getDest() == to) {
                //dbug("found path! reconstructing.");
                // found path!
                //start_timer("build-path");

                // now transform this search into a set of moves between edges, and
                // schedule our intersection crossings.

                WeightedStep at = cur;

                while (true) {
                    if (at == null) {
                        dbug("whats going on here? sofar we've got " + path);
                        break SEARCH;
                    }

                    // schedule whatever we planned on doing, unless we're already there
                    if (!at.isFirstStep()) {
                        // TODO so do we schedule here, or let planRoute schedule?
                        // the .at time value already encodes when we'll start the turn
                    	try {
                            // don't have to schedule lane changes! woot
                            if (at instanceof WeightedTurnStep) {
                                WeightedTurnStep atStep = (WeightedTurnStep) at;
                                atStep.turn.from.to().schedule(atStep.turnAt, atStep.turn.timeToCross(this, atStep.curVeloc), atStep.turn);	// TODO pretty sure this step and not the prev's veloc
                            }
                    	}
                    	catch (SchedulingConflictException ex) {
                            // start over. null indicates we want a new start time.
                            dbug("timetable overwritten during a*! retrying...");
                            // TODO but now we have to cancel the reservations we've put
                            // in! aaargh. lock when we start doing this and buffer the
                            // requests.
                            return null;
                    	}
                    }

                    // where are we now? sorry about the crappy variable names
                    Edge here = at.getDest();

                    if (at instanceof WeightedSideStep) {
                        // Ah, a lane shift.
                        WeightedSideStep atStep = (WeightedSideStep) at;
                        // When we stick these in the search space, we assume we're
                        // starting on them at the same time we start on any other lane in
                        // the same edge. But when we try to meet the deadline in realtime,
                        // we want to get to the end of the edge by a common time,
                        // regardless of the lanechanges. So borrow that time from the next
                        // move, which should be a turn (or another lane change leading to
                        // a turn).
                        // TODO this breaks when our last move is a lane-change, but then,
                        // many things do.
                        if (path.size() > 0) {
                            path.addFirst(
                                new Move(Action.ACT_LANECHANGE, atStep.to, path.get(0).start)
                            );
                        } else {
                            dbug("WARNING: lanechange is last command, breakage likely");
                            path.addFirst( new Move(Action.ACT_LANECHANGE, atStep.to, atStep.at) );
                        }
                    } else {
                        path.addFirst( new Move(Action.ACT_TRAVERSE, here, at.at) );
                    }

                    if (at.isFirstStep()) {
                        //dbug("that's all, folks: %s", at);
                        //stop_timer("build-path");
                        // DONT remove first step, aka, from
                        //dbug("done reconstructing");
                        break SEARCH;
                    } else {
                        if (at instanceof WeightedTurnStep) {
                            WeightedTurnStep atStep = (WeightedTurnStep) at;
                            path.addFirst( new Move(atStep.turn, atStep.turnAt) );
                        }

                        // clean up visited as we go so that loops are broken
                        at = visited.remove(at.getSource());
                    }
                }
            }

            // start at cur.turn.to and see where we have to go...
            Edge cur_e = cur.getDest();
            Pair<Double, Double> info = cur.calcTime(first ? initProg : 0);
            double timeToTraverse = info.get1();
            double curVel = info.get2();
            // make sure curvel isnt 0, we dont have NaN curvel or traverse-time

            first = false;
            
            // Lane shifts! We magically get there with the same time
            for (Edge shift : Util.loopAll(cur_e.getLeft(), cur_e.getRight())) {
                if (shift != null && !visited.containsKey(shift)) {
                    // pass along those associated magic velocities and stuff
                    // TODO adjust the priority to be realistic and avoid too much
                    // graph-searching
                    WeightedStep next_step = new WeightedSideStep(cur_e, shift, cur.at, cur.pri, info);
                    open.add(next_step);
                    visited.put(shift, next_step);
                }
            }

            // normal intersections
            for (Turn turn : cur_e.getDestinations()) {
                Edge next = turn.to;
                if (!visited.containsKey(next)) {
                    // TODO shorter route to e? is this possible?

                    // when's the earliest time we can cross this intersection?
                    if (turn.from == null || turn.to == null) {
                        dbug("astar requesting a null turn!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                    }
                    double crossTime = turn.timeToCross(this, curVel);	// TODO pretty sure its this or the previous step's curVeloc...
                    // make sure crosstime isnt nan.
                    double when = cur_e.to().request(cur.at + timeToTraverse, crossTime, turn);
                    //dbug("... got %f", when);

                    // TODO there are better heuristics. currently euclid distance from
                    // next to goal. this now uses real distance
                    double heuristic = Coordinate.getDistanceInMeters(
                        worldToGPS(next.getStart()), worldToGPS(goal)
                    );

                    WeightedStep next_step = new WeightedTurnStep(turn, when, when + crossTime, curVel, when + heuristic);

                    // rinse and repeat...
                    open.add(next_step);

                    // How do we get to 'next'?
                    visited.put(next, next_step);
                }
            }
            //stop("add-lanes");
        }

        if (path.isEmpty()) {
            dbug("failed to find a route from " + from + " to " + to);
        }

        //stop_timer("total-astar");
        //dumpTimes();

        dbug("a* from " + from + " to " + to);
        push();
        double total = 0;
        for (Move mv : path) {
            if (mv.act == Action.ACT_LANECHANGE) {
                dbug("lanechange at %fs: " + mv.edge, mv.start);
            } else if (mv.act == Action.ACT_TRAVERSE) {
                dbug("step at %fs: " + mv.edge, mv.start);
                total += mv.edge.length();
            } else if (mv.act == Action.ACT_TURN) {
                dbug("turn at %fs: " + mv.turn.type, mv.start);
            }
        }
        dbug("done with astar totally. total was %f without turns", total);
        pop();

        return path;
    }
}
