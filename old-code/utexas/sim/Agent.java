/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.sim;

import static utexas.helper.Log.dbug;
import static utexas.helper.Log.push;
import static utexas.helper.Log.pop;

import java.util.LinkedList;
import java.util.List;
import java.util.Iterator;

import utexas.helper.Util;
import utexas.map.Coordinate;
import utexas.map.Edge;
import utexas.map.Graph;
import utexas.map.Turn;

// Represents some vehicle on the graph somewhere.
public class Agent {
    public static int idCnt;
    public static final double STD_ACCELERATION = 2.7;
    public static final double STD_DECELERATION = -9000.7;
    //public static final double MAX_DECELERATION = -9.8;

    public int id;

    // current state
    public Edge edge;
    public Turn turn;

    public boolean killMe = false;  // we're stuck.   TODO RouteWorker got stuck.

    private double progress = -1;   // how far are we along our edge in sums of euclid dist?
    private double velocity;
    private List<Graph.Move> route = new LinkedList<Graph.Move>();

    private double total_dist;  // of edge or turn lineseg
    private double lengths[];

    private boolean done = true;
    private boolean locked = false; // are we routefinding right now?

    // slight misnomer; now we handle entering a turn move too
    public Agent (Edge start) {
        id = idCnt++;
        edge = start;
        changedEdge();  // to set line lengths and stuff

        // TODO static case
        int cnt = 0;
        double first_p = start.randomProgress();
        double want_p = first_p;

        double min = start.minSpawnProgress();
        double max = start.maxSpawnProgress();

        double thres = 10;
        boolean reset = false;

        //dbug("picking a spot for %d", id);
        push();

        while (cnt++ < 50) {
            Agent who = edge.getAgentNearestTo(want_p);
            if (who != null) {
                /*
                dbug("we're trying for %f. collided with someone at %f.", want_p,
                        who.progress);
                        */
            } else {
                //dbug("we're first. want %f", want_p);
            }
            // we are hilarious.
            double diff = who == null ? 9001 : Math.abs(want_p - who.progress);
            //dbug("...thats %f between em", diff);
            if (diff >= thres) {  // approx the length of a car. meters.
                progress = want_p;
                //dbug("we got it! %f", progress);
                break;
            } else {
                // add epsilon so we dont collide with the same guy again
                want_p = who.progress + thres + 0.00000000;  // try in front of them. dont collide
                if (want_p >= max) {
                    want_p = min;
                    reset = true;
                } else if (reset && first_p - want_p < thres) {
                    //dbug("looped around, give up");
                    // we've looped around
                    break;
                }
            }
        }
        if (progress == -1) {
            dbug("no room to spawn on %s. tried %d times.", edge, cnt);
            killMe = true;
        }

        pop();

        edge.insertAgent(this);
    }

    // NOTE we no longer leave/enter edge queues.
    private void changedEdge () {
        total_dist = 0;
        if (edge != null) {
            List<Edge.Line> lines = edge.lines;
            lengths = new double[ lines.size() ];
            for (int i = 0; i < lines.size(); i++) {
                lengths[i] = lines.get(i).length(getGraph());
                total_dist += lengths[i];
            }
        } else {
            Edge.Line l = turn.getLine().get(0);
            lengths = new double[1];
            lengths[0] = l.length(getGraph());
            // lengths shouldnt be NaN anymore ever
            total_dist = lengths[0];
        }
    }

    public double getProgress(){
    	return progress;
    }

    public double getVelocity(){
    	return velocity;
    }

    public void setProgress (double p) {
    	progress = p;
    }

    public String toString () {
        return String.format("Agent %d @ %s", id, edge == null ? turn.toString() : edge.toString());
    }

    public Graph getGraph () {
        return edge == null ? turn.from.getRoad().getGraph() : edge.getRoad().getGraph();
    }

    // convention: first element of route is the NEXT place to go.

    // the first step is really the next edge, like it should be.
    public void planRoute (List<Graph.Move> visit) {
        route.clear();
        route.addAll(visit);
        done = false;
        //dbug("Route: " + visit.toString());
        if (route.size() == 0) {
            // almost certain I fixed this.
            dbug("shouldnt happen, but planned empty route for %s", toString());  // TODO shouldnt happen
            done = true;
            unlock(); // TODO just shut them up till we really deal with this
        } else {
            // First step...
            if (edge != route.get(0).edge) {
                dbug("OY!!! started at %s but route starts us at %s. bug.", edge, route.get(0).edge);
            } else {
                edge = route.remove(0).edge;
            }
        }
    }

    // dt is now in seconds
    public void step (double dt, double currentTime) {
        // Wait for a new goal.
        if (locked) {
            return;
        }

        int old_pos = -1;
        int old_e = -1;

        // update our position on the edge. first disappear...
        if (edge != null) {
            old_pos = edge.removeAgent(this);
            old_e = edge.id;
        }
        // this also updates our velocity.
        progress += physicsStep(dt, currentTime);

        do {
            at();   // are we done?

            if (done && route.size() != 0) {
                done = false;   // not done with whole route just yet
                Graph.Move next = route.remove(0);
                double leftover = progress - total_dist;
                if (next.turn != null) {
                    // we better be coming from a traversal!
                    edge = null;    // ignore all of the other old state from an edge
                    turn = next.turn;

                    //dbug("starting %s at %f, dt %fs, should be %f", turn.toString(), currentTime, dt, next.start);
                } else {
                    // we better be coming from a turn!
                    turn = null;    // ignore all of the other old state from a turn
                    edge = next.edge;
                    //dbug("starting %s at %f, dt %fs, should be %f", edge.toString(), currentTime, dt, next.start);
                }

                changedEdge();

                // Remember we're doing the work of a discrete interval of time... so
                // next.start should be in between current - dt and current.
                double early = currentTime - next.start;
                if (early < 0.0) {
                    //dbug("%s is early by %fs", toString(), -early);
                }
                double late = (currentTime - dt) - next.start;
                if (late > 0.0) {
                    //dbug("%s is late by %fs", toString(), late);
                }

                progress = leftover;    // if we arrive exactly on time, this is 0, cool

                // If we're gonna skip over a lane-change move because it's a tiny edge
                // (detected by the below snippet), then hopefully there's room to do so...
                // maybe we shouldnt schedule lanechanges on really small roads?
                /*
                at();
                if (next.act == Graph.Action.ACT_LANECHANGE && done) {
                    dbug("%s skipping over a lanechange...", toString());
                }
                */
            } else {
                break;
            }
        } while (true);

        // Did we end up at a point where we should change lanes? Do it last.
        Graph.Move lanechange = route.size() > 0 ? route.get(0) : null;
        if (lanechange != null && lanechange.act == Graph.Action.ACT_LANECHANGE && progress /
            total_dist >= Util.cfg_double("laneshift-min"))
        {
            if (progress / total_dist >= Util.cfg_double("laneshift-max")) {
                // This shouldn't happen when we fly over entire lanes anymore. Hmm.
                dbug("oops! %s missed their lanechange.", toString());
            } else {
                // check that we can switch lanes right now
                // for now, we model lane-changing as instantaneous. reasonably we just
                // have to check whoever will be behind us in the new lane. if jumping over
                // might clobber them, then try again next step or TODO adjust velocity to
                // speed up or slow down and fight our way in!
                Agent avoid = route.get(0).edge.getAgentBeforeOrAt(progress);
                // TODO complication: our progress has been updated; has theirs? or
                // likewise, we could wait to update our progress, but then theirs might've
                // been... need simulation order structure soon.

                // also, we measure difference in progress rather some euclidean distance.
                // makes sense: progress already encodes distance along edge.
                if (avoid == null || Math.abs(avoid.getProgress() - progress) < Util.cfg_double("laneshift-buffer"))
                {
                    // success.
                    edge = route.remove(0).edge;
                    changedEdge();
                    // TODO make sure times are correct.
                    // Assumes the two edges have approximately the same geometry. technically
                    // might warp backwards!
                    // TODO that should use up some of our progress...
                } else {
                    dbug("%s not changing lanes yet... someone's in the way.", toString());
                }
            }
        }

        // TODO another way to update our position on the edge?
        // this should handle all cases... we end up here.
        if (edge != null) {
            if (done && progress > edge.length()) {
                //dbug("%s at their last edge, and actually theyre already done, so not adding to Q", toString());
            } else {
                int new_pos = edge.insertAgent(this);

                if (old_e == edge.id && old_pos != new_pos) {
                    dbug("%s went from %d to %d in the Q", toString(), old_pos, new_pos);
                }
            }
        }
    }

    // so where are we? null if we're past the edge.
    public Coordinate at () {
        // make sure total_dist isnt Nan, shouldnt happen anymore
        if (killMe) {   // temp :(
            //dbug("is this happening?");
            return new Coordinate(0, 0);
        }
        List<Edge.Line> lines = edge == null ? turn.getLine() : edge.lines;
        if (progress >= total_dist) {
            //dbug("DONE! %f >= %f", progress, total_dist);
            done = true;    // this is how we signal!
            Edge.Line last = lines.get( lines.size() - 1);
            return new Coordinate (last.x2, last.y2); // the end!
        } else if (progress < 0) {
            dbug("negative progress! %s has progress %f", toString(), progress);
            killMe = true;
            return new Coordinate(0, 0);
        }

        double len = progress;
        for (int i = 0; i < lines.size(); i++) {
            if (len < lengths[i]) {
                Edge.Line l = lines.get(i);
                // so we're on this line. we know its length, and our remaining length.
                double width = l.x2 - l.x1;
                double height = l.y2 - l.y1;
                // what percentage of width and height do we add to (l.x1, l.y1)?
                double dx = width * (len / lengths[i]);
                double dy = height * (len / lengths[i]);
                return new Coordinate(l.x1 + dx, l.y1 + dy);
            } else {
                len -= lengths[i];
            }
        }

        // Shouldn't happen...
        dbug("ERR: can't find where on edge/turn an agent is. edge is %s. %f >= %f?", edge, progress, total_dist);
        return new Coordinate(0, 0);
    }

    // ready for a new route
    public boolean done () {
        return done && !locked;
    }

    // figuring out a new route
    public void lock () {
        locked = true;
    }

    public void unlock () {
        locked = false;
    }

    public boolean locked () {
        return locked;
    }
    
    public double getTargetVelocity (double dt, double now) {
        // TODO so we can go any speed during a turn?
        double maxSpeed = -1;
        if (turn == null) {
            maxSpeed = edge.getRoad().getSpeedLimit();

            Agent victim = getNextAgentOnRoute(100);    // how many meters to lookahead?

            if (victim != null) {
                maxSpeed = Math.min(maxSpeed, victim.getVelocity());
            }
        } else {
            maxSpeed = velocity;
        }

        double speed;

        if (route.size() > 0) {
            double goalTime = route.get(0).start;
            double timeLeft = goalTime - now;

            if (timeLeft < 0) {
                timeLeft = 0.0001;  // pressure to go fast to catch up to schedule
            }

            double distLeft = total_dist - progress;
            //dbug("be at end of edge by %f, it's %f now", goalTime, now);
            //dbug("so we have %f to cross %f dist going, at max, veloc %f, over a step of %f",
            //     timeLeft, distLeft, maxSpeed, dt
            //);
            //dbug("...");

            speed = distLeft / timeLeft;
            //dbug("want to go %f for this step", speed);
        } else {
            // TODO not sure what to do last.. does it matter still?
            speed = edge.getRoad().getSpeedLimit();
        }

        return speed > maxSpeed ? maxSpeed : speed;
    }

    // TODO note that we only consider edges, not turns. use timetables to figure out if
    // you can be at an intersection.
    private Agent getNextAgentOnRoute (double meters_ahead) {
        // are they on our edge?
        Agent first = edge.getNearestAgentAfter(progress);
        if (first != null) {
            if (first.getProgress() - progress <= meters_ahead) {
                return first;
            } else {
                return null;    // dont care, guy in front of us isn't that close.
            }
        }

        double dist = edge.length() - progress;
        Iterator<Graph.Move> step = route.iterator();
        while (dist <= meters_ahead && step.hasNext()) {
            Graph.Move next_move = step.next();
            if (next_move.act == Graph.Action.ACT_TRAVERSE) {
                if (next_move.edge.getAgents().isEmpty()) {
                    // nobody there...
                    dist += next_move.edge.length();
                } else {
                    Agent victim = next_move.edge.getAgents().get(0);
                    if (victim.getProgress() - progress <= meters_ahead) {
                        return victim;
                    } else {
                        return null;    // dont care, guy in front of us isn't that close.
                    }
                }
            } else if (next_move.act == Graph.Action.ACT_TURN) {
                dist += next_move.turn.length(getGraph());
            }
            // TODO are we counting distance correctly when we do lanechanges
        }
        return null;
    }

    // Here's the physics.  Yay Newton. Modifies velocity and returns distance travelled
    private double physicsStep (double dt, double currentTime) {
        double targetVelocity = getTargetVelocity(dt, currentTime);
        double initialVelocity = velocity;
        //The new velocity if we just accelerated for the entire step
        double newVel = (velocity < targetVelocity) ? (velocity + STD_ACCELERATION * dt) : (velocity == targetVelocity) ? velocity : (velocity + STD_DECELERATION * dt);
        
        //Determine how long it takes to get to the target velocity (splitting up the step by acceleration)
        double dt1 = dt;
        if (initialVelocity < targetVelocity & newVel > targetVelocity){
        	dt1 = (targetVelocity - initialVelocity)/STD_ACCELERATION;
        	velocity = targetVelocity;
        }
        else if (initialVelocity == targetVelocity) dt1 = 0;
        else if (initialVelocity > targetVelocity & newVel < targetVelocity){
        	dt1 = (targetVelocity - initialVelocity)/STD_DECELERATION;
        	velocity = targetVelocity;
        }
        else velocity = newVel;
        double dt2 = dt - dt1;
        
        //Now we need to figure out the change in distance
        double distChange = initialVelocity*dt1 + targetVelocity*dt2;
        distChange += (initialVelocity < targetVelocity) ? (1./2.)*STD_ACCELERATION*dt1*dt1 : (1./2.)*STD_DECELERATION*dt1*dt1;
        //End physics stuff.  Result is distChange (and the change to velocity)

        // make sure distChange isnt negative or NaN

        return distChange;
    }
}
