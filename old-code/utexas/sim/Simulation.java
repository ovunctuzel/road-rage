/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.sim;

import static utexas.helper.Log.pop;
import static utexas.helper.Log.push;
import static utexas.helper.Log.dbug;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import utexas.helper.Pair;
import utexas.helper.Util;
import utexas.map.Edge;
import utexas.map.Graph;

// Manages some agents?
public class Simulation implements Runnable {
    static public abstract class Callback {
        public abstract void fire (Object listener, String msg);
    }

    List< Pair<Object, Callback> > callbacks = new LinkedList< Pair<Object, Callback> >();
    List<Agent> agents = new LinkedList<Agent>();
    Graph graph;
    private Thread thread;
    private boolean running = false;
    private double tick = 0;    // how many seconds have passed in sim-time?

    private ExecutorService manager = Executors.newSingleThreadExecutor();
    //private ExecutorService manager = Executors.newFixedThreadPool(4);

    public Simulation (Graph map) {
        graph = map;
        Agent.idCnt = 0;        // TODO why? i forgot...
        thread = new Thread(this);
        thread.start();
    }

    public double getTick () {
        return tick;
    }

    public void addCB (Object listener, Callback cb) {
        callbacks.add( new Pair<Object, Callback>(listener, cb) );
    }

    public void toggle () {
        running = !running;
    }

    public Agent addAgent (Edge start) {
        if (start == null) {
            dbug("where are we spawning?!");
        }
        Agent a = new Agent(start);
        // Pick a random starting progress within the lane-change boundary
        agents.add(a);
        giveRoute(a);
        return a;
    }

    public Agent addAgent (Edge start, Edge end) {
        Agent a = new Agent(start);
        agents.add(a);
        a.lock();   // set the lock since the job might not start asap
        Runnable job = new RouteWorker(this, a, end);
        manager.execute(job);
        return a;
    }

    // This now submits jobs to the thread pool.
    private void giveRoute (Agent a) {
        a.lock();   // set the lock since the job might not start asap
        Runnable job = new RouteWorker(this, a);
        manager.execute(job);
    }

    public List<Agent> getAgents () {
        return agents;
    }

    @Override public void run () {
        while (true) {
            long start = System.currentTimeMillis();
            try {
                Thread.sleep(100);  // should fire <= 10x/second, but doesnt matter due to dt
            } catch (InterruptedException e) {
                System.out.println("ERR thread interrupted: " + e);
            }

            if (running) {
                step(System.currentTimeMillis() - start);
            }
        }
    }

    public void step (long dt_ms) {
        // dt is in ms... but not for long
        double dt = dt_ms / 1000.0 * Util.cfg_double("speedup");
        tick += dt;
        List<Agent> survivors = new LinkedList<Agent>();
        for (Agent a : agents) {
            if (!a.killMe) {
                a.step(dt, tick);
                if (a.done()) {
                    // Nuke them!
                } else {
                    survivors.add(a);
                    // TODO we're not using the ID for anything... right?
                }
            }   // don't add them; they die.
        }
        agents = survivors; // TODO do this faster

        for (Pair<Object, Callback> cb : callbacks) {
            cb.get2().fire(cb.get1(), "step");
        }
    }

    public int agentCnt () {
        return agents.size();
    }

    public void spawnArmy (int n) {
        push();
        for (int i = 1; i <= n; i++) {
            push();
            addAgent( graph.getRandomEdge(null) );  // first edge can be anything
            pop();
        }
        pop();
        dbug("");
    }

    public void runExperiment () {
        spawnArmy( Util.cfg_int("experiment-army-size") );

        addAgent(
            graph.getEdges().get( Util.cfg_int("experiment-start") ),
            graph.getEdges().get( Util.cfg_int("experiment-end") )
        );
    }
};

class RouteWorker implements Runnable {
    private Simulation sim;
    private Agent agent;
    private Edge goal;

    RouteWorker (Simulation s, Agent a) {
        sim = s;
        agent = a;
        goal = null;
    }

    RouteWorker (Simulation s, Agent a, Edge to) {
        sim = s;
        agent = a;
        goal = to;
    }

    @Override public void run () {
        //dbug("giving route to %s", agent.toString());
        push();
        List<Graph.Move> route;
        // TODO this should never fail anymore IF our graph is connected.
        // Give it a random or determined route to follow. A valid route.

        Edge to = goal == null ? sim.graph.getRandomEdge(agent.edge) : goal;
        PER_GOAL: while (true) {
            // we may get scheduling conflicts from another thread and have to retry
            route = sim.graph.astar(agent.edge, agent.getProgress(), to, sim.getTick());
            if (route != null) {
                break PER_GOAL;
            }
            if (route.isEmpty()) {
                dbug("killing new agent; we're in an SCC");
                // TODO possibly unsafe.
                agent.killMe = true;
            }
        }
        agent.planRoute(route);
        agent.unlock();
        pop();
    }
}
