/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.helper;

import java.util.Map;
import java.util.HashMap;

// This allows pretty-printing of debug statements in an indented hierarchy
public class Log {
    private static boolean DBUG = true; // turn off all output here
    private static int indent = 0;

    public static void push () {
        indent++;
    }

    public static void pop () {
        indent--;
    }

    public static void dbug (String say) {
        if (DBUG) {
            for (int i = 0; i < indent; i++) {
                System.out.print("  ");
            }
            System.out.println(say);
        }
    }

    // "I'll give up printf() when you pry my cold dead fingers from it"
    public static void dbug (String say, Object ... args) {
        dbug( String.format(say, args) );
    }

    // Even if you turn off DBUG, these still print.
    public static void cmt (String say) {
        System.out.println(say);
    }

    // What're we doing, when did we start it, and how many times have we done it?
    static class Task {   
        long time, sumTime = 0;
        int cnt = 0;
        String name;

        Task () {   
            time = System.nanoTime();
        }
    }
    
    private static Map<String, Task> times = new HashMap<String, Task>();

    // The once-per-lifetime tasks
    public static void start_timer (String task) {
        times.put( task, new Task() );
    }
    
    public static void stop_timer (String task) {
        double dt = (System.nanoTime() - times.remove(task).time) / 1000000000.0;
        dbug(task + " took " + dt + "s");
    }
    
    // The tasks we do over and over, like twiddling our thumbs
    public static void start (String task) {   
        Task t = times.get(task);                                                    
        if (t == null) {
            t = new Task();
            t.name = task;
            times.put(task, t);
        }
        t.cnt++;
        t.time = System.nanoTime();
    }

    public static void stop (String task) {
        Task t = times.get(task);
        t.sumTime += (System.nanoTime() - t.time);
        t.time = 0;
    }

    // Dump all the repeated task timings
    public static void dumpTimes () {
        for (Task t : times.values()) {
            double dt = t.sumTime / 1000000000.0;
            dbug("%s took %fs over %d iters, avg %f", t.name, dt, t.cnt, dt / t.cnt);
        }
        times.clear();
    }
}
