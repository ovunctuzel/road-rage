/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.helper;

// Basic tuples are convenient.
public class Pair<A, B> {
    private A fst;
    private B snd;

    public Pair (A a, B b) {
        fst = a;
        snd = b;
    }

    public A get1 () {
        return fst;
    }

    public B get2 () {
        return snd;
    }
}
