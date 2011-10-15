/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.helper;

// Haskell is influencing me too much.
public class Either<A, B> {
    private A fst;
    private B snd;

    public Either (Object o) {
        fst = null;
        snd = null;

        // type erasure breaks this. :(

        /*
        if (o instanceof A) {
            fst = (A) o;
        } else {
            snd = (B) o;
        }
        */
    }

    // Just delegate
    public String toString () {
        return get().toString();
    }

    private Object get () {
        return fst == null ? snd : fst;
    }
}
