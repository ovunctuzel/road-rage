/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.helper;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import static utexas.helper.Log.dbug;

public class Util {
    public static Map<String, Object> cfg = new HashMap<String, Object>();
    static {
        cfg.put("experiment-start", 2785);  // Guadalupe
        cfg.put("experiment-end", 2832);    // South Con
        cfg.put("experiment-army-size", 1000);  // How many victims -- I mean obstacles.

        cfg.put("mapmake-scale", 5000.0);
        cfg.put("maplane-width", 0.5);

        cfg.put("antialias",     false);
        cfg.put("center-dash",   false);
        cfg.put("cursor-bubble", true);
        cfg.put("draw-arrows",   true);

        cfg.put("doubleMin",  0.001);
        cfg.put("doubleMax",  100.0);
        cfg.put("doubleStep", 0.005);

        cfg.put("intMin",  0);
        cfg.put("intMax",  9999);
        cfg.put("intStep", 1);

        cfg.put("centerStroke",     0.1f);
        cfg.put("laneStroke",      0.05f);
        cfg.put("thickLaneStroke", 0.15f);
        cfg.put("laneWidth",        0.6f);
        cfg.put("zoomThreshold",    5.0);

        cfg.put("army-size", 500);
        cfg.put("speedup", 1.0);    // how much faster should everything go?
        cfg.put("laneshift-min",  0.1);
        cfg.put("laneshift-max",  0.9);
        cfg.put("laneshift-buffer", 1.0);   // how much progress needed as buffer space
    }

    public static Double cfg_double (String key) {
        Object value = cfg.get(key);
        if (value instanceof Double) {
            return (Double) value;
        } else {
            // TODO messy.
            Float tmp = (Float) value;
            return Double.parseDouble( Float.toString(tmp) );
        }
    }

    public static int cfg_int (String key) {
        return (Integer) cfg.get(key);
    }

    public static boolean cfg_true (String key) {
        return (Boolean) cfg.get(key);
    }

    // Watch the order!
    static public double arctan (double dx, double dy) {
        if (dx == 0) {
            return Math.PI / 2;
        } else {
            return Math.atan(dy / dx);
        }
    }

    // Watch the order!
    static public double atan2 (double x, double y) {
        // Tries to be clever. Apparently more clever than Math.atan and atan2.
        if (x == 0 && y > 0) {
            return Math.PI / 2;
        } else if (x == 0 && y < 0) {
            return 3 * Math.PI / 2;
        } else if (y == 0 && x > 0) {
            return 0;
        } else if (y == 0 && x < 0) {
            return Math.PI;
        } else {
            double t = Math.atan2(y, x);
            // Get in the range [0, 2pi]
            if (t < 0) {
                t += 2 * Math.PI;
            }
            // java's atan only surjects [-pi, pi], so if we're in quadrant 2 or 3, add
            // another pi
            if (x < 0) {
                //t += Math.PI;
            }
            return t;
        }
    }

    // Java needs less awkward iterators
    public static <A extends Object> List<A> loopAll(List<A> one, List<A> two) {
        List<A> ls = new LinkedList<A>();
        ls.addAll(one);
        ls.addAll(two);
        return ls;
    }

    public static <A extends Object> Set<A> loopAll(Set<A> one, Set<A> two) {
        Set<A> ls = new HashSet<A>();
        ls.addAll(one);
        ls.addAll(two);
        return ls;
    }

    // And anonymous list-making... for x in (one thing, another thing)...
    public static <A extends Object> List<A> loopAll(A one, A two) {
        List<A> ls = new LinkedList<A>();
        ls.add(one);
        ls.add(two);
        return ls;
    }

    // smallest rotation to get from one angle to another. + = counterclockwise, - = clockwise
    public static double angleRot (double a1, double a2) {
        double larger, smaller;
        if (a1 == a2) {
            return 0;
        } else if (a1 > a2) {
            larger = a1;
            smaller = a2;
        } else {
            larger = a2;
            smaller = a1;
        }

        double rot1 = larger - smaller;
        double rot2 = (2 * Math.PI) + smaller - larger;

        if (Math.abs(rot1) < Math.abs(rot2)) {
            if (smaller == a1) {
                return rot1;
            } else {
                return -rot1;
            }
        } else {
            if (larger == a1) {
                return rot2;
            } else {
                return -rot2;
            }
        }
    }

    public static double r2d (double angle) {
        return Math.toDegrees(angle);
    }

    // even when we create a list with an initial capacity, size is 0, so we can't
    // set stuff.
    @SuppressWarnings(value = "unchecked")
    public static void inflateArray (@SuppressWarnings("rawtypes") List ls, int sz) {
        // adding null is not the best idea, but the idea is that we'll fill it up
        // properly.
        while (ls.size() != sz) {
            ls.add(null);
        }
    }

    public static <A extends Object> List<A> reverse (List<A> orig) {
        List<A> rev = new LinkedList<A>(orig);
        Collections.reverse(rev);
        return rev;
    }

    private static Random rng;
    static {
        long time = System.currentTimeMillis();
        System.out.println("RNG seed: " + time);
        rng = new Random(time);
    }
    public static <A extends Object> A chooseRnd (List<A> ls) {
        return ls.get( rng.nextInt(ls.size()) );
    }

    public static double rndDouble (double min, double max) {
        if (max < min) {
            double tmp = min;
            min = max;
            max = tmp;
        } else if (min == max) {
            return min;
        }

        return min + rng.nextDouble() * (max - min);
    }

    public static void seedRNG (long seed) {
        rng.setSeed(seed);
    }
}
