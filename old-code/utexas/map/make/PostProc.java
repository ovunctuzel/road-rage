/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import static utexas.helper.Log.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.PriorityQueue;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import utexas.map.*;
import utexas.map.Graph.WeightedStaticStep;

// post-processing... right now, just possibly pre-computing a massive path lookup table...
public class PostProc {
    Graph g;

    public static void main (String[] args) {
        PrintStream out;
        try {
            out = new PrintStream(new FileOutputStream("dat/test.tbl"));
        } catch (IOException e) {
            dbug(e.toString());
            return;
        }

        out.println("<table>");
        (new PostProc()).precompute(out);
        out.println("</table>");
        out.close();
    }

    private void precompute (PrintStream out) {
        g = Graph.load("dat/test.map");

        int cnt = 0;
        int total = g.getEdges().size();
        // find all routes to one end at a time to exploit caching
        for (Edge end : g.getEdges()) {
            dbug("Pre-computing paths %f%%", 100 * ((0.0 + cnt) / total));
            for (Edge start : g.getEdges()) {
                out.print(String.format("  <path from=\"%d\" to=\"%d\" rt=\"", start.id, end.id));
                boolean first = true;
                for (Edge e : pathfind_cachey(start, end, start.id - 1)) {
                    if (!first) {
                        out.print(",");
                    }
                    first = false;
                    out.print(e.id);
                }
                out.print("\">\n");
            }
            cnt++;
            break;
        }
    }

    // Exploit what we already know, and short-circuit.
    private List<Edge> pathfind_cachey (Edge from, Edge to, int sofar) {
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
            Edge cur_e = cur.getDest();
            if ((cur_e ==to || cur_e.id <= sofar) && !first) {    // if from=to, find a loop and dont sit still.
                // found path! short-circuits when we hit a node which we already know how
                // to reach
                WeightedStaticStep at = cur;
                while (true) {
                    path.addFirst(at.getDest());

                    // clean up visited as we go so that loops are broken
                    at = visited.remove(at.getDest());
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
                    open.add( new WeightedStaticStep(next, cur.when(), cur.when() + heuristic) );
                    visited.put(next, cur);
                }
            }
        }
        
        return path;
    }
}
