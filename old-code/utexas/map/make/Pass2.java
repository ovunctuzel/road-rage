/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import static utexas.helper.Log.*;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import utexas.map.Coordinate;

// OSM ways cross many intersections; we want individual chunks of road.
public class Pass2 {
	private static final double MINIMUM_ROAD_SEGMENT_DISTANCE = 0.5;
	
    PreGraph2 g;
    List< List<PreGraph2.TmpVertex2> > sccs = new LinkedList< List<PreGraph2.TmpVertex2> >();

    public PreGraph2 separate (PreGraph1 graph) {
        // Coordinates have been transformed, so just make a new set of vertices
        Set<Coordinate> vertls = new HashSet<Coordinate>();
        for (PreGraph1.TmpVertex1 v : graph.vertices) {
            vertls.add(v.location);
        }

        // Find true edges between adjacent vertices
        g = new PreGraph2(graph.width, graph.height);
        for (PreGraph1.TmpVertex1 from : graph.vertices) {
            for (PreGraph1.TmpEdge1 e : from.roads) {
                // scan from each edge from the vertex
                int us = e.points.indexOf(from.location);

                LinkedList<Coordinate> scan1 = new LinkedList<Coordinate>();
                scan1.add(from.location);
                for (int i = us - 1; i > 0; i--) {
                    Coordinate pt = e.points.get(i);
                    scan1.addFirst(pt); // preserve order for one-ways?
                    if (vertls.contains(pt)) {
                        g.addEdge(from.location, pt, e.name, e.type, e.oneway, scan1, e.osmID);
                        break;
                    }
                }

                List<Coordinate> scan2 = new LinkedList<Coordinate>();
                scan2.add(from.location);
                for (int i = us + 1; i < e.points.size(); i++) {
                    Coordinate pt = e.points.get(i);
                    scan2.add(pt);
                    if (vertls.contains(pt)) {
                        g.addEdge(from.location, pt, e.name, e.type, e.oneway, scan2, e.osmID);
                        break;
                    }
                }
            }
        }
        
        removeTinyRoadParts();

        // Now run Tarjan's algorithm to locate the strongly-connected components of the
        // graph. A disconnected graph is bad, mkay?
        for (PreGraph2.TmpVertex2 v : g.vertices) {
            if (!visited.contains(v)) {
                tarjan(v);
            }
        }

        // Which is the biggest? If we have two comparably sized SCC's, arbitrarily chooses
        // one.
        int biggest = 0, biggest_idx = 0;
        for (int i = 0; i < sccs.size(); i++) {
            int sz = sccs.get(i).size();
            if (sz > biggest) {
                biggest = sz;
                biggest_idx = i;
            }
        }
        dbug("Start with %d verts and %d edges...", g.vertices.size(), g.edges.size());
        push();
        dbug("Largest SCC of %d has %d vertices", sccs.size(), biggest);

        // After we remove vertices, we have their old and new IDs. So keep a map.
        // to be clear: the key is the ORIGINAL id. the value is the NEW id.
        int vertIDs[] = new int[ g.vertices.size() ];
        // at first it's just the identity
        for (int j = 0; j < g.vertices.size(); j++) {
            vertIDs[j] = j;
        }

        // After we remove edges, we have their old and new IDs. So keep a map.
        int edgeIDs[] = new int[ g.edges.size() ];
        // at first it's just the identity
        for (int j = 0; j < g.edges.size(); j++) {
            edgeIDs[j] = j;
        }

        // Just remove edges first... not sure why things botch up otherwise.
        for (int i = 0; i < sccs.size(); i++) {
            if (i != biggest_idx) {
                List<PreGraph2.TmpVertex2> scc = sccs.get(i);

                //dbug("Nuking SCC with only %d vertices", scc.size());
                push();
                // Carefully remove things from the map. Luckily we're guarenteed to not
                // having dangling references in the final product since we just ran an
                // algorithm guarenteed to separate those things.

                // remember who we're going after next.
                Set<Integer> doomedEdges = new HashSet<Integer>();

                // Just to visually check Tarjan's works
                for (PreGraph2.TmpVertex2 v : scc) {
                    for (PreGraph2.TmpEdge2 e : v.roads) {
                        e.type = "doomed";
                        doomedEdges.add(e.orig_id);
                    }

                    // Nix the vertex
                    int vid = vertIDs[v.orig_id];
                    g.vertices.remove(vid);
                    //dbug("vert %d nuked", vid);

                    // Keep the permutation table updated
                    for (int j = vid; j < g.vertices.size(); j++) {
                        PreGraph2.TmpVertex2 vert = g.vertices.get(j);
                        if (vert.id == j) {
                            dbug("vert shifting is very very wrong... TODO");
                        }
                        vert.id = vertIDs[vert.orig_id] = j;
                    }
                }

                // Now nuke defunct edges
                for (int doomed : doomedEdges) {
                    // 'doomed' are the original IDs
                    int eid = edgeIDs[doomed];
                    g.edges.remove(eid);
                    //dbug("edge %d nuked (originally %d)", eid, doomed);

                    // Keep the permutation table updated
                    for (int j = eid; j < g.edges.size(); j++) {
                        PreGraph2.TmpEdge2 edge = g.edges.get(j);
                        if (edge.id == j) {
                            dbug("edge shifting is very very wrong... TODO");
                        }
                        if (edgeIDs[edge.orig_id] - j != 1) {
                            dbug("edge shifting is off. going from orig %d to %d, not %d",
                                    edge.orig_id, j, edgeIDs[edge.orig_id] - 1);
                        }
                        edge.id = edgeIDs[edge.orig_id] = j;
                    }
                }

                pop();
            }
        }

        // OHHHHHHH. we update e.v1 and e.v2, but then go and fuck with the IDs again.
        // clean up all of that at the VERY end.

        // Clean up edges' references
        // Before we remove anything, edges store original vertex IDs.
        for (PreGraph2.TmpEdge2 e : g.edges) {
            int new1 = vertIDs[ e.v1 ];
            int new2 = vertIDs[ e.v2 ];
            // If either is null, this edge is gonna die soon.
            if (e.v1 != new1) {
                //dbug("an edge switched vert ptr from %d to %d", e.v1, new1);
                e.v1 = new1;
            }
            if (e.v2 != new2) {
                //dbug("an edge switched vert ptr from %d to %d", e.v2, new2);
                e.v2 = new2;
            }
        }

        pop();
        dbug("End with %d verts and %d edges after removing SCC's.", g.vertices.size(), g.edges.size());
        // Temporary: check IDs for sanity.
        for (int i = 0; i < g.vertices.size(); i++) {
            if (g.vertices.get(i).id != i) {
                dbug("vert %d botched", i);
            }
        }
        for (int i = 0; i < g.edges.size(); i++) {
            if (g.edges.get(i).id != i) {
                dbug("edge %d botched", i);
            }
            if (g.edges.get(i).type.equals("doomed")) {
                dbug("our victim survived..");
            } 
        }

        return g;
    }

    Set<PreGraph2.TmpVertex2> visited = new HashSet<PreGraph2.TmpVertex2>();
    Stack<PreGraph2.TmpVertex2> stack = new Stack<PreGraph2.TmpVertex2>();
    int dfs = 0;    // DFS counter numbering

    // Pretty much verbatim from a CS 315H lecture during fall 2010
    private void tarjan (PreGraph2.TmpVertex2 v) {
        visited.add(v);
        v.idx = dfs++;
        v.low = v.idx;
        stack.add(v);
        v.inStack = true;

        for (PreGraph2.TmpEdge2 e : v.roads) {
            PreGraph2.TmpVertex2 neighbor = g.vertices.get( e.other(v) );
            if (!visited.contains(neighbor)) {
                tarjan(neighbor);
                v.low = Math.min(v.low, neighbor.low);
            // Here's that trick to avoid expensive linear stack contaiment
            } else if (neighbor.inStack) {
                // This is a back-edge
                v.low = Math.min(v.low, neighbor.idx);
            }
        }

        // Are we a 'root' of an SCC?
        if (v.low == v.idx) {
            PreGraph2.TmpVertex2 buddy;
            List<PreGraph2.TmpVertex2> scc = new LinkedList<PreGraph2.TmpVertex2>();
            do {
                buddy = stack.pop();
                buddy.inStack = false;
                scc.add(buddy);
            } while (v != buddy);
            sccs.add(scc);
        }
    }

    // Mike, you rock
    public void removeTinyRoadParts() {
    	for (PreGraph2.TmpEdge2 edge : g.edges) {
    		Coordinate lastPoint = null;
    		for (int i = 0; i < edge.points.size(); i++) {
    			Coordinate c = edge.points.get(i);
    			if (lastPoint == null) {     // Don't remove first point
    				lastPoint = c;
    				continue;
    			}
    			double dist = c.distTo(lastPoint);
    			if (dist < MINIMUM_ROAD_SEGMENT_DISTANCE) {
    				if (i != edge.points.size() - 1) {
                        if (lastPoint.equals(edge.points.get(i + 1))) {
                            dbug("not removing a short pt bc we'd have overlap");
                        } else {
                            edge.points.remove(i--);
                            //dbug("Removed point at %s. Distance from last point = %f", c, dist);
                        }
    				} else {
    				    if (i > 1){
                            // TODO uhh i think we always want at least 2 points?
                            // Remove the second to last point, since the last point is necessary
    					    edge.points.remove(--i);
                            //dbug("Removed point at %s. Distance from last point = %f", c, dist);
    					}
    					break;  // We just handled the last point. We're done.
    				}
    				
    			}
    			lastPoint = c;
    		}

            // Now, check to make sure we don't have adjacent points that're the same.
    		for (int i = 1; i < edge.points.size(); i++) {
                if (edge.points.get(i - 1).equals(edge.points.get(i))) {
                    dbug("warning: %s has duplicate points! %d total", edge.name, edge.points.size());
                    // TODO this breaks things because these roads are just weird...
                    //edge.points.remove(i--);
                }
            }

            if (edge.points.size() < 2) {
                dbug("why does %s have < 2 pts?!", edge.name);
            }
    	}
    }
}
