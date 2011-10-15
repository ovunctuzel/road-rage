/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.map.make;

import java.util.LinkedList;
import java.util.List;

import utexas.helper.Util;
import utexas.map.Coordinate;

// Generates a simple, fake graph for testing.
public class Fake {
    public static void main (String[] args) {
        double offset = 3.0;
        //double offset = 0.0;
        boolean oneway = false;
        double scale = Util.cfg_double("mapmake-scale");

        PreGraph1 g = new PreGraph1();

        Coordinate vCenter = new Coordinate((20 + offset) / scale, (20 + offset) / scale);
        Coordinate vN      = new Coordinate(20 / scale, 35 / scale);
        Coordinate vS      = new Coordinate(20 / scale, 15 / scale);
        Coordinate vW      = new Coordinate(5 / scale,  20 / scale);
        Coordinate vE      = new Coordinate(35 / scale, 20 / scale);

        ////////////////////

        //String type = "residential";
        String type = "primary";

        int rN = g.addEdge("North", type, oneway, ptsList(vCenter, vN), 10);
        int rS = g.addEdge("South", type, oneway, ptsList(vCenter, vS), 20);
        int rW = g.addEdge("West",  type, oneway, ptsList(vCenter, vW), 30);
        int rE = g.addEdge("East",  type, oneway, ptsList(vCenter, vE), 40);

        ////////////////////

        List<Integer> roadLs1 = new LinkedList<Integer>();
        roadLs1.add(rN); roadLs1.add(rS); roadLs1.add(rW); roadLs1.add(rE);
        g.addVertex(vCenter, roadLs1);

        g.addVertex(vN, rdList(rN));
        g.addVertex(vS, rdList(rS));
        g.addVertex(vW, rdList(rW));
        g.addVertex(vE, rdList(rE));


        ////////////////////

        g.constructGraph();
        PreGraph3 g3 = (new Pass3()).make_lanes((new Pass2()).separate(g), g.xOff, g.yOff, scale);
        g3.write_xml("dat/test.map", scale, g.xOff, g.yOff, false);
        System.out.println("Simple test map generated.");
    }

    private static List<Coordinate> ptsList (Coordinate from, Coordinate to) {
        List<Coordinate> ls = new LinkedList<Coordinate>();
        ls.add( new Coordinate(from) );
        ls.add( new Coordinate(to) );
        return ls;
    }

    private static List<Integer> rdList (int rd) {
        List<Integer> ls = new LinkedList<Integer>();
        ls.add(rd);
        return ls;
    }

//    private static List<Integer> rdList (int rd1, int rd2) {
//        List<Integer> ls = new LinkedList<Integer>();
//        ls.add(rd1);
//        ls.add(rd2);
//        return ls;
//    }
}
