/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.ui;

import static utexas.helper.Log.dbug;
import static utexas.helper.Log.pop;
import static utexas.helper.Log.push;

import java.awt.BasicStroke;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.KeyEvent;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JOptionPane;

import utexas.helper.Util;
import utexas.map.Coordinate;
import utexas.map.Edge;
import utexas.map.Graph;
import utexas.map.Road;
import utexas.map.Turn;
import utexas.map.Vertex;
import utexas.sim.Agent;
import utexas.sim.Simulation;

// on-screen line
class ScreenLine {
    public Line2D.Double line;
    
    Road road;        // for the center-line
    Edge edge;        // for a lane
    Edge.Line edgeLn; // for a lane
    Shape arrow;      // also for a lane

    public ScreenLine (Coordinate a, Coordinate b, Road r) {
        line = new Line2D.Double(a.x, a.y, b.x, b.y);
        road = r;
        edge = null;
        edgeLn = null;
        arrow = null;
    }

    public ScreenLine (Edge.Line l, Edge e) {
        line = new Line2D.Double(l.x1, l.y1, l.x2, l.y2);
        road = null;
        edge = e;
        edgeLn = l;
        arrow = MapCanvas.draw_arrow(l, 3);
    }
}

class Tile {
	List<ScreenLine> bg_lines = new LinkedList<ScreenLine>();
	List<ScreenLine> fg_lines = new LinkedList<ScreenLine>();
	Rectangle2D.Double box;

	public Tile (double x1, double y1, double w, double h) {
		box = new Rectangle2D.Double(x1, y1, w, h);
	}
}

public class MapCanvas extends ScrollCanvas {
	private static final long serialVersionUID = -1248337239496295754L;
	final double epsilon = 5.0;    // deal with imprecision, as usual, by epsilon bubble
    String highlightType = null;
    Graph map;  // so tired of casting constantly
    String currentRoad;
    public boolean pickMode = false, pickAgent = false;
    String pathfindType;
    public int pickWhich;
    public Edge fromE, toE;
    Set<Edge> route = new HashSet<Edge>();
    Edge mouseover;
    int turn_n = -1;
    List<Integer> markedAgents = new LinkedList<Integer>();
    Simulation sim;

    // a measure of granularity... higher number means really high start-up cost, but maybe cheaper run-time cost
    // TODO maybe depends on map-size?
    private static final int numTiles = 50;

    List<Tile> tiles = new LinkedList<Tile>();

    MapCanvas (JComponent context) {
        super(context);
    }

    // find all matching tiles, add the line there
    private void addToTiles (ScreenLine ln, boolean bg) {
        for (Tile t : tiles) {
            if (ln.line.intersects(t.box)) {
                if (bg) {
                    t.bg_lines.add(ln);
                } else {
                    t.fg_lines.add(ln);
                }
            }
        }
    }

    // Orders things so nicely!
    private List<ScreenLine> matchLines (Rectangle2D.Double window) {
        List<ScreenLine> bg_lines = new LinkedList<ScreenLine>();
        List<ScreenLine> fg_lines = new LinkedList<ScreenLine>();
        for (Tile t : tiles) {
            if (t.box.intersects(window)) {
                bg_lines.addAll(t.bg_lines);
                fg_lines.addAll(t.fg_lines);
            }
        }
        bg_lines.addAll(fg_lines);
        return bg_lines;
    }

    @Override public void usedSource () {
        // TODO reset lots of other stuff too probably
        route.clear();
        tiles.clear();
        map = (Graph) source;
        sim = new Simulation(map);
        sim.addCB(this, new Simulation.Callback () {
            public void fire (Object us, String msg) {
                // TODO just step events for now
                ((MapCanvas) us).repaint();
                ((MapCanvas) us).updateStatus();
            }
        });

        dbug("Pre-rendering tiles...");

        double tileW = map.getWidth() / numTiles;
        double tileH = map.getHeight() / numTiles;
        for (int x = 0; x < numTiles; x++) {
            for (int y = 0; y < numTiles; y++) {
                tiles.add( new Tile(x * tileW, y * tileH, tileW, tileH) );
            }
        }

        dbug("  Pre-rendering base roads...");

        // Draw base roads first
        for (Road r : map.getRoads()) {
            List<Coordinate> pts = r.getPoints();
            for (int i = 1; i < pts.size(); i++) {
                Coordinate from = pts.get(i - 1);
                Coordinate to = pts.get(i);
                if (r.isOneWay()) {
                    Edge.Line l = r.getOneWayLanes().get(0).lines.get(i - 1);
                    // shift the road over a half-lane? eh...
                    from = new Coordinate((from.x + l.x1) / 2, (from.y + l.y1) / 2);
                    to = new Coordinate((to.x + l.x2) / 2, (to.y + l.y2) / 2);
                }
                addToTiles( new ScreenLine(from, to, r), true );
            }
        }

        dbug("  Pre-rendering lanes...");

        // Then lanes
        for (Road r : map.getRoads()) {
            for (Edge e : Util.loopAll(r.getPosLanes(), r.getNegLanes())) {
                for (Edge.Line l : e.lines) {
                    addToTiles( new ScreenLine(l, e), false );
                }
            }
        }

        // Start in the centerish
        zoom = 1.0;
        xOff = canvasWidth() / 2;
        yOff = canvasHeight() / 2;
        repaint();
    }

    @Override void renderCanvas (Graphics2D g2d) {
        int width = (int) map.getWidth();
        int height = (int) map.getHeight();

        float centerDash[] = { 1.0f };
        Stroke centerStroke = new BasicStroke(
            (Float) Util.cfg.get("centerStroke"), BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1.0f, centerDash, 0.0f
        );
        Stroke laneStroke = new BasicStroke((Float) Util.cfg.get("laneStroke"));
        Stroke thickLaneStroke = new BasicStroke((Float) Util.cfg.get("thickLaneStroke"));

        // Bounds?
        double x1 = screenToMapX(0);
        double y1 = screenToMapY(0);
        double x2 = screenToMapX( getWidth() );
        double y2 = screenToMapY( getHeight() );
		Rectangle2D.Double window = new Rectangle2D.Double(x1, y1, x2 - x1, y2 - y1);

        // Draw the roads.
        int cntDone = 0;
        //int cntSkipped = 0;
        Set<String> seen = new HashSet<String>();
        List<ScreenLine> roadsSeen = new LinkedList<ScreenLine>();
        for (ScreenLine l : matchLines(window)) {
		    cntDone++;
			if (l.road != null) {
			    String type = l.road.getType();
                if (type == null) {
				    type = "null";
                }
                if (highlightType != null && type.equals(highlightType)) {
                //if (l.road.isOneWay()) {
				    g2d.setColor(Color.GREEN);
                } else {
				    g2d.setColor(Color.BLACK);
                }
                seen.add( l.road.toString() );

                g2d.setStroke( new BasicStroke(((Float) (Util.cfg.get("laneWidth"))) * l.road.numLanes()) );
                g2d.draw(l.line);
                if (!l.road.isOneWay()) {
                    // don't add to roads seen; we don't want a center line!
                    roadsSeen.add(l);
                }
			} else if (zoom >= (Double) Util.cfg.get("zoomThreshold")) {
			    g2d.setStroke(laneStroke);

                if (route.contains(l.edge)) {
                  g2d.setStroke(thickLaneStroke);
                  g2d.setColor(Color.GREEN);
                } else if (pickMode && (fromE == l.edge || toE == l.edge)) {
                  g2d.setStroke(thickLaneStroke);
                  g2d.setColor(Color.ORANGE);
                } else {
                  g2d.setColor(Color.WHITE);
                }
                g2d.draw(l.line);

                // TODO some kind of forced spacing if we have a billion line segments
                if (Util.cfg_true("draw-arrows")) {
                    g2d.setColor(Color.BLUE);
                    g2d.fill(l.arrow);
                }
            }
		}

        // draw center lines after all road backgrounds have been drawn
        if (zoom >= (Double) Util.cfg.get("zoomThreshold")) {
            for (ScreenLine l : roadsSeen) {
                g2d.setColor(Color.YELLOW);
                if ((Boolean) Util.cfg.get("center-dash")) {
                    g2d.setStroke(centerStroke);
                } else {
                    g2d.setStroke(laneStroke);
                }
                g2d.draw(l.line);

                /*
                // where do line segments start and end?
                double eps = epsilon * (1 / zoom);
                g2d.fill(new Ellipse2D.Double(l.line.x1 - eps, l.line.y1 - eps, eps * 2, eps * 2));
                g2d.fill(new Ellipse2D.Double(l.line.x2 - eps, l.line.y2 - eps, eps * 2, eps * 2));
                */
            }
        }

        // Draw any turns we've got
        if (zoom >= (Double) Util.cfg.get("zoomThreshold") && mouseover != null) {
            draw_intersection(g2d, mouseover);
        }

        // Draw agents.
        for (Agent a : sim.getAgents()) {
            Coordinate at = a.at();
            double eps = epsilon * (1 / zoom);
            if (markedAgents.contains(new Integer(a.id))) {
                g2d.setColor(Color.RED);
            } else if (a.locked()) {
                g2d.setColor(Color.GREEN);
            } else {
                g2d.setColor(Color.CYAN);
            }
            g2d.fill(new Ellipse2D.Double(at.x - eps, at.y - eps, eps * 2, eps * 2));
        }

        // Draw map boundaries
        g2d.setColor(Color.BLUE);
        g2d.draw( new Line2D.Double(0, 0, width, 0) );
        g2d.draw( new Line2D.Double(0, height, width, height) );
        g2d.draw( new Line2D.Double(0, 0, 0, height) );
        g2d.draw( new Line2D.Double(width, 0, width, height) );

        // Draw epsilon cursor
        if ((Boolean) Util.cfg.get("cursor-bubble")) {
            g2d.setColor(Color.RED);
            double eps = epsilon * (1 / zoom);
            Ellipse2D.Double box = new Ellipse2D.Double(
                screenToMapX(mouseAtX) - eps,
                screenToMapY(mouseAtY) - eps,
                eps * 2,
                eps * 2
            );
            g2d.fill(box);
        }

        //System.out.printf("Rendered %d / %d line segs\n", cntDone, cntDone + cntSkipped);
        //System.out.println("  " + seen);
    }

    // TODO lots of the trig can probably be optimized, done once, etc
    public static Shape draw_arrow (Edge.Line line, int size) {
        // TODO enum for size
        double width, height;   // how far out is the tip, and how tall is the arrow?
        if (size == 3) {
            width = 0.25;
            height = 1.0;
        } else if (size == 2) {
            width = 0.15;
            height = 0.75;
        } else {
            width = 0.1;
            height = 0.5;
        }

        Coordinate mid = line.midPt();
        double theta = line.angle();
        double x = mid.x + (height * Math.cos(theta));
        double y = mid.y - (height * Math.sin(theta));   // y inversion

        // Perpendiculous!
        double thetaPerp1 = theta + (Math.PI / 2);
        double cosPerp1 = width * Math.cos(thetaPerp1);
        double sinPerp1 = -1 * width * Math.sin(thetaPerp1);    // y inversion
        double thetaPerp2 = theta - (Math.PI / 2);
        double cosPerp2 = width * Math.cos(thetaPerp2);
        double sinPerp2 = -1 * width * Math.sin(thetaPerp2);    // y inversion

        Path2D.Double arrow = new Path2D.Double();
        arrow.moveTo(x, y);
        arrow.lineTo(mid.x + cosPerp1, mid.y + sinPerp1);
        arrow.lineTo(mid.x + cosPerp2, mid.y + sinPerp2);

        return (Shape) arrow;
    }

    @Override int canvasWidth () {
        return (int) map.getWidth();
    }

    @Override int canvasHeight () {
        return (int) map.getHeight();
    }

    // Warning: kind of slow. use a quadtree or something to speedup.
    @Override void updateCursor (double x, double y) {
        // is this a good way of doing mouseover? :D
        double eps = epsilon * (1 / zoom);
        Rectangle2D.Double box = new Rectangle2D.Double(x - eps, y - eps, eps * 2, eps * 2);
        currentRoad = ROAD_LBL;
        mouseover = null;
        turn_n = -1;

        Edge e = null;
        for (ScreenLine l : matchLines(box)) {
            if (l.line.intersects(box)) {
                if (l.road != null) {
                    currentRoad = l.road.toString();
                } else {
                    currentRoad = l.edge.toString() + ": " + l.edge.length() + "m";
                    mouseover = l.edge;
                    //l.edge.describeQueue();
                    e = l.edge;
                }
                break;
            }
        }

        if (pickMode) {
            if (pickWhich == 0) {
                fromE = e;
            } else if (pickWhich == 1) {
                toE = e;
            }
        }


        if ((Boolean) Util.cfg.get("cursor-bubble")) {
            repaint();
        }
    }

    JPanel header;
    JLabel zoomLbl, cntLbl, timeLbl, roadLbl;
    JLabel interHelp = new JLabel("Press ESCAPE to cancel, SPACE to confirm"), interFrom, interTo;

    @Override public JPanel make_header () {
        JPanel normal, intersection;

        normal = new JPanel();
        normal.setLayout( new BoxLayout(normal, BoxLayout.X_AXIS) );
        normal.add( zoomLbl = new JLabel("...") );
        normal.add( Box.createHorizontalGlue() );
        normal.add( cntLbl = new JLabel("...") );
        normal.add( Box.createHorizontalGlue() );
        normal.add( timeLbl = new JLabel("...") );
        normal.add( Box.createHorizontalGlue() );
        normal.add( roadLbl = new JLabel("...") );
        roadLbl.setFont( new Font("Monospaced", 0, 15) );

        intersection = new JPanel();
        intersection.setLayout( new BoxLayout(intersection, BoxLayout.X_AXIS) );
        intersection.add(interHelp);
        intersection.add( Box.createHorizontalGlue() );
        intersection.add( interFrom = new JLabel("...") );
        intersection.add( Box.createHorizontalGlue() );
        intersection.add( interTo = new JLabel("...") );

        header = new JPanel( new CardLayout() );
        header.add(normal, "NORMAL");
        header.add(intersection, "INTERSECTION");

        return header;
    }

    @Override public void handleKey (int key) {
        switch (key) {
            case KeyEvent.VK_SPACE:
                if (pickMode) {
                    if (pickWhich == 0 && fromE != null) {
                        pickWhich++;
                    } else if (pickWhich == 1 && toE != null) {
                        if (pickAgent) {
                            // spawning an agent
                            if (fromE == toE) {
                                dbug("cant loop, idiot");
                            } else {
                                sim.addAgent(fromE, toE);
                            }
                            toE = null;
                        } else {
                            // pathfinding
                            showPathfind(fromE, toE, pathfindType.equals("A*"));
                        }
                        showHeader("NORMAL");
                        pickMode = false;
                        repaint();
                    }
                }
                break;
            case KeyEvent.VK_ESCAPE:
                if (pickMode) {
                    showHeader("NORMAL");
                    pickMode = false;
                }
                break;
            case KeyEvent.VK_S:
                sim.step(1000); // an entire second!
                break;
            case KeyEvent.VK_P:
                sim.toggle();
                break;
            case KeyEvent.VK_TAB:
                if (mouseover != null) {
                    turn_n++;
                    if (turn_n >= mouseover.getDestinations().size()) {
                        turn_n = 0;
                    }
                    repaint();  // this'll call draw_intersections
                }
                break;
        }
    }

    private static final String ROAD_LBL = "                                             ";
    @Override void updateStatus () {
        if (pickMode) {
            interFrom.setText( "From: " + fromE );
            interTo.setText( "To: " + toE );
        } else {
            zoomLbl.setText( String.format("Zoom: %.1f", zoom) );
            cntLbl.setText( String.format("Agents: %d", sim.agentCnt()) );
            timeLbl.setText( String.format("Time: %.2f", sim.getTick()) );
            // pad or truncate currentRoad
            String trunc = currentRoad + ROAD_LBL;
            roadLbl.setText("Road: " + trunc.substring(0, 45));
        }
    }

    @Override void callback (String cmd, String arg1, String arg2) {
        if (cmd.equals("hilite")) {
            highlightType = arg1;
            repaint();
        } else if (cmd.equals("reset-pick")) {
            showHeader("INTERSECTION");
            pickMode = true;
            pickAgent = false;
            pickWhich = 0;
            fromE = null;
            toE = null;
            pathfindType = arg1;
            route.clear();
            repaint();
        } else if (cmd.equals("clear-route")) {
            route.clear();
            repaint();
        } else if (cmd.equals("spawn-agent")) {
            showHeader("INTERSECTION");
            pickMode = true;
            pickAgent = true;
            pickWhich = 0;
            fromE = null;
            toE = null;
            route.clear();
            repaint();
        } else if (cmd.equals("spawn-army")) {
            sim.spawnArmy( Util.cfg_int("army-size") );
        } else if (cmd.equals("experiment")) {
            sim.runExperiment();
        } else if (cmd.equals("toggle")) {
            sim.toggle();
        } else if (cmd.equals("find-rd")) {
            int id = -1;
            try {
                id = Integer.parseInt(arg1);
            } catch (NumberFormatException e) {}

            boolean found1 = false, found2 = false;
            String rd1 = "", rd2 = "";
            Vertex match = null;
            SEARCH: for (Vertex v : map.getVertices()) {
                match = v;
                for (Edge e : v.allEdges()) {
                    if (id != -1) {
                       if (e.id == id) {
                        rd1 = rd2 = e.road.getName();
                        found1 = found2 = true;
                        break SEARCH;
                       }
                    } else {
                        if (e.road.getName().toLowerCase().contains(arg1)) {
                            found1 = true;
                            rd1 = e.road.getName();
                            if (found2) {
                                break SEARCH;
                            }
                        }
                        if (e.road.getName().toLowerCase().contains(arg2)) {
                            found2 = true;
                            rd2 = e.road.getName();
                            if (found1) {
                                break SEARCH;
                            }
                        }
                    }
                }
            }
            if (found1 && found2) {
                dbug("Warping to %s and %s. Hands and feet inside at all times...", rd1, rd2);
                dbug("%s", map.worldToGPS(match.location));
                // TODO center it.
                //xOff = (v.location.x + (canvasWidth() / 2)) * zoom;
                //yOff = (v.location.y + (canvasHeight() / 2)) * zoom;
                xOff = match.location.x * zoom;
                yOff = match.location.y * zoom;
                repaint();
            } else {
                dbug("Sorry sir, I couldn't find that.");
            }
        } else if (cmd.equals("mark-agent")) {
            try {
                String[] inputs = JOptionPane.showInputDialog("Who to mark?").split(",");
                for (String in : inputs) {
                    markedAgents.add( Integer.parseInt(in) );
                }
            } catch (NumberFormatException e) {}
        } else if (cmd.equals("unmark-agent")) {
            try {
                String[] inputs = JOptionPane.showInputDialog("Who to unmark?").split(",");
                for (String in : inputs) {
                    markedAgents.remove( new Integer(Integer.parseInt(in)) );
                }
            } catch (NumberFormatException e) {}
        } else if (cmd.equals("set-rng")) {
            try {
                long time = Long.parseLong( JOptionPane.showInputDialog("Seed the RNG god!") );
                Util.seedRNG(time);
            } catch (NumberFormatException e) {}
        }
    }

    void showPathfind (Edge from, Edge to, boolean best) {
        List<Edge> path = best ? map.pathfind_astar(from, to) : map.pathfind_naive(from, to);
        for (Edge step : path) {
            //System.out.println(step.edge + " ->");
            route.add(step);
        }
    }

    public void resetPick () {
    }

    // Or more accurately, the turns in an intersection
    private void draw_intersection (Graphics2D g2d, Edge lane) {
        if (turn_n == -1) {
            for (Turn to : lane.getDestinations()) {
                draw_turn(g2d, to, 0);
            }
        } else {
            draw_turn(g2d, lane.getDestinations().get(turn_n), 1);
        }
    }

    // TODO enum please!
    // modes: 0=dont showconflicts; 1=show conflicts; 2=we're a conflict, show just the line;
    // 3=we're a conflict, show arrow.

    // and possibly its conflicts
    private void draw_turn (Graphics2D g2d, Turn turn, int mode) {
        Edge lane = turn.from;
        // draw over the lines...
        if (mode == 1) {
            for (Turn wreck : turn.getConflicts()) {
                draw_turn(g2d, wreck, 2);
            }
        }

        Edge next = turn.to;
        Coordinate pt1 = lane.getEnd();
        Coordinate pt2 = lane.to().location;
        Coordinate pt3 = next.getStart();
        CubicCurve2D.Double curve = new CubicCurve2D.Double(
            pt1.x, pt1.y, pt2.x, pt2.y, pt2.x, pt2.y, pt3.x, pt3.y
        );

        if (mode == 2 || mode == 3) {
            g2d.setColor(Color.RED);
        } else {
            switch (turn.type) {
                case CROSS:         g2d.setColor(Color.WHITE);   break;
                case CROSS_MERGE:   g2d.setColor(Color.RED);     break;
                case RIGHT:         g2d.setColor(Color.GREEN);   break;
                case LEFT:          g2d.setColor(Color.ORANGE);  break;
                case UTURN:         g2d.setColor(Color.MAGENTA); break;
            }
        }
        if (mode != 2) {
            // and, I don't know, a nice arrow most of the time
            g2d.fill( draw_arrow(new Edge.Line(pt2.x, pt2.y, pt3.x,pt3.y), mode == 3 ? 1 : 2) );
        }
        if (mode != 3) {
            g2d.draw(curve);
        }

        // but draw conflict arrows on top
        if (mode == 1) {
            for (Turn wreck : turn.getConflicts()) {
                draw_turn(g2d, wreck, 3);
            }
        }
    }

    public void showHeader (String pane) {
        CardLayout chooser = (CardLayout) header.getLayout();
        chooser.show(header, pane);
    }
}
