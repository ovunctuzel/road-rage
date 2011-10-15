/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import utexas.helper.Util;

// A somewhat general component that interprets key and mouse controls to permit panning
// and zooming.
public abstract class ScrollCanvas extends JPanel implements MouseListener, MouseMotionListener, MouseWheelListener, KeyListener  {
	private static final long serialVersionUID = -3178883295149463583L;
	List<JLabel> lbls = new LinkedList<JLabel>();

    ScrollCanvas (JComponent context) {
        context.setFocusTraversalKeysEnabled(false);    // it eats tab. we want tab.
        context.addMouseListener(this);
        context.addMouseMotionListener(this);
        context.addMouseWheelListener(this);
        context.addKeyListener(this);
        context.setFocusable(true);
    }

    // Current viewing window
    protected double zoom = 1.0, xOff = 0.0, yOff = 0.0;
    // Controls
    private final int keySpeed = 10;
    private final double zoomStep = 0.1;

    // The thing that draws
    protected Object source;

    public void useSource (Object src) {
        source = src;
        usedSource();
        zoomed();
    }

    // so (xOff, yOff) is always the top-left corner of the screen
    public double screenToMapX (double x) {
        return (x + xOff) * (1 / zoom);
    }
    public double screenToMapY (double y) {
        return (y + yOff) * (1 / zoom);
    }

    public double mapToScreenX (double x) {
        return (x - xOff) * zoom;
    }
    public double mapToScreenY (double y) {
        return (y - yOff) * zoom;
    }

    // draws the stuff
    abstract void renderCanvas (Graphics2D g2d);
    abstract int canvasWidth ();
    abstract int canvasHeight ();
    abstract void updateCursor (double x, double y);
    abstract void updateStatus ();
    abstract void callback (String cmd, String arg1, String arg2);
    abstract JPanel make_header ();
    abstract void usedSource ();

    @Override public void paintComponent (Graphics g) {
        /*if (!Util.cfg_true("draw-lines")) {
            dbug("nvmnd");
            return;
        }*/

        // Clear
        super.paintComponent(g);
        setBackground(Color.LIGHT_GRAY);
        Graphics2D g2d = (Graphics2D) g;

        // The magic
        AffineTransform transform = new AffineTransform();
        transform.translate(-xOff, -yOff);
        transform.scale(zoom, zoom);
        g2d.transform(transform);
        
        if ((Boolean) Util.cfg.get("antialias")) {
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        } else {
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
        }

        renderCanvas(g2d);
    }

    // Controls
    @Override public void mouseExited   (MouseEvent e) {}
    @Override public void mouseEntered  (MouseEvent e) {}
    @Override public void mouseReleased (MouseEvent e) {
        fastMode = false;
    }
    @Override public void mouseClicked  (MouseEvent e) {}

    // Track so we can zoom more accurately
    double mouseAtX, mouseAtY;
    @Override public void mouseMoved (MouseEvent e) {
        Point pt = e.getPoint();
        mouseAtX = pt.x;
        mouseAtY = pt.y;

        updateCursor( screenToMapX(mouseAtX), screenToMapY(mouseAtY) );
        updateStatus();
    }

    double clickX, clickY;
    boolean fastMode = false;
    @Override public void mousePressed (MouseEvent e) {
        Point pt = e.getPoint();
        clickX = pt.x;
        clickY = pt.y;
        fastMode = e.getButton() == MouseEvent.BUTTON3;
    }

    @Override public void mouseDragged (MouseEvent e) {
        Point pt = e.getPoint();
        double dx = clickX - pt.x;
        double dy = clickY - pt.y;

        // TODO use scr <-> map formulas to figure out what this should be
        if (fastMode) {
            xOff += zoom * dx;
            yOff += zoom * dy;
        } else {
            xOff += dx;
            yOff += dy;
        }

        fix();
        repaint();

        clickX = pt.x;
        clickY = pt.y;

        // Well, the mouse moved, do that stuff again.
        mouseAtX = pt.x;
        mouseAtY = pt.y;
        updateCursor( screenToMapX(mouseAtX), screenToMapY(mouseAtY) );
        updateStatus();
    }

    @Override public void mouseWheelMoved (MouseWheelEvent e) {
        double oldZoom = zoom;

        int dz = e.getWheelRotation();
        zoom -= zoomStep * dz * 10;
        if (zoom < zoomStep) {
            zoom = zoomStep; // cap it somewhere.
        }

        // I want screenToMap of mouseAt to still point to the same thing after zooming.
        // The math comes from solving a proportion. TODO comment derivation
        xOff = ((zoom / oldZoom) * (mouseAtX + xOff)) - mouseAtX;
        yOff = ((zoom / oldZoom) * (mouseAtY + yOff)) - mouseAtY;

        zoomed();
        fix();
        repaint();
        updateStatus();
    }

    // Fix the coordinates to fit the current buffer.
    private void fix () {
        double x2 = screenToMapX( (double) getWidth() );
        double y2 = screenToMapY( (double) getHeight() );

        double xFix = canvasWidth() - x2;
        if (xFix < 0) {
            xOff += xFix;
        }
        double yFix = canvasHeight() - y2;
        if (yFix < 0) {
            yOff += yFix;
        }

        xOff = Math.max(0, xOff);
        yOff = Math.max(0, yOff);
    }

    public void keyPressed (KeyEvent e) {
        int key = e.getKeyCode();
        switch (key) {
            case KeyEvent.VK_UP:
                yOff -= fastMode ? zoom * keySpeed : keySpeed;
                break;
            case KeyEvent.VK_DOWN:
                yOff += fastMode ? zoom * keySpeed : keySpeed;
                break;
            case KeyEvent.VK_LEFT:
                xOff -= fastMode ? zoom * keySpeed : keySpeed;
                break;
            case KeyEvent.VK_RIGHT:
                xOff += fastMode ? zoom * keySpeed : keySpeed;
                break;
            case KeyEvent.VK_R:
                //double oldZoom = zoom;
                zoom = 1.0;
                xOff = 0;
                yOff = 0;
                zoomed();
                break;
            default:
                handleKey(key);
                return;
        }

        // Standard postlude
        fix();
        repaint();
        updateStatus();
    }
    public void handleKey (int key) {}  // Override if desired
    public void keyReleased (KeyEvent e) {}
    public void keyTyped (KeyEvent e) {}

    // zoom level changed, so redraw our buffered image
    BufferedImage img;
    private void zoomed () {
        /*
        dbug("zooming...");
        img = (BufferedImage) createImage((int) (canvasWidth() * zoom), (int) (canvasHeight() * zoom));
        if (img != null) {
            Graphics2D g2d = img.createGraphics();
            renderCanvas(g2d);
            dbug("zoomed.");
        } else {
            dbug("img was null.. try again when we have a display?");
        }
        */
    }
}
