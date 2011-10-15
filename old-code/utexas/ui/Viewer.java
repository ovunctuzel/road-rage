/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.ui;

import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import utexas.helper.Util;
import utexas.map.Graph;

// It's an ugly GUI!
public class Viewer extends JComponent {
	private static final long serialVersionUID = -7139358855627235486L;

	ScrollCanvas content;

    public static void main (String[] args) {
        JFrame frame = new JFrame("Map Viewer");
        frame.setSize(800, 600);
        frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
        frame.setContentPane( new Viewer(frame) );
        frame.setVisible(true);
    }

    Viewer (final JFrame frame) {
        setLayout( new BorderLayout() );
        content = new MapCanvas(this);

        add(content.make_header(), BorderLayout.NORTH);
        add(content, BorderLayout.CENTER);

        // Data
        content.useSource( Graph.load("dat/test.map") );

        // Menu
        JMenuBar menu = new JMenuBar();
        frame.setJMenuBar(menu);

        JMenu menuFile = new JMenu("File");
        JMenuItem itemFile1 = new JMenuItem("Process OSM map");
            itemFile1.addActionListener(new ActionListener() {
                public void actionPerformed (ActionEvent e) {
                    FileDialog fd = new FileDialog(frame, "Choose OSM");
                    fd.setFilenameFilter(new FilenameFilter () {
                        public boolean accept(File dir, String fn) {
                            return fn.toLowerCase().endsWith(".osm");
                        }
                    });
                    fd.setVisible(true);
                    if (fd.getFile() != null) {
                        String file = fd.getDirectory() + fd.getFile();
                        try {
                            Process p = Runtime.getRuntime().exec("./run a " + file);
                            BufferedReader in = new BufferedReader( new InputStreamReader( p.getInputStream() ) );
                                String line;
                                while ((line = in.readLine()) != null) {
                                    System.out.println(line);
                                }
                                content.useSource( Graph.load("dat/test.map") );
                        } catch (IOException err) {
                            System.out.println(err);
                        }
                    }
                }
            });
        JMenuItem itemFile2 = new JMenuItem("Configuration");
            itemFile2.addActionListener(new ActionListener() {
                public void actionPerformed (ActionEvent e) { ui_config(); }
            });
        JMenuItem itemFile3 = new JMenuItem("Quit");
            itemFile3.addActionListener(new ActionListener() {
                public void actionPerformed (ActionEvent e) {
                    System.exit(0);
                }
            });
        menuFile.add(itemFile1);
        menuFile.add(itemFile2);
        menuFile.addSeparator();
        menuFile.add(itemFile3);
        menu.add(menuFile);

        JMenu menuView = new JMenu("View");
        JMenu submenuView = new JMenu("Highlight Road Class");
        String[] roadTypes = {
            "null", "residential", "unclassified", "secondary",
            "motorway_link", "motorway", "trunk_link", "secondary_link", "primary_link",
            "tertiary", "primary", "service", "doomed"
        };
        
        JMenuItem viewNada = new JMenuItem("Clear all highlighting");
        viewNada.addActionListener(new ActionListener() {
            public void actionPerformed (ActionEvent e) { content.callback("hilite", "", ""); }
        });
        submenuView.add(viewNada);
        submenuView.addSeparator();

        for (String type : roadTypes) {
            JMenuItem typeItem = new JMenuItem(type);
            typeItem.addActionListener(new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("hilite", e.getActionCommand(), ""); }
            });
            submenuView.add(typeItem);
        }
        menuView.add(submenuView);
        menu.add(menuView);

        JMenu menuInteract = new JMenu("Interact");
        JMenuItem interactLocate = new JMenuItem("Locate...");
            interactLocate.addActionListener(new ActionListener() {
                public void actionPerformed (ActionEvent e) { locate_road(); }
            });
        menuInteract.add(interactLocate);
        JMenuItem interactPathfind1 = new JMenuItem("Pathfind - A*");
            interactPathfind1.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("reset-pick", "A*", ""); }
            });
        menuInteract.add(interactPathfind1);
        JMenuItem interactPathfind2 = new JMenuItem("Pathfind - Naive");
            interactPathfind2.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("reset-pick", "Naive", ""); }
            });
        menuInteract.add(interactPathfind2);
        JMenuItem interactClear = new JMenuItem("Clear Route");
            interactClear.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("clear-route", "", ""); }
            });
        menuInteract.add(interactClear);
        JMenuItem interactSpawn = new JMenuItem("Spawn Agent");
            interactSpawn.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("spawn-agent", "", ""); }
            });
        menuInteract.add(interactSpawn);
        JMenuItem interactArmy = new JMenuItem("Spawn Army!");
            interactArmy.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("spawn-army", "", ""); }
            });
        menuInteract.add(interactArmy);
        JMenuItem interactTest = new JMenuItem("EXPERIMENT!");
            interactTest.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("experiment", "", ""); }
            });
        menuInteract.add(interactTest);
        JMenuItem interactMark1 = new JMenuItem("Mark agent");
            interactMark1.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("mark-agent", "", ""); }
            });
        menuInteract.add(interactMark1);
        JMenuItem interactMark2 = new JMenuItem("Unmark agent");
            interactMark2.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("unmark-agent", "", ""); }
            });
        menuInteract.add(interactMark2);
        JMenuItem interactRNG = new JMenuItem("Set RNG");
            interactRNG.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("set-rng", "", ""); }
            });
        menuInteract.add(interactRNG);
        menu.add(menuInteract);

        JMenuItem play_btn = new JMenuItem("Play/Pause");
            play_btn.addActionListener( new ActionListener() {
                public void actionPerformed (ActionEvent e) { content.callback("toggle", "", ""); }
            });
        menu.add(play_btn);

        setup_config();
    }

    ///////////// cfg stuff now
    Object[] items, buttons = { "Apply", "Cancel" };

    // for boolean things.
    final static Map<String, String> booleans = new HashMap<String, String>();
    static {
        booleans.put("antialias", "Antialiased lines? (slower but nicer)");
        booleans.put("center-dash", "Dash the center yellow line?");
        booleans.put("cursor-bubble", "Draw a bubble around the cursor?");
        booleans.put("draw-arrows", "Draw orientation arrows on lanes?");
    }
    Map<String, ButtonGroup> buttongroups = new HashMap<String, ButtonGroup>();
    Map<String, JRadioButton> yesBtns     = new HashMap<String, JRadioButton>();
    Map<String, JRadioButton> noBtns      = new HashMap<String, JRadioButton>();

    // for numerical things.
    final static Map<String, String> doubles = new HashMap<String, String>();
    final static Map<String, String> ints    = new HashMap<String, String>();
    static {
        doubles.put("centerStroke",    "Width of center yellow line");
        doubles.put("laneStroke",      "Width of normal lane");
        doubles.put("thickLaneStroke", "Width of a lane in a route");
        doubles.put("laneWidth",       "How much wider a road background should be per lane");
        doubles.put("zoomThreshold",   "How close to zoom before drawing details");
        doubles.put("speedup",         "How much to scale all speed");

        ints.put("army-size",          "How big an army to spawn?");
    }
    Map<String, JSpinner> spinners = new HashMap<String, JSpinner>();

    // TODO color
    //JButton colorAbtn;

    private void setup_config () {
        List<Object> itemLs = new LinkedList<Object>();

        for (String bool : booleans.keySet()) {
            JRadioButton yes = new JRadioButton("Yes");
            JRadioButton no = new JRadioButton("No");

            yesBtns.put(bool, yes);
            noBtns.put(bool, no);

            ButtonGroup group = new ButtonGroup();
            buttongroups.put(bool, group);
            group.add(yes);
            group.add(no);

            itemLs.add(booleans.get(bool));
            itemLs.add(yes);
            itemLs.add(no);
        }

        double minDouble = Util.cfg_double("doubleMin");
        double maxDouble = Util.cfg_double("doubleMax");
        double stepDouble = Util.cfg_double("doubleStep");
        for (String num : doubles.keySet()) {
            double init = Util.cfg_double(num);
            JSpinner spinner = new JSpinner(new SpinnerNumberModel(init, minDouble, maxDouble, stepDouble));
            spinners.put(num, spinner);
            itemLs.add(doubles.get(num));
            itemLs.add(spinner);
        }

        int minInt = Util.cfg_int("intMin");
        int maxInt = Util.cfg_int("intMax");
        int stepInt = Util.cfg_int("intStep");
        for (String num : ints.keySet()) {
            int init = Util.cfg_int(num);
            JSpinner spinner = new JSpinner(new SpinnerNumberModel(init, minInt, maxInt, stepInt));
            spinners.put(num, spinner);
            itemLs.add(ints.get(num));
            itemLs.add(spinner);
        }

        /*JButton colorAbtn = new JButton("Change from RED");
        colorAbtn.addActionListener( new ActionListener() {
            public void actionPerformed (ActionEvent e) {
                JColorChooser.showDialog(null, "CHOOSE", colorA);
            }
        });
        */

        items = itemLs.toArray();
    }

    private void ui_config () {
        // setting current values
        for (String bool : booleans.keySet()) {
            boolean value = (Boolean) Util.cfg.get(bool);
            yesBtns.get(bool).setSelected(value);
            noBtns.get(bool).setSelected(!value);
        }

        for (String num : doubles.keySet()) {
            spinners.get(num).setValue( Util.cfg_double(num) );
        }

        for (String num : ints.keySet()) {
            spinners.get(num).setValue( Util.cfg_int(num) );
        }

        // run the cfg
        JOptionPane popup = new JOptionPane(
            items, JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION, null, buttons
        );
        popup.createDialog(this, "Configuration").setVisible(true);
        if (popup.getValue() == null || popup.getValue().equals("Cancel")) {
            return;
        }

        // update the world
        for (String bool : booleans.keySet()) {
            Util.cfg.put(bool, yesBtns.get(bool).getSelectedObjects() != null);
        }

        for (String num : doubles.keySet()) {
            Double value = (Double) spinners.get(num).getValue();
            if (Util.cfg.get(num) instanceof Float) {
                // TODO messy   
                Util.cfg.put(num, Float.parseFloat(value.toString()));
            } else {
                Util.cfg.put(num, value);
            }
        }

        for (String num : ints.keySet()) {
            Integer value = (Integer) spinners.get(num).getValue();
            Util.cfg.put(num, value);
        }

        content.repaint();
    }

    private void locate_road () {
        String road1 = (String) JOptionPane.showInputDialog(
            "Teleportation mechanism engaged. Where do you want to go today?"
        );
        String road2 = (String) JOptionPane.showInputDialog(
            "And the cross street?"
        );
        if (road1 != null && road1.length() > 0) {
            content.callback("find-rd", road1.toLowerCase(), road2.toLowerCase());
        }
    }
}
