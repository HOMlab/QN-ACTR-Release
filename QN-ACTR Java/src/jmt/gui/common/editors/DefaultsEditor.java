/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */

package jmt.gui.common.editors;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SpringLayout;
import javax.swing.WindowConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.framework.gui.layouts.SpringUtilities;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.routingStrategies.RoutingStrategy;

/**
 * <p>Title: Defaults Editor</p>
 * <p>Description: A modal or non-modal editor used to setup default values for every
 * parameter of the model.</p>
 * 
 * @author Bertoli Marco
 *         Date: 12-lug-2005
 *         Time: 16.18.35
 *         
 * Modified by Ashanka (July 2010)
 * Desc: Added new defaults control of a Random CheckBox.
 */
public class DefaultsEditor extends JDialog implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Constants used to select which parameters should be shown
	 */
	public static final int JMODEL = 0;
	public static final int JSIM = 1;

	protected static final int BORDERSIZE = 20;
	protected static final int MINIMUM_TIME = 5; // Minimum simulation duration

	protected int target;

	// --- Constructors --------------------------------------------------------------------------------
	/**
	 * Construct a new DefaultsEditor in a non-modal dialog
	 * @param target Used to specify to show specific parameters for JMODEL or JSIM
	 */
	public DefaultsEditor(int target) {
		super();
		initWindow(target);
	}

	/**
	 * Construct a new DefaultsEditor in a modal dialog
	 * @param owner owner Frame
	 * @param target Used to specify to show specific parameters for JMODEL or JSIM
	 */
	public DefaultsEditor(Frame owner, int target) {
		super(owner, true);
		initWindow(target);
	}

	/**
	 * Construct a new DefaultsEditor in a modal dialog
	 * @param owner owner Dialog
	 * @param target Used to specify to show specific parameters for JMODEL or JSIM
	 */
	public DefaultsEditor(Dialog owner, int target) {
		super(owner, true);
		initWindow(target);
	}

	/**
	 * Returns a new instance of DefaultsEditor, given parent container (used to find
	 * top level Dialog or Frame to create this dialog as modal)
	 * @param parent any type of container contained in a Frame or Dialog
	 * @param target Used to specify to show specific parameters for JMODEL or JSIM
	 * @return new instance of DefaultsEditor
	 */
	public static DefaultsEditor getInstance(Container parent, int target) {
		// Finds top level Dialog or Frame to invoke correct costructor
		while (!(parent instanceof Frame || parent instanceof Dialog)) {
			parent = parent.getParent();
		}

		if (parent instanceof Frame) {
			return new DefaultsEditor((Frame) parent, target);
		} else {
			return new DefaultsEditor((Dialog) parent, target);
		}
	}

	// -------------------------------------------------------------------------------------------------

	// --- Actions performed by buttons and EventListeners ---------------------------------------------
	// When okay button is pressed
	protected AbstractAction okayAction = new AbstractAction("Okay") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Closes this window and saves all changes");
		}

		public void actionPerformed(ActionEvent e) {
			Defaults.save();
			DefaultsEditor.this.dispose();
		}
	};

	// When cancel button is pressed
	protected AbstractAction cancelAction = new AbstractAction("Cancel") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Closes this window discarding all changes");
		}

		public void actionPerformed(ActionEvent e) {
			Defaults.reload();
			DefaultsEditor.this.dispose();
		}
	};

	// When reset button is pressed
	protected AbstractAction resetAction = new AbstractAction("Reset") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Reverts all values to original ones");
		}

		public void actionPerformed(ActionEvent e) {
			// Unregister all stringListener to avoid strange random things
			JTextField tmp;
			while (!registeredStringListener.isEmpty()) {
				tmp = registeredStringListener.remove(0);
				tmp.removeFocusListener(stringListener);
				tmp.removeKeyListener(stringListener);
			}
			Defaults.revertToDefaults();
			DefaultsEditor.this.getContentPane().removeAll();
			DefaultsEditor.this.initComponents(target);
			DefaultsEditor.this.show();
		}
	};

	/**
	 * Listener used to set parameters (associated to param_panel's JTextFields).
	 * Parameters are set when JTextField loses focus or ENTER key is pressed.
	 */
	protected class inputListener implements KeyListener, FocusListener {
		/**
		 * Update values of Defaults fields
		 */
		protected void updateValues(Object source) {
			JTextField src = (JTextField) source;
			Defaults.set(src.getName(), src.getText());
		}

		public void focusLost(FocusEvent e) {
			updateValues(e.getSource());
		}

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ENTER) {
				updateValues(e.getSource());
				e.consume();
			}
		}

		public void focusGained(FocusEvent e) {
		}

		public void keyReleased(KeyEvent e) {
		}

		public void keyTyped(KeyEvent e) {
		}
	}

	protected inputListener stringListener = new inputListener();
	// Vector that contains every compoent in which a StringListener has been registered
	// It is used as the focus listener can do "random" things while reverting values
	// to original ones
	protected Vector<JTextField> registeredStringListener = new Vector<JTextField>();

	// -------------------------------------------------------------------------------------------------

	/**
	 * Initialize parameters of the window (size, title)... Then calls <code>initComponents</code>
	 * @param target target application (JMODEL or JSIM)
	 */
	protected void initWindow(int target) {
		this.target = target;
		// Sets default title, close operation and dimensions
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		this.setTitle("Editing Default Values...");
		int width = 648, height = 480;

		// Centers this dialog on the screen
		Dimension scrDim = Toolkit.getDefaultToolkit().getScreenSize();
		this.setBounds((scrDim.width - width) / 2, (scrDim.height - height) / 2, width, height);
		// If user closes this window, act as cancel and reloads saved parameters
		this.addWindowStateListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				Defaults.reload();
			}
		});
		initComponents(target);
	}

	/**
	 * Initialize all graphic objects
	 * @param target target application (JMODEL or JSIM)
	 */
	protected void initComponents(int target) {
		// Creates a main panel and adds margins to it
		JPanel mainpanel = new JPanel(new BorderLayout());
		mainpanel.setLayout(new BorderLayout());
		mainpanel.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));
		this.getContentPane().add(mainpanel, BorderLayout.CENTER);

		// Adds bottom_panel to contentpane
		JPanel bottom_panel = new JPanel(new FlowLayout());
		this.getContentPane().add((bottom_panel), BorderLayout.SOUTH);

		// Adds Okay button to bottom_panel
		JButton okaybutton = new JButton(okayAction);
		bottom_panel.add(okaybutton);

		// Adds Cancel button to bottom_panel
		JButton cancelbutton = new JButton(cancelAction);
		bottom_panel.add(cancelbutton);

		// Adds Cancel button to bottom_panel
		JButton resetbutton = new JButton(resetAction);
		bottom_panel.add(resetbutton);

		// Creates param_panel
		JPanel param_panel = new JPanel(new GridLayout(1, 4));
		mainpanel.add(new JScrollPane(param_panel), BorderLayout.CENTER);

		Map<String, String> tmpMap;

		// Class Parameters
		JPanel class_panel = new JPanel(new SpringLayout());
		int classpanelnum = 0; // Counts all inserted elements
		class_panel.setBorder(new TitledBorder(new EtchedBorder(), "Class parameters"));

		// Name
		addInputString("Name", "className", class_panel);
		classpanelnum++;

		// Type
		tmpMap = new TreeMap<String, String>();
		tmpMap.put("" + CommonConstants.CLASS_TYPE_CLOSED, "Closed");
		tmpMap.put("" + CommonConstants.CLASS_TYPE_OPEN, "Open");
		addInputCombo("Type", "classType", class_panel, tmpMap);
		classpanelnum++;

		// Priority
		addInputSpinner("Priority", "classPriority", class_panel, 0);
		classpanelnum++;

		// Population
		addInputSpinner("Population (closed classes)", "classPopulation", class_panel, 1);
		classpanelnum++;

		// Distribution
		addInputDistribution("Distribution (open classes)", "classDistribution", class_panel);
		classpanelnum++;

		SpringUtilities.makeCompactGrid(class_panel, classpanelnum, 2, //rows, cols
				6, 6, //initX, initY
				6, 6);//xPad, yPad
		param_panel.add(class_panel);

		// Station Parameters
		JPanel station_panel = new JPanel(new SpringLayout());
		int stationpanelnum = 0; // Counts all inserted elements
		station_panel.setBorder(new TitledBorder(new EtchedBorder(), "Station parameters"));

		// Name (JSIM only)
		if (target == JSIM) {
			addInputString("Name", "stationName", station_panel);
			stationpanelnum++;
		}

		// Station type (JSIM only)
		if (target == JSIM) {
			tmpMap = new TreeMap<String, String>();
			tmpMap.put(STATION_TYPE_SERVER, STATION_NAMES.get(STATION_TYPE_SERVER));
			tmpMap.put(STATION_TYPE_DELAY, STATION_NAMES.get(STATION_TYPE_DELAY));
			tmpMap.put(STATION_TYPE_ROUTER, STATION_NAMES.get(STATION_TYPE_ROUTER));
			tmpMap.put(STATION_TYPE_FORK, STATION_NAMES.get(STATION_TYPE_FORK));
			tmpMap.put(STATION_TYPE_JOIN, STATION_NAMES.get(STATION_TYPE_JOIN));
			addInputCombo("Type", "stationType", station_panel, tmpMap);
			stationpanelnum++;
		}

		// Queue Capacity
		addInputInfSpinner("Queue Capacity", "stationCapacity", station_panel, 1, -1);
		stationpanelnum++;

		// Number of Servers
		addInputSpinner("Number of Servers", "stationServers", station_panel, 1);
		stationpanelnum++;

		// Queue Strategy
		addInputQueueStrategy("Queue Strategy", "stationQueueStrategy", station_panel);
		stationpanelnum++;

		// Service strategy
		addInputDistribution("Service Distribution", "stationServiceStrategy", station_panel);
		stationpanelnum++;

		// Delay service strategy
		addInputDistribution("Delay Service Distribution", "stationDelayServiceStrategy", station_panel);
		stationpanelnum++;

		// Routing strategy
		addInputRouting("Routing Strategy", "stationRoutingStrategy", station_panel);
		stationpanelnum++;

		// Fork Blocking
		addInputInfSpinner("Fork Capacity", "forkBlock", station_panel, 1, -1);
		stationpanelnum++;

		// Drop rule
		addInputDropRule("Drop Rule", "dropRule", station_panel);
		stationpanelnum++;

		// Number of jobs created for each fork link
		addInputSpinner("Fork degree", "forkJobsPerLink", station_panel, 1);
		stationpanelnum++;

		SpringUtilities.makeCompactGrid(station_panel, stationpanelnum, 2, //rows, cols
				6, 6, //initX, initY
				6, 6);//xPad, yPad
		param_panel.add(station_panel);

		// Simulation Parameters
		JPanel sim_panel = new JPanel(new SpringLayout());
		int simpanelnum = 0; // Counts all inserted elements
		sim_panel.setBorder(new TitledBorder(new EtchedBorder(), "Simulation parameters"));

		// Measure Alpha
		addInput01Spinner("Confidence Interval Measure (0-1)", "measureAlpha", sim_panel);
		simpanelnum++;

		// Measure Precision
		addInput01Spinner("Max Relative Error Measure (0-1)", "measurePrecision", sim_panel);
		simpanelnum++;

		// Simulation Seed
		//addInputSpinner("Simulation seed", "simulationSeed", sim_panel, 1);
		addInputRandomSpinner("Simulation seed", "isSimulationSeedRandom", "simulationSeed", sim_panel, 1);
		simpanelnum++;

		// Maximum duration
		addInputInfSpinner("Maximum duration (sec)", "simulationMaxDuration", sim_panel, MINIMUM_TIME, -1);
		simpanelnum++;

		// Maximum duration
		addInputSpinner("Maximum number of samples", "maxSimulationSamples", sim_panel, 100000);
		simpanelnum++;

		//Polling interval
		simpanelnum++;
		addInputSpinner("Animation update interval (sec)", "simulationPolling", sim_panel, 1);

		// Francesco D'Aquino
		// Animation enabled/disabled
		simpanelnum++;
		this.addInputAnimationSpinner("Number of classes in queue animation", "isWithAnimation", "representableClasses", sim_panel, 1, 10);
		// end Francesco D'Aquino

		SpringUtilities.makeCompactGrid(sim_panel, simpanelnum, 2, //rows, cols
				6, 6, //initX, initY
				6, 6);//xPad, yPad

		param_panel.add(sim_panel);

		// Blocking region parameters
		JPanel block_panel = new JPanel(new SpringLayout());
		int blockpanelnum = 0; // Counts all inserted elements
		block_panel.setBorder(new TitledBorder(new EtchedBorder(), "Finite Capacity Region Parameters"));

		// Blocking Region name
		addInputString("Name", "blockingRegionName", block_panel);
		blockpanelnum++;

		// Region Capacity
		addInputInfSpinner("Global Region Capacity", "blockingMaxJobs", block_panel, 1, -1);
		blockpanelnum++;

		// Region Capacity per Class
		addInputInfSpinner("Region Capacity per Class", "blockingMaxJobsPerClass", block_panel, 1, -1);
		blockpanelnum++;

		// Drop rule
		addBooleanComboBox("Drop", "blockingDropPerClass", block_panel);
		blockpanelnum++;

		SpringUtilities.makeCompactGrid(block_panel, blockpanelnum, 2, //rows, cols
				6, 6, //initX, initY
				6, 6);//xPad, yPad
		param_panel.add(block_panel);
	}

	/**
	 * Adds an input field to insert a String
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInputString(String text, String property, Container cont) {
		JLabel label = new JLabel(text + ":");
		JTextField field = new JTextField(10);
		field.setName(property);
		label.setLabelFor(field);
		field.setText(Defaults.get(property));
		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
		field.addKeyListener(stringListener);
		field.addFocusListener(stringListener);
		registeredStringListener.add(field);
		cont.add(label);
		cont.add(field);
	}

	/**
	 * Adds an input field to select from a ComboBox
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 * @param values Map with internal value <-> showed value relations
	 */
	protected void addInputCombo(String text, final String property, Container cont, final Map<String, String> values) {
		JLabel label = new JLabel(text + ":");
		JComboBox combo = new JComboBox(values.values().toArray());
		combo.setName(property);
		label.setLabelFor(combo);
		combo.setSelectedItem(values.get(Defaults.get(property)));
		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		combo.setMaximumSize(new Dimension(combo.getMaximumSize().width, combo.getMinimumSize().height));
		combo.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				// As Map does not allows reverse mapping, scans the entire keyset to
				// find the key corresponding to a given object
				Object[] keys = values.keySet().toArray();
				for (Object key : keys) {
					if (values.get(key) == e.getItem()) {
						Defaults.set(property, (String) key);
					}
				}
			}
		});
		cont.add(label);
		cont.add(combo);
	}

	/**
	 * Adds an input field to chose a number from a Spinner
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 * @param minvalue minimum value allowed for this property
	 */
	protected void addInputSpinner(String text, final String property, Container cont, final int minvalue) {
		JLabel label;
		label = new JLabel(text + ":");
		final JSpinner spinner = new JSpinner();
		label.setLabelFor(spinner);
		spinner.setValue(Defaults.getAsInteger(property));
		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		spinner.setMaximumSize(new Dimension(spinner.getMaximumSize().width, spinner.getMinimumSize().height));
		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					spinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				//new number of classes
				int x = minvalue;
				try {
					x = ((Integer) spinner.getValue()).intValue();
				} catch (NumberFormatException nfe) {
					//null
				} catch (ClassCastException cce) {
					//null
				}
				if (x < minvalue) {
					x = minvalue;
				}
				spinner.setValue(new Integer(x));
				Defaults.set(property, Integer.toString(x));
			}
		});
		cont.add(label);
		cont.add(spinner);
	}

	/**
	 * Adds an input field to chose a number from a Spinner. Number is between 0 and 1
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInput01Spinner(String text, final String property, Container cont) {
		JLabel label;
		label = new JLabel(text + ":");
		final JSpinner spinner = new JSpinner();
		spinner.setModel(new SpinnerNumberModel(0.0, 0.0, 1.0, 0.01));
		label.setLabelFor(spinner);
		spinner.setValue(Defaults.getAsDouble(property));
		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		spinner.setMaximumSize(new Dimension(spinner.getMaximumSize().width, spinner.getMinimumSize().height));
		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					spinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				//new number of classes
				double x = 0;
				try {
					x = ((Double) spinner.getValue()).doubleValue();
				} catch (NumberFormatException nfe) {
					// null
				} catch (ClassCastException cce) {
					//null
				}
				if (x < 0) {
					x = 0;
				}
				if (x > 1) {
					x = 1;
				}
				spinner.setValue(new Double(x));
				Defaults.set(property, Double.toString(x));
			}
		});
		cont.add(label);
		cont.add(spinner);
	}

	/**
	 * Adds an input field to chose a for a Distribution.
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInputDistribution(String text, final String property, Container cont) {
		// Creates a Map with Distributions Names, then delegates addInputCombo to create graphical
		// components
		Map<String, String> distributions = new TreeMap<String, String>();
		Distribution[] all = Distribution.findAll();
		for (Distribution element : all) {
			distributions.put(element.getClass().getName(), element.getName());
		}
		addInputCombo(text, property, cont, distributions);
	}

	/**
	 * Adds an input field to chose a for a Routing strategy. Uses reflection to find all
	 * available strategies
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInputRouting(String text, final String property, Container cont) {
		// Creates a Map with Routing Strategies Names, then delegates addInputCombo to create
		// graphical components
		Map<String, String> distributions = new TreeMap<String, String>();
		RoutingStrategy[] all = RoutingStrategy.findAll();
		for (RoutingStrategy element : all) {
			distributions.put(element.getClass().getName(), element.toString());
		}
		addInputCombo(text, property, cont, distributions);
	}

	/**
	 * Adds an input field to chose a for a Queue strategy. Uses reflection to find all
	 * available strategies
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInputQueueStrategy(String text, final String property, Container cont) {
		// Creates a Map with Queue Strategies Names, then delegates addInputCombo to create
		// graphical components
		Map<String, String> distributions = new TreeMap<String, String>();
		distributions.put(CommonConstants.QUEUE_STRATEGY_FCFS, CommonConstants.QUEUE_STRATEGY_FCFS);
		distributions.put(CommonConstants.QUEUE_STRATEGY_LCFS, CommonConstants.QUEUE_STRATEGY_LCFS);
		addInputCombo(text, property, cont, distributions);
	}

	/**
	 * Adds an input field to chose a for a Drop Rule. Uses reflection to find all
	 * available strategies
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addInputDropRule(String text, final String property, Container cont) {
		// Creates a Map with Queue Strategies Names, then delegates addInputCombo to create
		// graphical components
		Map<String, String> distributions = new TreeMap<String, String>();
		Field[] fields = jmt.gui.jmodel.JMODELConstants.class.getFields();
		try {
			for (Field field : fields) {
				if (field.getName().startsWith("FINITE_")) {
					distributions.put(field.get(null).toString(), field.get(null).toString());
				}
			}
		} catch (IllegalAccessException ex) {
			System.err.println("A security manager has blocked reflection");
			ex.printStackTrace();
		}
		addInputCombo(text, property, cont, distributions);
	}

	/**
	 * Adds an input field to chose a number from a Spinner. A special infinite value can be chosen
	 * with a checkbox
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 * @param minvalue minimum value allowed for this property
	 * @param infvalue special value to be stored for infinite selection
	 */
	protected void addInputInfSpinner(String text, final String property, Container cont, final int minvalue, final int infvalue) {
		// This one is a clone of addInputSpinner but adds a checkbox to select Infinity
		JLabel label;
		label = new JLabel(text + ":");
		final JSpinner spinner = new JSpinner();
		final JCheckBox inf_button = new JCheckBox();
		inf_button.setText("Infinite");
		JPanel internal = new JPanel(new BorderLayout(5, 0));
		internal.add(inf_button, BorderLayout.EAST);
		internal.add(spinner, BorderLayout.CENTER);
		label.setLabelFor(internal);
		// If current default is infinity hides spinner and selects inf_button
		if (Defaults.getAsInteger(property).intValue() == infvalue) {
			spinner.setValue(new Integer(minvalue));
			spinner.setEnabled(false);
			inf_button.setSelected(true);
		} else {
			spinner.setValue(Defaults.getAsInteger(property));
		}
		// Adds a listener to support inf_button change events
		inf_button.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (inf_button.isSelected()) {
					spinner.setEnabled(false);
					Defaults.set(property, Integer.toString(infvalue));
				} else {
					spinner.setEnabled(true);
					Defaults.set(property, spinner.getValue().toString());
				}
			}
		});

		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		internal.setMaximumSize(new Dimension(internal.getMaximumSize().width, internal.getMinimumSize().height));

		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					spinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				//new number of classes
				int x = minvalue;
				try {
					x = ((Integer) spinner.getValue()).intValue();
				} catch (NumberFormatException nfe) {
					//null
				} catch (ClassCastException cce) {
					//null
				}
				if (x < minvalue) {
					x = minvalue;
				}
				spinner.setValue(new Integer(x));
				Defaults.set(property, Integer.toString(x));
			}
		});
		cont.add(label);
		cont.add(internal);
	}

	/**
	 * Adds a ComboBox to select a boolean property
	 * @param text text to be shown on a label
	 * @param property property to be changed in Defaults
	 * @param cont container where input field must be added
	 */
	protected void addBooleanComboBox(String text, final String property, Container cont) {
		JLabel label = new JLabel(text + ":");
		JComboBox combo = new JComboBox(new Object[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() });
		combo.setName(property);
		label.setLabelFor(combo);
		combo.setSelectedItem(Defaults.get(property));
		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		combo.setMaximumSize(new Dimension(combo.getMaximumSize().width, combo.getMinimumSize().height));
		combo.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				Defaults.set(property, (String) e.getItem());
			}
		});
		cont.add(label);
		cont.add(combo);
	}

	protected void addInputAnimationSpinner(String text, final String booleanProperty, final String valueProperty, Container cont,
			final int minvalue, final int maxvalue) {
		// This one is a clone of addInputSpinner but adds a checkbox to select Infinity
		JLabel label;
		label = new JLabel(text + ":");
		final JSpinner spinner = new JSpinner();
		final JCheckBox animation_button = new JCheckBox();
		animation_button.setText("Animation");
		JPanel internal = new JPanel(new BorderLayout(5, 0));
		internal.add(animation_button, BorderLayout.EAST);
		internal.add(spinner, BorderLayout.CENTER);
		label.setLabelFor(internal);
		// If current default is !animation hides spinner and deselects animation_button
		if (Defaults.getAsBoolean(booleanProperty).booleanValue()) {
			spinner.setEnabled(true);
			animation_button.setSelected(true);
		} else {
			spinner.setEnabled(false);
			animation_button.setSelected(false);
		}
		spinner.setValue(Defaults.getAsInteger(valueProperty));

		// Adds a listener to support animation_button change events
		animation_button.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (animation_button.isSelected()) {
					spinner.setEnabled(true);
					Defaults.set(booleanProperty, "true");
				} else {
					spinner.setEnabled(false);
					Defaults.set(booleanProperty, "false");
				}

			}
		});

		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		internal.setMaximumSize(new Dimension(internal.getMaximumSize().width, internal.getMinimumSize().height));

		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					spinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				//new number of represented classes
				int x = 0;
				try {
					x = ((Integer) spinner.getValue()).intValue();
				} catch (NumberFormatException nfe) {
					//null
				} catch (ClassCastException cce) {
					//null
				}
				if ((x < minvalue) || (x > maxvalue)) {
					x = Defaults.getAsInteger(valueProperty).intValue();
				}
				spinner.setValue(new Integer(x));
				Defaults.set(valueProperty, Integer.toString(x));
			}
		});
		cont.add(label);
		cont.add(internal);
	}
	
	//Added the Random Checkbox and a spinner.
	protected void addInputRandomSpinner(String text, final String booleanProperty, final String valueProperty, Container cont,
			final int minvalue) {
		JLabel label;
		label = new JLabel(text + ":");
		final JSpinner spinner = new JSpinner();
		final JCheckBox random_button = new JCheckBox();
		random_button.setText("Random");
		JPanel internal = new JPanel(new BorderLayout(5, 0));
		internal.add(random_button, BorderLayout.EAST);
		internal.add(spinner, BorderLayout.CENTER);
		label.setLabelFor(internal);
		// If current default is !animation hides spinner and deselects animation_button
		if (Defaults.getAsBoolean(booleanProperty).booleanValue()) {
			spinner.setEnabled(false);
			random_button.setSelected(true);
		} else {
			spinner.setEnabled(true);
			random_button.setSelected(false);
		}
		spinner.setValue(Defaults.getAsInteger(valueProperty));

		// Adds a listener to support animation_button change events
		random_button.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (random_button.isSelected()) {
					spinner.setEnabled(false);
					Defaults.set(booleanProperty, "true");
				} else {					
					spinner.setEnabled(true);
					Defaults.set(booleanProperty, "false");
				}

			}
		});

		// Sets maximum size to minimal one, otherwise springLayout will stretch this
		internal.setMaximumSize(new Dimension(internal.getMaximumSize().width, internal.getMinimumSize().height));

		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					spinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				//new number of classes
				int x = 0;
				try {
					x = ((Integer) spinner.getValue()).intValue();
				} catch (NumberFormatException nfe) {
					//null
				} catch (ClassCastException cce) {
					//null
				}
				if (x < minvalue) {
					x = minvalue;
				}
				spinner.setValue(new Integer(x));
				Defaults.set(valueProperty, Integer.toString(x));
			}
		});
		cont.add(label);
		cont.add(internal);
	}
}
