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
package jmt.gui.exact.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.DecimalFormat;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;

/**
 * <p>Title: What-If Analysis Panel</p>
 * <p>Description: This panel is used to show and parametrize what if analysis
 * for jMVA. This is really complex as I have to cope with horrible JMVA data structure
 * and many controls must be performed to correctly parametrize analysis.</p>
 *
 * @author Bertoli Marco
 *         Date: 26-mag-2006
 *         Time: 13.48.45
 */
public class WhatIfPanel extends WizardPanel implements ExactConstants, ForceUpdatablePanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ExactWizard wizard;
	private HoverHelp help;
	private JLabel description, stationLabel, classLabel, fromLabel, toLabel, iterationLabel;

	public static final String helpText = "<html>In this panel you can select a <em>control parameter</em> "
			+ "that will be used to performa a what-if analysis.<br>Model solution is iterated by changing selected "
			+ "parameter and graphical results will be shown.</html>";

	private static final String NO_ANALYSIS = "-- Disabled --";
	private static final String ALL_CLASSES = "-- All classes proportionally --";

	private static final String FROM_ALL = "From (%) : ";
	private static final String FROM_ARRIVAL = "From (job/s) : ";
	private static final String FROM_CUSTOMERS = "From (Ni) : ";
	private static final String FROM_DEMANDS = "From (s) : ";
	private static final String FROM_MIX = "From (\u03b2i) : ";
	private static final String TO_ALL = "To (%) : ";
	private static final String TO_ARRIVAL = "To (job/s) : ";
	private static final String TO_CUSTOMERS = "To (Ni) : ";
	private static final String TO_DEMANDS = "To (s) : ";
	private static final String TO_MIX = "To (\u03b2i) : ";

	private JComboBox type, stationName, className;
	private JTextField from, to, iterations;
	private ClassTable classTable;
	private JPanel tablePanel;

	private DecimalFormat intFormatter = new DecimalFormat("#"); // This is used to display population
	private DecimalFormat doubleFormatter = new DecimalFormat("#0.0###"); // This is used to display doubles

	// Data structures
	private ExactModel data;
	private TreeMap<String, Integer> classNames; // A map className --> index in source array
	private TreeMap<String, Integer> closedClassNames, openClassNames;
	private TreeMap<String, Integer> stationNames; // A map stationName --> index in source array without LD stations
	private double[] values;
	private String currentType, currentClass;
	private Vector<String> openClasses, closedClasses; // All open classes and closed classes names
	private Vector<String> modes = new Vector<String>(); // Available what-if types
	// Current value ('from' field)
	private double current;

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "What-if";
	}

	/**
	 * Creates a new What-If Panel, given parent ExactWizard
	 * @param ex
	 */
	public WhatIfPanel(ExactWizard ex) {
		this.wizard = ex;
		help = ex.getHelp();
		initComponents();
		sync();
	}

	/**
	 * retrive current data structure and updates shown components
	 */
	private void sync() {
		/* retrive current data structure */
		data = wizard.getData();

		// Adds all classes to classNames and closedClassNames/openClassNames arrays
		closedClassNames = new TreeMap<String, Integer>();
		closedClasses = new Vector<String>();
		closedClassNames.put(ALL_CLASSES, new Integer(-1));
		openClassNames = new TreeMap<String, Integer>();
		openClassNames.put(ALL_CLASSES, new Integer(-1));
		openClasses = new Vector<String>();
		classNames = new TreeMap<String, Integer>();
		classNames.put(ALL_CLASSES, new Integer(-1));
		for (int i = 0; i < data.getClasses(); i++) {
			String name = data.getClassNames()[i];
			Integer index = new Integer(i);
			classNames.put(name, index);
			if (data.getClassTypes()[i] == ExactConstants.CLASS_OPEN) {
				openClassNames.put(name, index);
				openClasses.add(name);
			} else {
				closedClassNames.put(name, index);
				closedClasses.add(name);
			}
		}

		// Removes LD stations
		stationNames = new TreeMap<String, Integer>();
		for (int i = 0; i < data.getStations(); i++) {
			if (data.getStationTypes()[i] != ExactConstants.STATION_LD) {
				stationNames.put(data.getStationNames()[i], new Integer(i));
			}
		}

		// Finds available analysis types
		modes.clear();
		type.removeAllItems();
		modes.add(NO_ANALYSIS);
		type.addItem(NO_ANALYSIS);
		// If model has some closed classes
		if (data.isClosed() || data.isMixed()) {
			modes.add(WHAT_IF_CUSTOMERS);
			type.addItem(WHAT_IF_CUSTOMERS);
		} else {
			type.addItem(GRAY_S + WHAT_IF_CUSTOMERS + " (only closed classes)" + GRAY_E);
		}

		// If model has some open classes
		if (data.isOpen() || data.isMixed()) {
			modes.add(WHAT_IF_ARRIVAL);
			type.addItem(WHAT_IF_ARRIVAL);
		} else {
			type.addItem(GRAY_S + WHAT_IF_ARRIVAL + " (only open classes)" + GRAY_E);
		}

		// If model has two closed classes, allow population mix
		if (closedClasses.size() == 2) {
			modes.add(WHAT_IF_MIX);
			type.addItem(WHAT_IF_MIX);
		} else {
			type.addItem(GRAY_S + WHAT_IF_MIX + " (only 2 closed classes)" + GRAY_E);
		}

		// If model has at least one not LD station, allows service demands
		if (stationNames.size() > 0) {
			modes.add(WHAT_IF_DEMANDS);
			type.addItem(WHAT_IF_DEMANDS);
		} else {
			type.addItem(GRAY_S + WHAT_IF_DEMANDS + " (only LI or Delay)" + GRAY_E);
		}

		// Selects correct type
		if (!data.isWhatIf() || data.getWhatIfType() == null || !modes.contains(data.getWhatIfType())) {
			type.setSelectedItem(NO_ANALYSIS);
		} else {
			type.setSelectedItem(data.getWhatIfType());
		}

		// Selects correct class
		if (data.getWhatIfClass() < 0 || data.getWhatIfClass() >= data.getClasses()) {
			className.setSelectedItem(ALL_CLASSES);
		} else {
			className.setSelectedItem(data.getClassNames()[data.getWhatIfClass()]);
		}

		// Selects correct station
		if (data.getWhatIfStation() >= 0 && data.getWhatIfStation() < data.getStations()) {
			stationName.setSelectedItem(data.getStationNames()[data.getWhatIfStation()]);
		} else if (stationName.getItemCount() > 0) {
			stationName.setSelectedIndex(0);
		}
		if (data.getWhatIfValues() != null) {
			values = data.getWhatIfValues();
		}
		setFromToIterations();
	}

	/**
	 * Sets values for from, to and iteration fields
	 */
	private void setFromToIterations() {
		// Gets type
		String type = (String) this.type.getSelectedItem();

		// Sets from and to values
		if (values != null && !type.equals(NO_ANALYSIS)) {
			// Percentage values
			if (classNames.get(className.getSelectedItem()).intValue() < 0) {
				from.setText(doubleFormatter.format(values[0] * 100));
				to.setText(doubleFormatter.format(values[values.length - 1] * 100));
			}
			// Normal values
			else {
				if (!type.equals(ExactConstants.WHAT_IF_CUSTOMERS)) {
					from.setText(doubleFormatter.format(values[0]));
					to.setText(doubleFormatter.format(values[values.length - 1]));
				} else { // Integer values
					from.setText(intFormatter.format(values[0]));
					to.setText(intFormatter.format(values[values.length - 1]));
				}
			}
			iterations.setText("" + values.length);
		}
	}

	/**
	 * Commits changes to data structure
	 */
	private void commit() {
		if (type.getSelectedItem().equals(NO_ANALYSIS)) {
			data.setWhatIfType(null);
			data.setWhatIfClass(-1);
			data.setWhatIfStation(-1);
			data.setWhatIfValues(null);
		} else {
			data.setWhatIfType((String) type.getSelectedItem());
			data.setWhatIfValues(values);
			// Sets selected class
			data.setWhatIfClass(classNames.get(className.getSelectedItem()).intValue());

			// Sets selected station if Service Demands is selected
			if (!data.getWhatIfType().equals(ExactConstants.WHAT_IF_DEMANDS)) {
				data.setWhatIfStation(-1);
			} else {
				data.setWhatIfStation(stationNames.get(stationName.getSelectedItem()).intValue());
			}
		}
	}

	/**
	 * Initialize gui of this panel
	 */
	private void initComponents() {
		setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		setLayout(new BorderLayout(5, 5));

		// Listener for From, To and Iteration fields
		inputListener listener = new inputListener();

		// Description label
		description = new JLabel(DESCRIPTION_WHATIF_NONE);
		description.setPreferredSize(new Dimension(220, 200));
		description.setVerticalAlignment(SwingConstants.NORTH);
		add(description, BorderLayout.WEST);

		// Builds panel with parameters
		JPanel paramPanel = new JPanel(new GridLayout(6, 2, 5, 5));
		// Control parameter
		paramPanel.add(new JLabel("Control Parameter :", SwingConstants.RIGHT));
		type = new JComboBox(new String[] { NO_ANALYSIS });
		type.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				updateType();
			}
		});
		help.addHelp(type, "Select control parameter for what-if analysis. If no parameter is selected, analysis is disabled.");
		paramPanel.add(type);
		// Station name
		stationLabel = new JLabel("Station :", SwingConstants.RIGHT);
		paramPanel.add(stationLabel);
		stationName = new JComboBox();
		stationName.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				updateLabels(false);
			}
		});
		help.addHelp(stationName, "Select at which station service demand should be modified.");
		paramPanel.add(stationName);
		// Class Name
		classLabel = new JLabel("Class :", SwingConstants.RIGHT);
		paramPanel.add(classLabel);
		className = new JComboBox(new String[] { ALL_CLASSES });
		className.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				updateLabels(false);
			}
		});
		help.addHelp(className, "Select reference class for current analysis. All classes can be selected too.");
		paramPanel.add(className);
		// From field
		fromLabel = new JLabel("From :", SwingConstants.RIGHT);
		paramPanel.add(fromLabel);
		from = new JTextField();
		from.addKeyListener(listener);
		from.addFocusListener(listener);
		help.addHelp(from, "Initial value for what-if analysis. This is actual value.");
		paramPanel.add(from);
		// To field
		toLabel = new JLabel("To :", SwingConstants.RIGHT);
		paramPanel.add(toLabel);
		to = new JTextField();
		to.addKeyListener(listener);
		to.addFocusListener(listener);
		help.addHelp(to, "Specify final value for what-if analysis (default value is 200% of actual value)");
		paramPanel.add(to);
		// Iteration number field
		iterationLabel = new JLabel("Steps (n. of executions) :", SwingConstants.RIGHT);
		paramPanel.add(iterationLabel);
		iterations = new JTextField();
		iterations.addKeyListener(listener);
		iterations.addFocusListener(listener);
		help.addHelp(iterations, "Specify number of executions to be performed (default value is 11)");
		paramPanel.add(iterations);
		JPanel tmpPanel = new JPanel(new BorderLayout()); // Used to pack fields
		tmpPanel.add(paramPanel, BorderLayout.NORTH);
		add(tmpPanel, BorderLayout.CENTER);
		// Adds class table
		classTable = new ClassTable();
		help.addHelp(classTable, "Classes characteristics. Values in red are currently selected for what-if analysis.");
		tablePanel = new JPanel(new BorderLayout());
		tablePanel.add(classTable);
		tablePanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
		tablePanel.setVisible(false);
		add(tablePanel, BorderLayout.SOUTH);
	}

	/**
	 * This method is called each time what-if analysis type is changed
	 */
	private void updateType() {
		String parameter = (String) type.getSelectedItem();
		if (currentType != null && currentType.equalsIgnoreCase(parameter)) {
			return;
		}

		// If an unavailable mode is selected, select previous one
		if (currentType != null && !modes.contains(parameter)) {
			type.setSelectedItem(currentType);
			return;
		}

		// Stores old selected class
		String selClassName = (String) className.getSelectedItem();

		if (parameter == null || parameter.equals(NO_ANALYSIS)) {
			description.setText(DESCRIPTION_WHATIF_NONE);
			// Hide all components
			stationLabel.setVisible(false);
			stationName.setVisible(false);
			classLabel.setVisible(false);
			className.setVisible(false);
			fromLabel.setVisible(false);
			from.setVisible(false);
			toLabel.setVisible(false);
			to.setVisible(false);
			iterationLabel.setVisible(false);
			iterations.setVisible(false);
			tablePanel.setVisible(false);
		} else {
			// Shows nearly all components (except station)
			stationLabel.setVisible(false);
			stationName.setVisible(false);
			classLabel.setVisible(true);
			className.setVisible(true);
			fromLabel.setVisible(true);
			from.setVisible(true);
			toLabel.setVisible(true);
			to.setVisible(true);
			iterationLabel.setVisible(true);
			iterations.setVisible(true);
			tablePanel.setVisible(true);
			// Disables from field
			from.setEnabled(false);
			Iterator<String> it;
			// Default help for 'from' and 'to' values (only changes for population mix)
			help.removeHelp(from);
			help.addHelp(from, "Initial value for what-if analysis. This is actual value.");
			help.removeHelp(to);
			help.addHelp(to, "Specify final value for what-if analysis (default value is 200% of actual value)");

			if (parameter.equals(WHAT_IF_ARRIVAL)) {
				// Sets open classes for selection
				className.removeAllItems();
				it = openClassNames.keySet().iterator();
				while (it.hasNext()) {
					className.addItem(it.next());
				}
				// Removes 'ALL_CLASS' if only a single open class was found
				if (openClassNames.size() == 2) {
					className.removeItem(ALL_CLASSES);
				}
				className.setSelectedItem(selClassName);
				// Shows correct help on item
				help.removeHelp(classTable);
				help.addHelp(classTable, "Initial arrival rates. Values in red are currently selected for what-if analysis.");
			} else if (parameter.equals(WHAT_IF_CUSTOMERS)) {
				// Sets closed class for selection
				className.removeAllItems();
				it = closedClassNames.keySet().iterator();
				while (it.hasNext()) {
					className.addItem(it.next());
				}
				// Removes 'ALL_CLASS' if only a single closed class was found
				if (closedClassNames.size() == 2) {
					className.removeItem(ALL_CLASSES);
				}
				className.setSelectedItem(selClassName);
				// Shows correct help on item
				help.removeHelp(classTable);
				help
						.addHelp(classTable,
								"Initial number of customers and population mix values (\u03b2i = Ni / N). Values in red are currently selected for what-if analysis.");
			} else if (parameter.equals(WHAT_IF_MIX)) {
				// Enables 'from' field
				from.setEnabled(true);
				// Sets the two closed class for selection
				className.removeAllItems();
				it = closedClassNames.keySet().iterator();
				while (it.hasNext()) {
					className.addItem(it.next());
				}
				className.removeItem(ALL_CLASSES);
				className.setSelectedItem(selClassName);
				// Shows correct help on item
				help.removeHelp(classTable);
				help
						.addHelp(classTable,
								"Initial number of customers and population mix values (\u03b2i = Ni / N). Values in red are currently selected for what-if analysis.");
				help.removeHelp(from);
				help.addHelp(from, "Initial value corresponding to an integer, not null, number of customers.");
				help.removeHelp(to);
				help.addHelp(to, "Final value corresponding to an integer, not null, number of customers.");
			} else if (parameter.equals(WHAT_IF_DEMANDS)) {
				stationLabel.setVisible(true);
				stationName.setVisible(true);
				// Sets all classes for selection
				className.removeAllItems();
				it = classNames.keySet().iterator();
				while (it.hasNext()) {
					className.addItem(it.next());
				}
				// Removes 'ALL_CLASS' if only a single class was found
				if (classNames.size() == 2) {
					className.removeItem(ALL_CLASSES);
				}
				className.setSelectedItem(selClassName);
				// Sets all non-ld stations for selection
				stationName.removeAllItems();
				it = stationNames.keySet().iterator();
				while (it.hasNext()) {
					stationName.addItem(it.next());
				}
				stationName.setSelectedIndex(0);
				// Shows correct help on item
				help.removeHelp(classTable);
				help.addHelp(classTable, "Initial service demands. Values in red are currently selected for what-if analysis.");
			}
			updateLabels(true);
		}
		currentType = parameter;
	}

	/**
	 * This method is called each time selected class is changed
	 * @param changedType tells if type of what-if analysis was changed too.
	 */
	private void updateLabels(boolean changedType) {
		String selType = (String) type.getSelectedItem();
		if (changedType || (currentClass != null && className.getSelectedItem() != null)) {
			if (className.getSelectedItem().equals(ALL_CLASSES)) {
				fromLabel.setText(FROM_ALL);
				toLabel.setText(TO_ALL);
				values = new double[] { 1.0, 2.0 };
				current = 1.0;
				// Sets the correct description
				if (selType.equals(WHAT_IF_ARRIVAL)) {
					description.setText(DESCRIPTION_WHATIF_ARRIVAL_ALL);
				} else if (selType.equals(WHAT_IF_CUSTOMERS)) {
					description.setText(DESCRIPTION_WHATIF_CUSTOMERS_ALL);
				} else if (selType.equals(WHAT_IF_DEMANDS)) {
					description.setText(DESCRIPTION_WHATIF_DEMANDS_ALL);
				}
			} else {
				// Finds selected values
				int selClass = classNames.get(className.getSelectedItem()).intValue();
				int selStation;
				Object st = stationName.getSelectedItem();
				if (st != null) {
					selStation = stationNames.get(st).intValue();
				} else {
					selStation = 0;
				}
				if (selType.equals(ExactConstants.WHAT_IF_DEMANDS)) {
					description.setText(DESCRIPTION_WHATIF_DEMANDS_ONE);
					fromLabel.setText(FROM_DEMANDS);
					toLabel.setText(TO_DEMANDS);
					// Finds current service demand for selected station
					current = data.getServiceTimes()[selStation][selClass][0] * data.getVisits()[selStation][selClass];
					if (current <= 0) {
						current = 1.0;
					}
					values = new double[] { current, current * 2 };
				} else if (selType.equals(ExactConstants.WHAT_IF_ARRIVAL)) {
					description.setText(DESCRIPTION_WHATIF_ARRIVAL_ONE);
					fromLabel.setText(FROM_ARRIVAL);
					toLabel.setText(TO_ARRIVAL);
					// Finds current arrival rate for selected class
					current = data.getClassData()[selClass];
					if (current <= 0) {
						current = 1.0;
					}
					values = new double[] { current, current * 2 };
				} else if (selType.equals(ExactConstants.WHAT_IF_CUSTOMERS)) {
					description.setText(DESCRIPTION_WHATIF_CUSTOMERS_ONE);
					fromLabel.setText(FROM_CUSTOMERS);
					toLabel.setText(TO_CUSTOMERS);
					// Finds current number of customers for selected class
					current = data.getClassData()[selClass];
					if (current <= 0) {
						current = 1.0;
					}
					values = new double[] { current, current * 2 };
				} else if (selType.equals(ExactConstants.WHAT_IF_MIX)) {
					description.setText(DESCRIPTION_WHATIF_MIX);
					fromLabel.setText(FROM_MIX);
					toLabel.setText(TO_MIX);
					values = new double[] { 0.0, 1.0 };
					current = 0.0;
				}
			}
			// Shows new values
			setFromToIterations();
			iterations.setText("11");
			updateFields();

			// Updates classTable
			classTable.update();
		}
		currentClass = (String) className.getSelectedItem();
	}

	/**
	 * This function is called each time from, to or iterations field is changed.
	 */
	private void updateFields() {
		String type = (String) this.type.getSelectedItem();
		double from = current;
		// From field is 'current' value if parsing fails.
		try {
			from = Double.parseDouble(this.from.getText());
			if (from < 0) {
				from = 0.0;
			}
		} catch (NumberFormatException ex) {
			// Do nothing
		}
		try {
			double to = Double.parseDouble(this.to.getText());
			if (to < 0) {
				to = 0.0;
			}
			int iterations = 2;
			if (this.iterations.getText() != null && !this.iterations.getText().equals("")) {
				iterations = Integer.parseInt(this.iterations.getText());
			}
			if (iterations < 2) {
				iterations = 2;
			}
			int classNum = classNames.get(className.getSelectedItem()).intValue();
			int stationNum = -1;
			// If from and to are expressed as percentage, divides them by 100
			if (classNum < 0) {
				from /= 100;
				to /= 100;
			}
			// Correct values for population mix what-if analysis
			if (type.equals(ExactConstants.WHAT_IF_MIX)) {
				if (from > 1) {
					from = 0;
				}
				if (to > 1) {
					to = 1;
				}
			}

			if (stationName.getSelectedItem() != null) {
				stationNum = stationNames.get(stationName.getSelectedItem()).intValue();
			}
			values = data.generateWhatIfValues(type, from, to, iterations, classNum, stationNum);
		} catch (NumberFormatException e) {
			// Number format is wrong, skips this update.
		} catch (ArithmeticException ex) {
			JOptionPane.showMessageDialog(this, "Closed class with 0 customers found. They will be set to 1.", "Error", JOptionPane.ERROR_MESSAGE);
			double[] classData = data.getClassData().clone();
			for (int i = 0; i < classData.length; i++) {
				if (data.getClassTypes()[i] == ExactConstants.CLASS_CLOSED && classData[i] < 1) {
					classData[i] = 1.0;
				}
			}
			data.setClassData(classData);
			updateFields();
		}
		setFromToIterations();
	}

	/**
	 * Listener used to modify from, to and iterations fields
	 * when JTextField loses focus or ENTER key is pressed.
	 */
	protected class inputListener implements KeyListener, FocusListener {
		public void focusLost(FocusEvent e) {
			updateFields();
		}

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ENTER) {
				updateFields();
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

	/**
	 * called by the Wizard when the user presses the help button
	 */
	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		retrieveData();
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		commitData();
	}

	/**
	 * This method force a stream of data from application to GUI panel. This grants application
	 * user to be working on perfectly updated data as far as <code>retrieveData()</code> was called.
	 */
	public void retrieveData() {
		sync();
	}

	/**
	 * This method force a stream of data from GUI panel to application. This must grant other GUI
	 * panels to be working on the most recently updated version of the data this GUI panel was working
	 * on, so that when other implementing classes call retrieveData() they will have coherent data with
	 * this class.
	 */
	public void commitData() {
		commit();
	}

	/**
	 * This table is used to display informations on customer class (like population,
	 * arrival rates and betas)
	 */
	protected class ClassTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * Creates a new ClassTable with a ClassTableModel
		 */
		public ClassTable() {
			super(new ClassTableModel());
		}

		/**
		 * This method must be called each time analysis type or selected class is changed
		 */
		public void update() {
			tableChanged(new TableModelEvent(classTable.getModel(), TableModelEvent.ALL_COLUMNS));
			this.getColumnModel().setColumnSelectionAllowed(false);
			this.getColumnModel().getColumn(0).setMaxWidth(40);
		}

		/**
		 * Prepares the renderer by querying the data model for the
		 * value and selection state
		 * of the cell at <code>row</code>, <code>column</code>.
		 * Returns the component (may be a <code>Component</code>
		 * or a <code>JComponent</code>) under the event location.
		 * <p/>
		 *
		 * This paints in red elements of selected classes
		 *
		 * @param renderer the <code>TableCellRenderer</code> to prepare
		 * @param row      the row of the cell to render, where 0 is the first row
		 * @param column   the column of the cell to render,
		 *                 where 0 is the first column
		 * @return the <code>Component</code> under the event location
		 */
		@Override
		public Component prepareRenderer(TableCellRenderer renderer, int row, int column) {
			Component comp = super.prepareRenderer(renderer, row, column);
			if (comp instanceof JLabel) {
				((JLabel) comp).setHorizontalAlignment(SwingConstants.CENTER);
			}
			String name = (String) className.getSelectedItem();
			// Sets color of selected elements
			if ((name.equals(ALL_CLASSES) || getColumnName(column).equals(name)) && column != 0) {
				comp.setForeground(Color.red);
			} else {
				comp.setForeground(getForeground());
			}
			// Changes background and font of headers
			if (column == 0 || row == 0) {
				comp.setBackground(this.getTableHeader().getBackground());
				comp.setFont(comp.getFont().deriveFont(Font.BOLD));
			} else {
				comp.setBackground(Color.white);
				comp.setFont(comp.getFont().deriveFont(Font.PLAIN));
			}
			return comp;
		}

	}

	/**
	 * Table model for ClasTable
	 */
	protected class ClassTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * Returns the number of columns in the model. A
		 * <code>JTable</code> uses this method to determine how many columns it
		 * should create and display by default.
		 *
		 * @return the number of columns in the model
		 * @see #getRowCount
		 */
		public int getColumnCount() {
			String parameter = (String) type.getSelectedItem();
			if (parameter.equals(NO_ANALYSIS)) {
				return 1;
			} else if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
				return closedClasses.size() + 1;
			} else if (parameter.equals(WHAT_IF_ARRIVAL)) {
				return openClasses.size() + 1;
			} else {
				return data.getClasses() + 1;
			}
		}

		/**
		 * Returns the number of rows in the model. A
		 * <code>JTable</code> uses this method to determine how many rows it
		 * should display.  This method should be quick, as it
		 * is called frequently during rendering.
		 *
		 * @return the number of rows in the model
		 * @see #getColumnCount
		 */
		public int getRowCount() {
			String parameter = (String) type.getSelectedItem();
			if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
				return 3;
			} else {
				return 2;
			}
		}

		/**
		 * @return the object at (rowIndex, columnIndex)
		 */
		public Object getValueAt(int rowIndex, int columnIndex) {
			String parameter = (String) type.getSelectedItem();
			int index;
			// First column is header
			if (columnIndex == 0) {
				switch (rowIndex) {
					case 0:
						return "";
					case 1:
						if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
							return "N.job";
						} else if (parameter.equals(WHAT_IF_ARRIVAL)) {
							return "\u03bbi";
						} else {
							return "Di";
						}
					case 2:
						return "\u03b2i";

				}
			}

			switch (rowIndex) {
				case 0: // first row is header
					return getColumnName(columnIndex);
				case 1: // customers or arrival rates
					if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
						index = closedClassNames.get(closedClasses.get(columnIndex - 1)).intValue();
					} else if (parameter.equals(WHAT_IF_ARRIVAL)) {
						index = openClassNames.get(openClasses.get(columnIndex - 1)).intValue();
					} else {
						index = columnIndex - 1;
					}
					// Retrives class data
					double num;
					if (parameter.equals(WHAT_IF_DEMANDS)) {
						int station = stationNames.get(stationName.getSelectedItem()).intValue();
						num = data.getServiceTimes()[station][index][0] * data.getVisits()[station][index];
					} else {
						num = data.getClassData()[index];
					}

					if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
						return intFormatter.format(num);
					} else {
						return doubleFormatter.format(num);
					}
				case 2: // betas
					// This is available only for closed classes
					index = closedClassNames.get(closedClasses.get(columnIndex - 1)).intValue();
					return doubleFormatter.format(data.getClassData()[index] / data.getMaxpop());
			}
			return null;
		}

		@Override
		public String getColumnName(int index) {
			// First column is row header
			String parameter = (String) type.getSelectedItem();
			if (parameter.equals(NO_ANALYSIS) || index == 0) {
				return "";
			}
			if (parameter.equals(WHAT_IF_CUSTOMERS) || parameter.equals(WHAT_IF_MIX)) {
				return closedClasses.get(index - 1);
			} else if (parameter.equals(WHAT_IF_ARRIVAL)) {
				return openClasses.get(index - 1);
			} else {
				return data.getClassNames()[index - 1];
			}
		}
	}
}
