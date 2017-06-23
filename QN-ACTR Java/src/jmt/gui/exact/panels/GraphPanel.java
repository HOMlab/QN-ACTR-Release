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
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Arrays;

import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import jmt.framework.data.ArrayUtils;
import jmt.framework.gui.graph.WhatIfPlot;
import jmt.framework.gui.table.editors.ColorCellEditor;
import jmt.framework.gui.table.editors.ComboBoxCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;

/**
 * <p>Title: Graph Panel</p>
 * <p>Description: This panelis used to display JMVA what-if analysis
 * results in a graph. Number of allowed lines in graph is determined
 * by <code>graph.getColors().length</code>. Modify it to allow more lines.</p>
 *
 * @author Bertoli Marco
 *         Date: 1-giu-2006
 *         Time: 11.01.29
 */
public class GraphPanel extends WizardPanel implements ExactConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Data structure
	private ExactModel model;
	// Plot
	private WhatIfPlot graph;
	// Performance index selector
	private JComboBox index;
	// Bounds for graph
	private JSpinner Xmin, Xmax, Ymin, Ymax;
	// Tells if spinner update is forced. This is needed to avoid that updates made by
	// code will be interpreted as updated made by user.
	private boolean forcedUpdate = false;
	// Table used to select performance indices to be plotted
	private LinesTable table;
	// Scrollpane used for table
	private JScrollPane tableScrollPane;
	// Dimension of bounds spinners
	final static Dimension DIM_SPINNER = new Dimension(60, 20);
	// Current performance index
	private String currentIndex = "none";
	// Selected performance indices
	private int[] classes;
	private int[] stations;
	// Aggregate special value
	private static final String AGGREGATE = "<html><b><i>Aggregate</i></b></html>";

	//Added by ASHANKA START
	private TableColumn stationColumn;

	//Added by ASHANKA STOP

	/**
	 * Builds a new GraphPanel, given an exact model data structure
	 * @param model reference to data structure
	 */
	public GraphPanel(ExactModel model) {
		this.model = model;
		initGraphics();
	}

	/**
	 * Initialize GUI of this panel
	 */
	private void initGraphics() {
		setLayout(new BorderLayout(10, 10));
		setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		JPanel mainPanel = new JPanel(new BorderLayout(5, 5));
		mainPanel.setBorder(BorderFactory.createEtchedBorder());

		// Adds description label
		JLabel descrLabel = new JLabel(DESCRIPTION_GRAPH);
		add(descrLabel, BorderLayout.NORTH);
		add(mainPanel, BorderLayout.CENTER);

		// Creates left panel with options
		JPanel left = new JPanel(new BorderLayout(3, 3));
		// Adds performance index selection
		JPanel indexPanel = new JPanel();
		JLabel pIndex = new JLabel("Performance index: ");
		index = new JComboBox(ExactConstants.INDICES_TYPES);
		// Adds aggregate types
		for (String element : AGGREGATE_TYPES) {
			index.addItem(element);
		}
		pIndex.setLabelFor(index);
		indexPanel.add(pIndex);
		indexPanel.add(index);
		left.add(indexPanel, BorderLayout.NORTH);

		// Adds panel for bounds selection
		JPanel boundsPanel = new JPanel(new GridLayout(2, 4, 1, 1));
		boundsPanel.add(new JLabel("Xmin: ", SwingConstants.RIGHT));
		Xmin = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Xmin.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Xmin);
		boundsPanel.add(new JLabel("Xmax: ", SwingConstants.RIGHT));
		Xmax = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Xmax.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Xmax);
		boundsPanel.add(new JLabel("Ymin: ", SwingConstants.RIGHT));
		Ymin = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymin.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Ymin);
		boundsPanel.add(new JLabel("Ymax: ", SwingConstants.RIGHT));
		Ymax = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymax.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Ymax);
		left.add(boundsPanel, BorderLayout.SOUTH);

		mainPanel.add(left, BorderLayout.WEST);

		// Puts graph in the right panel
		// Creates label for X-axis
		String xLabel = "";
		if (model.getWhatIfClass() >= 0) {
			graph = new WhatIfPlot(model.getWhatIfValues());
			if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_ARRIVAL)) {
				xLabel = "Arrival rate \u03bbi for " + model.getClassNames()[model.getWhatIfClass()] + " [job/s]";
			} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_CUSTOMERS)) {
				xLabel = "Number of customers Ni for " + model.getClassNames()[model.getWhatIfClass()];
			} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_DEMANDS)) {
				xLabel = "Service demand Di for " + model.getClassNames()[model.getWhatIfClass()] + " at "
						+ model.getStationNames()[model.getWhatIfStation()] + " [s]";
			} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_MIX)) {
				xLabel = "Population mix \u03b2i for " + model.getClassNames()[model.getWhatIfClass()];
			}
		} else {
			graph = new WhatIfPlot(ArrayUtils.multiply(model.getWhatIfValues(), 100.0));
			if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_ARRIVAL)) {
				xLabel = "% of arrival rates \u03bbi w.r.t. initial values";
			} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_CUSTOMERS)) {
				xLabel = "% of customers Ni w.r.t. initial values";
			} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_DEMANDS)) {
				xLabel = "% of Service demands Di at " + model.getStationNames()[model.getWhatIfStation()] + " w.r.t. initial values";
			}
		}
		graph.setXLabel(xLabel);
		mainPanel.add(graph, BorderLayout.CENTER);

		// Adds table and inits data structure for it.
		classes = new int[graph.getColors().length];
		if (model.isMultiClass()) {
			Arrays.fill(classes, -10);
		}
		stations = new int[graph.getColors().length];
		Arrays.fill(stations, -10);
		table = new LinesTable();
		tableScrollPane = new JScrollPane(table);
		tableScrollPane.setPreferredSize(new Dimension(160, tableScrollPane.getPreferredSize().height));
		left.add(tableScrollPane, BorderLayout.CENTER);

		updateSpinners();
		addActions();
		updateIndex();
	}

	/**
	 * Updates values in spinners used to select ranges to be shown in graph
	 */
	private void updateSpinners() {
		// Check for special value used if graph is empty
		if (graph.getXRange()[0] != Double.MAX_VALUE) {
			Xmin.setValue(new Double(graph.getXRange()[0]));
			Xmax.setValue(new Double(graph.getXRange()[1]));
			Ymin.setValue(new Double(graph.getYRange()[0]));
			Ymax.setValue(new Double(graph.getYRange()[1]));
		} else {
			Xmin.setValue(new Double(0.0));
			Xmax.setValue(new Double(0.0));
			Ymin.setValue(new Double(0.0));
			Ymax.setValue(new Double(0.0));
		}
	}

	/**
	 * Used when a spinne value is updated
	 */
	private void setBounds() {
		double xmin, xmax, ymin, ymax;
		Object val = Xmin.getValue();
		if (val instanceof Number) {
			xmin = ((Number) val).doubleValue();
		} else {
			xmin = graph.getXRange()[0];
		}
		val = Xmax.getValue();
		if (val instanceof Number) {
			xmax = ((Number) val).doubleValue();
		} else {
			xmax = graph.getXRange()[1];
		}
		val = Ymin.getValue();
		if (val instanceof Number) {
			ymin = ((Number) val).doubleValue();
		} else {
			ymin = graph.getYRange()[0];
		}
		val = Ymax.getValue();
		if (val instanceof Number) {
			ymax = ((Number) val).doubleValue();
		} else {
			ymax = graph.getYRange()[1];
		}
		// Sets bounds
		graph.setXRange(xmin, xmax);
		graph.setYRange(ymin, ymax);
		graph.repaint();
	}

	/**
	 * This function must be called each time selected performance
	 * index changes
	 */
	private void updateIndex() {
		String current = (String) index.getSelectedItem();
		if (!current.equals(currentIndex)) {
			currentIndex = current;
			//Added by ASHANKA START
			if (currentIndex.equals(ExactConstants.INDICES_TYPES[4])) {
				if (stationColumn == null) {
					stationColumn = table.getColumnModel().getColumn(2);
				}
				//If the System Power is selected then Need to remove the Stations Column if present
				//If column count is less than 3 then do nothing as Stations Column is already removed.
				if (table.getColumnCount() == 3) {
					table.removeColumn(stationColumn);
				}
			} else {
				//If any thing other than System Power is clicked then 
				//restore the Stations Column only if it is not present.
				if (table.getColumnCount() < 3) {
					table.addColumn(stationColumn);
				}
			}
			//Added by ASHANKA STOP
			// System Response time
			if (currentIndex.equals(AGGREGATE_TYPES[0])) {
				tableScrollPane.setVisible(false);
				graph.clear(false);
				graph.draw(0, model.getGlobalR());
				graph.fillPlot();
			}
			// System throughput
			else if (currentIndex.equals(AGGREGATE_TYPES[1])) {
				tableScrollPane.setVisible(false);
				graph.clear(false);
				graph.draw(0, model.getGlobalX());
				graph.fillPlot();
			}
			// Number of customers
			else if (currentIndex.equals(AGGREGATE_TYPES[2])) {
				tableScrollPane.setVisible(false);
				graph.clear(false);
				graph.draw(0, model.getGlobalQ());
				graph.fillPlot();
			}
			//Added by ASHANKA START
			//For single Class TableScroll Pane is removed. 
			else if (currentIndex.equals(ExactConstants.INDICES_TYPES[4]) && !model.isMultiClass()) {
				tableScrollPane.setVisible(false);
				graph.clear(false);
				graph.draw(0, model.getGlobalSP());
				graph.fillPlot();
			}
			//Added by ASHANKA STOP
			// otherwise
			else {
				tableScrollPane.setVisible(true);
				// Removes incorrect utilization measures
				if (table.getCellEditor() != null) {
					table.getCellEditor().stopCellEditing();
				}
				if (currentIndex.equals(ExactConstants.INDICES_TYPES[3])) {
					for (int i = 0; i < stations.length; i++) {
						if (stations[i] == -1) {
							stations[i] = -2;
						}
					}
				}
				table.repaint();
				paintAllIndices();
			}
			// Updates graph
			graph.setYLabel(current);
			graph.repaint();
		}
	}

	/**
	 * Adds action listeners to GUI components
	 */
	private void addActions() {
		// Listener used for bounds spinners
		ChangeListener boundsListener = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (!forcedUpdate) {
					setBounds();
					updateSpinners();
				}
			}
		};
		Xmin.addChangeListener(boundsListener);
		Xmax.addChangeListener(boundsListener);
		Ymin.addChangeListener(boundsListener);
		Ymax.addChangeListener(boundsListener);
		// Listener for index selection comboBox
		index.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				updateIndex();
			}
		});

		// Adds a listener to the graph to detect zoom events
		graph.addRescaleListener(new WhatIfPlot.RescaleListener() {
			public void Rescaled() {
				forcedUpdate = true;
				updateSpinners();
				forcedUpdate = false;
			}
		});
	}

	/**
	 * Paints performance index at specified row
	 * @param rowNum row number of index to be painted
	 */
	private void paintIndexAtRow(int rowNum) {
		// Clears previous graph
		graph.clear(rowNum);
		int classNum = classes[rowNum];
		int statNum = stations[rowNum];

		//Modified the below condition by ASHANKA for 
		//System Power there is no Station Panel
		//in fact the station panel is removed 
		//System Power is Indices type 4
		//if (classNum < -1 || statNum < -1) {
		if (classNum < -1 || (statNum < -1 && !currentIndex.equals(ExactConstants.INDICES_TYPES[4]))) {
			// Resets view
			autosizeGraph();
			return;
		}

		// Throughput
		if (currentIndex.equals(ExactConstants.INDICES_TYPES[0])) {
			if (classNum >= 0 && statNum >= 0) {
				graph.draw(rowNum, model.getThroughput()[statNum][classNum]);
			} else if (classNum < 0 && statNum >= 0) {
				graph.draw(rowNum, model.getPerStationX()[statNum]);
			} else if (classNum >= 0 && statNum < 0) {
				graph.draw(rowNum, model.getPerClassX()[classNum]);
			} else {
				graph.draw(rowNum, model.getGlobalX());
			}
		}
		// Queue length
		if (currentIndex.equals(ExactConstants.INDICES_TYPES[1])) {
			if (classNum >= 0 && statNum >= 0) {
				graph.draw(rowNum, model.getQueueLen()[statNum][classNum]);
			} else if (classNum < 0 && statNum >= 0) {
				graph.draw(rowNum, model.getPerStationQ()[statNum]);
			} else if (classNum >= 0 && statNum < 0) {
				graph.draw(rowNum, model.getPerClassQ()[classNum]);
			} else {
				graph.draw(rowNum, model.getGlobalQ());
			}
		}
		// Residence times
		if (currentIndex.equals(ExactConstants.INDICES_TYPES[2])) {
			if (classNum >= 0 && statNum >= 0) {
				graph.draw(rowNum, model.getResTimes()[statNum][classNum]);
			} else if (classNum < 0 && statNum >= 0) {
				graph.draw(rowNum, model.getPerStationR()[statNum]);
			} else if (classNum >= 0 && statNum < 0) {
				graph.draw(rowNum, model.getPerClassR()[classNum]);
			} else {
				graph.draw(rowNum, model.getGlobalR());
			}
		}
		// Utilization
		if (currentIndex.equals(ExactConstants.INDICES_TYPES[3])) {
			if (classNum >= 0 && statNum >= 0) {
				graph.draw(rowNum, model.getUtilization()[statNum][classNum]);
			} else {
				graph.draw(rowNum, model.getPerStationU()[statNum]);
			}
		}
		//Added by ASHANKA START
		//System Power
		else if (currentIndex.equals(ExactConstants.INDICES_TYPES[4])) {
			if (classNum >= 0) {
				graph.draw(rowNum, model.getPerClassSP()[classNum]);
			} else if (classNum == -1) {
				graph.draw(rowNum, model.getGlobalSP());
			}
		}
		//Added by ASHANKA STOP
		// Resets view
		autosizeGraph();
	}

	/**
	 * Paints all performance indices of current table
	 */
	private void paintAllIndices() {
		for (int i = 0; i < classes.length; i++) {
			paintIndexAtRow(i);
		}
	}

	/**
	 * AutoResizes graph window
	 */
	private void autosizeGraph() {
		graph.fillPlot();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Graphical Results";
	}

	/**
	 * Table used to select performance indices to be drawn
	 */
	protected class LinesTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/** ComboBoxes used as cell editors */
		private ComboEditor classEditor, stationsEditor, uStationsEditor;

		/**
		 * Builds a new LinesTable
		 */
		public LinesTable() {
			super(new LinesTableModel());
			setDefaultRenderer(Color.class, new ColorCellEditor());
			setDefaultRenderer(String.class, ComboBoxCellEditor.getRendererInstance());
			// Sets column sizes
			getColumnModel().getColumn(0).setMaxWidth(30);
			getColumnModel().getColumn(1).setPreferredWidth(80);
			getColumnModel().getColumn(2).setPreferredWidth(80);
			setRowHeight(18);

			// Creates class editors (one is for utilizations)
			JComboBox classCombo = new JComboBox();
			// Null elements
			classCombo.addItem("");
			// Aggregate measures
			classCombo.addItem(AGGREGATE);
			for (int i = 0; i < model.getClasses(); i++) {
				classCombo.addItem(model.getClassNames()[i]);
			}

			// Creates station editor
			JComboBox stationsCombo = new JComboBox();
			JComboBox uStationsCombo = new JComboBox();
			stationsCombo.addItem("");
			uStationsCombo.addItem("");
			stationsCombo.addItem(AGGREGATE);
			uStationsCombo.addItem(ExactConstants.GRAY_S + AGGREGATE + ExactConstants.GRAY_E);
			for (int i = 0; i < model.getStations(); i++) {
				stationsCombo.addItem(model.getStationNames()[i]);
				uStationsCombo.addItem(model.getStationNames()[i]);
			}

			// Creates editors
			classEditor = new ComboEditor(classCombo);
			uStationsEditor = new ComboEditor(uStationsCombo);
			stationsEditor = new ComboEditor(stationsCombo);
		}

		/**
		 * Returns an appropriate editor for the cell specified by
		 * <code>row</code> and <code>column</code>. If the
		 * <code>TableColumn</code> for this column has a non-null editor,
		 * returns that.  If not, finds the class of the data in this
		 * column (using <code>getColumnClass</code>)
		 * and returns the default editor for this type of data.
		 * <p/>
		 *
		 * @param row    the row of the cell to edit, where 0 is the first row
		 * @param column the column of the cell to edit,
		 *               where 0 is the first column
		 * @return the editor for this cell;
		 *         if <code>null</code> return the default editor for
		 *         this type of cell
		 * @see javax.swing.DefaultCellEditor
		 */
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return classEditor;
			} else if (currentIndex.equals(ExactConstants.INDICES_TYPES[3])) {
				return uStationsEditor;
			} else {
				return stationsEditor;
			}
		}

		/**
		 * Returns an appropriate renderer for the cell specified by this row and
		 * column. If the <code>TableColumn</code> for this column has a non-null
		 * renderer, returns that.  If not, finds the class of the data in
		 * this column (using <code>getColumnClass</code>)
		 * and returns the default renderer for this type of data.
		 * <p/>
		 * <b>Note:</b>
		 * Throughout the table package, the internal implementations always
		 * use this method to provide renderers so that this default behavior
		 * can be safely overridden by a subclass.
		 *
		 * @param row    the row of the cell to render, where 0 is the first row
		 * @param column the column of the cell to render,
		 *               where 0 is the first column
		 * @return the assigned renderer; if <code>null</code>
		 *         returns the default renderer
		 *         for this type of object
		 * @see javax.swing.table.DefaultTableCellRenderer
		 * @see javax.swing.table.TableColumn#setCellRenderer
		 * @see #setDefaultRenderer
		 */
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (model.isMultiClass() || column != 1) {
				return super.getCellRenderer(row, column);
			} else {
				return super.getDefaultRenderer(Object.class);
			}
		}

		/**
		 * Inner class used as a comboBox editor
		 */
		protected class ComboEditor extends DefaultCellEditor {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			protected JComboBox combo;

			/**
			 * Constructs a <code>DefaultCellEditor</code> object that uses a
			 * combo box.
			 *
			 * @param comboBox a <code>JComboBox</code> object
			 */
			public ComboEditor(JComboBox comboBox) {
				super(comboBox);
				combo = comboBox;
			}

			/**
			 * Returns selected index - 2 (so -1 means all classes and -2 or -3
			 * means no selection)
			 */
			@Override
			public Object getCellEditorValue() {
				int val = combo.getSelectedIndex();
				return new Integer(val - 2);
			}
		}
	}

	/**
	 * Table model for LinesTable
	 */
	protected class LinesTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * Returns <code>Object.class</code> regardless of <code>columnIndex</code>.
		 *
		 * @param columnIndex the column being queried
		 * @return the Object.class
		 */
		@Override
		public Class getColumnClass(int columnIndex) {
			switch (columnIndex) {
				case 0:
					return Color.class;
				case 1:
					return String.class;
				case 2:
					return String.class;
				default:
					return super.getColumnClass(columnIndex);
			}
		}

		/**
		 * Returns the number of columns in the model. A
		 * <code>JTable</code> uses this method to determine how many columns it
		 * should create and display by default.
		 *
		 * @return the number of columns in the model
		 * @see #getRowCount
		 */
		public int getColumnCount() {
			return 3;
		}

		/**
		 * Returns a default name for the column using spreadsheet conventions:
		 * A, B, C, ... Z, AA, AB, etc.  If <code>column</code> cannot be found,
		 * returns an empty string.
		 *
		 * @param column the column being queried
		 * @return a string containing the default name of <code>column</code>
		 */
		@Override
		public String getColumnName(int column) {
			switch (column) {
				case 0:
					return " ";
				case 1:
					return "Class";
				case 2:
					return "Station";
			}

			return super.getColumnName(column); //To change body of overridden methods use File | Settings | File Templates.
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
			return graph.getColors().length;
		}

		/**
		 * Returns the value for the cell at <code>columnIndex</code> and
		 * <code>rowIndex</code>.
		 *
		 * @param    rowIndex    the row whose value is to be queried
		 * @param    columnIndex the column whose value is to be queried
		 * @return the value Object at the specified cell
		 */
		public Object getValueAt(int rowIndex, int columnIndex) {
			int stationNum = stations[rowIndex];
			int classNum = classes[rowIndex];
			switch (columnIndex) {
				case 0:
					return graph.getColors()[rowIndex];
				case 1:
					if (classNum >= 0) {
						return model.getClassNames()[classNum];
					} else if (classNum == -1) {
						return AGGREGATE;
					} else {
						return null;
					}
				case 2:
					if (stationNum >= 0) {
						return model.getStationNames()[stationNum];
					} else if (stationNum == -1) {
						return AGGREGATE;
					} else {
						return null;
					}
			}
			return null;
		}

		/**
		 * This empty implementation is provided so users don't have to implement
		 * this method if their data model is not editable.
		 *
		 * @param aValue      value to assign to cell
		 * @param rowIndex    row of cell
		 * @param columnIndex column of cell
		 */
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			if (columnIndex == 2) {
				if (currentIndex.equals(ExactConstants.INDICES_TYPES[3]) && ((Integer) aValue).intValue() < 0) {
					stations[rowIndex] = -2;
				} else {
					stations[rowIndex] = ((Integer) aValue).intValue();
				}
			} else {
				classes[rowIndex] = ((Integer) aValue).intValue();
			}
			// Paints new index
			paintIndexAtRow(rowIndex);
		}

		/**
		 * Class and stations are editables
		 *
		 * @param rowIndex    the row being queried
		 * @param columnIndex the column being queried
		 * @return false
		 */
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return columnIndex == 2 || (columnIndex == 1 && model.isMultiClass());
		}
	}
}
