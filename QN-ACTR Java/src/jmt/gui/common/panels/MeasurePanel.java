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

package jmt.gui.common.panels;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.exact.table.DisabledCellRenderer;
import jmt.gui.exact.table.ExactCellEditor;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 26-lug-2005
 * Time: 16.08.15
 * Modified by Bertoli Marco 29/09/2005, 7-oct-2005
 *                           9-jan-2006  --> ComboBoxCellEditor
 *                           
 * Modified by Ashanka (May 2010):
 * Description: Resized some column's width and edited the column headings.
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 * 
 */
public class MeasurePanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//Interfaces for model data exchange
	protected ClassDefinition classData;

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = measureTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
	}

	protected StationDefinition stationData;
	protected SimulationDefinition simData;

	protected WarningScrollTable warningPanel;

	//label containing description of this panel's purpose
	private JLabel descrLabel = new JLabel(MEASURES_DESCRIPTION);

	//table containing measure data
	protected MeasureTable measureTable;

	//button for measure addition
	private JButton addMeasureButton;

	//types of measures selectable
	protected static final String[] measureTypes = new String[] { SimulationDefinition.MEASURE_QL, SimulationDefinition.MEASURE_QT,
			SimulationDefinition.MEASURE_RD, SimulationDefinition.MEASURE_RP, SimulationDefinition.MEASURE_U, SimulationDefinition.MEASURE_X,
			SimulationDefinition.MEASURE_DR, SimulationDefinition.MEASURE_S_X, SimulationDefinition.MEASURE_S_RP, SimulationDefinition.MEASURE_S_DR,
			SimulationDefinition.MEASURE_S_CN,
			//Added by ASHANKA START
			//Adds system power index to the performance index combo box in the JSIM panel
			SimulationDefinition.MEASURE_S_SP,
			SimulationDefinition.MEASURE_X_PER_SINK,
			SimulationDefinition.MEASURE_R_PER_SINK
	//Added by ASHANKA STOP
	};

	// Measure selection ComboBox
	protected JComboBox measureSelection = new JComboBox(measureTypes);

	/** Editors and renderers for table */
	protected ImagedComboBoxCellEditorFactory stationsCombos, classesCombos;

	//deletes a measure from list
	protected AbstractAction deleteMeasure = new AbstractAction("") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Deletes this measure");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent e) {
			int index = measureTable.getSelectedRow();
			if (index >= 0 && index < measureTable.getRowCount()) {
				deleteMeasure(index);
			}
		}
	};

	//addition of a class one by one
	protected AbstractAction addMeasure = new AbstractAction("Add selected index") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, ActionEvent.ALT_MASK));
			putValue(Action.SHORT_DESCRIPTION, "Adds a new measure with selected performance index");
		}

		public void actionPerformed(ActionEvent e) {
			addMeasure();
		}
	};

	public MeasurePanel(ClassDefinition classes, StationDefinition stations, SimulationDefinition simParams) {
		stationsCombos = new ImagedComboBoxCellEditorFactory(stations);
		classesCombos = new ImagedComboBoxCellEditorFactory(classes);
		classesCombos.setAllowsNull(true);
		setData(classes, stations, simParams);
		initComponents();
	}

	private void initComponents() {
		this.setBorder(new EmptyBorder(20, 20, 20, 20));
		this.setLayout(new BorderLayout(5, 5));

		addMeasureButton = new JButton(addMeasure);
		JPanel rightPanel = new JPanel(new BorderLayout(5, 5));
		rightPanel.add(addMeasureButton, BorderLayout.SOUTH);
		rightPanel.add(measureSelection, BorderLayout.CENTER);

		measureTable = new MeasureTable();

		JPanel headPanel = new JPanel(new BorderLayout(5, 5));
		headPanel.add(descrLabel, BorderLayout.CENTER);
		headPanel.add(rightPanel, BorderLayout.EAST);
		addMeasureButton.setMaximumSize(DIM_BUTTON_M);
		this.add(headPanel, BorderLayout.NORTH);
		warningPanel = new WarningScrollTable(measureTable, WARNING_CLASS_STATION);
		warningPanel.addCheckVector(classData.getClassKeys());
		warningPanel.addCheckVector(stationData.getStationRegionKeysNoSourceSink());
		this.add(warningPanel, BorderLayout.CENTER);
	}

	/**Updates data contained in this panel's components*/
	public void setData(ClassDefinition classes, StationDefinition stations, SimulationDefinition simParams) {
		classData = classes;
		stationData = stations;
		simData = simParams;
		stationsCombos.setData(stations);
		classesCombos.setData(classes);
		refreshComponents();
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		stationsCombos.clearCache();
		classesCombos.clearCache();
		refreshComponents();
	}

	@Override
	public void repaint() {
		refreshComponents();
		super.repaint();
	}

	private void refreshComponents() {
		if (measureTable != null) {
			measureTable.tableChanged(new TableModelEvent(measureTable.getModel()));
		}
		if (warningPanel != null) {
			warningPanel.clearCheckVectors();
			warningPanel.addCheckVector(classData.getClassKeys());
			warningPanel.addCheckVector(stationData.getStationRegionKeysNoSourceSink());
		}
	}

	private void addMeasure() {
		if (stationData.getStationRegionKeysNoSourceSink().size() == 0 || classData.getClassKeys().size() == 0) {
			return;
		}

		simData.addMeasure((String) measureSelection.getSelectedItem(), null, null);
		measureTable.tableChanged(new TableModelEvent(measureTable.getModel()));
	}

	private void deleteMeasure(int index) {
		simData.removeMeasure(simData.getMeasureKeys().get(index));
		measureTable.tableChanged(new TableModelEvent(measureTable.getModel()));
	}

	@Override
	public String getName() {
		return "Performance Indices";
	}

	protected class MeasureTable extends JTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private JButton deleteButton = new JButton(deleteMeasure);

		public MeasureTable() {
			setModel(new MeasureTableModel());
			setRowHeight(ROW_HEIGHT);
			sizeColumns();
			setDefaultEditor(Double.class, new ExactCellEditor());
			setDefaultRenderer(Object.class, new DisabledCellRenderer());
		}

		private void sizeColumns() {
			int[] columnWidths = ((MeasureTableModel) getModel()).columnWidths;
			for (int i = 0; i < columnWidths.length; i++) {
				int prefWidth = columnWidths[i];
				if (i == columnWidths.length - 1) {
					getColumnModel().getColumn(i).setMaxWidth(getRowHeight());
				} else {
					getColumnModel().getColumn(i).setPreferredWidth(prefWidth);
				}
			}
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 5) {
				return new ButtonCellEditor(deleteButton);
			} else if (column == 2 && simData.isSinkMeasure(simData.getMeasureKeys().get(row))) {
				return stationsCombos.getEditor(stationData.getStationKeysSink());
			} else if (column == 2) {
				return stationsCombos.getEditor(stationData.getStationRegionKeysNoSourceSink());
			} else if (column == 1) {
				return classesCombos.getEditor(classData.getClassKeys());
			} else {
				return super.getCellEditor(row, column);
			}
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 5) {
				return new ButtonCellEditor(deleteButton);
			} else if (column == 2 && !simData.isGlobalMeasure(simData.getMeasureKeys().get(row))) {
				return stationsCombos.getRenderer();
			} else if (column == 1) {
				return classesCombos.getRenderer();
			} else {
				return super.getCellRenderer(row, column);
			}
		}

	}

	protected class MeasureTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] columnNames = new String[] { "Performance Index", "Class", "Station/Region", "Conf.Int.",
				"Max Rel.Err.", "" };
		private Class[] columnClasses = new Class[] { String.class, String.class, String.class, Double.class, Double.class, Object.class };
		public int[] columnWidths = new int[] { 120, 80, 120, 60, 60, 20 };

		public int getRowCount() {
			return simData.getMeasureKeys().size();
		}

		public int getColumnCount() {
			return columnNames.length;
		}

		@Override
		public String getColumnName(int columnIndex) {
			return columnNames[columnIndex];
		}

		@Override
		public Class<Integer> getColumnClass(int columnIndex) {
			return columnClasses[columnIndex];
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			// Avoid editing of Measure type
			if (columnIndex == 0) {
				return false;
			}
			// Avoid set reference station for global measures
			if (columnIndex == 2 && simData.isGlobalMeasure(simData.getMeasureKeys().get(rowIndex))) {
				return false;
			}
			return true;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = simData.getMeasureKeys().get(rowIndex);
			switch (columnIndex) {
				case 0: {
					return simData.getMeasureType(key);
				}
				case 1: {
					return simData.getMeasureClass(key);
				}
				case 2: {
					return simData.getMeasureStation(key);
				}
				case 3: {
					return simData.getMeasureAlpha(key);
				}
				case 4: {
					return simData.getMeasurePrecision(key);
				}
			}
			return null;
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object key = simData.getMeasureKeys().get(rowIndex);
			switch (columnIndex) {
				case 0: {
					simData.setMeasureType((String) aValue, key);
					break;
				}
				case 1: {
					simData.setMeasureClass(aValue, key);
					break;
				}
				case 2: {
					simData.setMeasureStation(aValue, key);
					break;
				}
				case 3: {
					try {
						String doubleVal = (String) aValue;
						simData.setMeasureAlpha(Double.valueOf(doubleVal), key);
						break;
					} catch (NumberFormatException e) {
					}
				}
				case 4: {
					try {
						String doubleVal = (String) aValue;
						simData.setMeasurePrecision(Double.valueOf(doubleVal), key);
						break;
					} catch (NumberFormatException e) {
					}
				}
			}
		}

	}
}
