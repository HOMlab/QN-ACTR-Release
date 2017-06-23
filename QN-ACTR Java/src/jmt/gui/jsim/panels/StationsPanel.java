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

package jmt.gui.jsim.panels;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.text.ParseException;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.exact.table.DisabledCellRenderer;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 13-mag-2005
 * Time: 14.43.32
 * This panel provides functionality of editing and visualizing data about model's
 * classes.
 * Modified by Bertoli Marco 11-oct-2005, 10-apr-2006
 */
public class StationsPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//Table containg class-set data
	private StationTable stationTable;

	//Button that allows to add classes one by one
	private JButton addStation;

	//ComboBox editor for station type
	protected ImagedComboBoxCellEditorFactory comboEditor;

	//Enabled types of station for station editing
	Object[] stationTypes = new Object[] { STATION_TYPE_DELAY, STATION_TYPE_SERVER, STATION_TYPE_FORK, STATION_TYPE_JOIN, STATION_TYPE_ROUTER,
			STATION_TYPE_LOGGER };

	//Component responsible of setting global number of classes at once
	private JSpinner stationNumSpinner = new JSpinner() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					//stop editing text inside spinner
					try {
						stationNumSpinner.commitEdit();
					} catch (ParseException pe) {
						//if string does not represent a number, return
						return;
					}
					//new number of stations
					int x = -1;
					try {
						x = ((Integer) stationNumSpinner.getValue()).intValue();
					} catch (NumberFormatException nfe) {
						// Nothing
					} catch (ClassCastException cce) {
						// Nothing
					}
					//if new number is valid, proceed updating number
					if (x != -1) {
						setNumberOfStations(x);
					} else {
						//otherwise, reset to 0
						stationNumSpinner.setValue(new Integer(0));
					}
				}
			});
		}
	};

	//Interface linking to underlying implementation layer
	private StationDefinition data;

	//Interface linking to underlying implementation layer
	private ClassDefinition classData;

	//Index for temporary station name assignment
	private int stationNameIndex = 0;

	//deletion of one class
	private AbstractAction deleteStation = new AbstractAction("") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Delete");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent e) {
			int index = stationTable.getSelectedRow();
			if (index >= 0 && index < stationTable.getRowCount()) {
				deleteStation(index);
			}
		}
	};

	//addition of a class one by one
	private AbstractAction addNewStation = new AbstractAction("Add Station") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, ActionEvent.ALT_MASK));
			putValue(Action.SHORT_DESCRIPTION, "Adds a new station");
		}

		public void actionPerformed(ActionEvent e) {
			addStation();
		}
	};

	/**Creates a new instance of <code>ClassesPanel</code> given a model definition.*/
	public StationsPanel(StationDefinition sd, ClassDefinition cd) {
		super();
		stationTable = new StationTable();
		comboEditor = new ImagedComboBoxCellEditorFactory(true);
		initComponents();
		//forbid column to be moved
		stationTable.getTableHeader().setReorderingAllowed(false);
		setData(sd, cd);
	}

	/**Sets data model for this panel.
	 * Instantly all of the panel components are assigned their specific value.
	 * @param sd: data for station definition.*/
	public void setData(StationDefinition sd, ClassDefinition cd) {
		data = sd;
		classData = cd;
		stationTable.setModel(new StationTableModel());
		stationNumSpinner.setValue(new Integer(data.getStationKeys().size()));
		comboEditor.clearCache();
	}

	/**Gets data model for this panel.
	 * @return : data for class definition.*/
	public StationDefinition getData() {
		return data;
	}

	//Builds internal structure of the panel. Sets up layout of components
	private void initComponents() {
		//create margins for this panel.
		Box vBox = Box.createVerticalBox();
		Box hBox = Box.createHorizontalBox();
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(hBox);
		vBox.add(Box.createVerticalStrut(20));
		hBox.add(Box.createHorizontalStrut(20));

		//build central panel
		JPanel componentsPanel = new JPanel(new BorderLayout());
		//new BoxLayout(componentsPanel, BoxLayout.Y_AXIS);

		//build upper part of central panel
		JPanel upperPanel = new JPanel(new BorderLayout());
		JLabel descrLabel = new JLabel(STATIONS_DESCRIPTION);
		//descrLabel.setMaximumSize(new Dimension(300, 1000));
		upperPanel.add(descrLabel, BorderLayout.CENTER);

		//build upper right corner of the main panel
		JPanel upRightPanel = new JPanel(new BorderLayout());
		addStation = new JButton(addNewStation);
		addStation.setMinimumSize(DIM_BUTTON_S);
		upRightPanel.add(addStation, BorderLayout.CENTER);

		//build spinner panel
		JPanel spinnerPanel = new JPanel();
		JLabel spinnerDescrLabel = new JLabel("Stations:");
		//stationNumSpinner = new JSpinner();
		stationNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		spinnerPanel.add(spinnerDescrLabel);
		spinnerPanel.add(stationNumSpinner);

		//add all panels to the mail panel
		upRightPanel.add(spinnerPanel, BorderLayout.SOUTH);
		upperPanel.add(upRightPanel, BorderLayout.EAST);
		componentsPanel.add(upperPanel, BorderLayout.NORTH);
		componentsPanel.add(new JScrollPane(stationTable), BorderLayout.CENTER);
		hBox.add(componentsPanel);
		hBox.add(Box.createHorizontalStrut(20));
		this.setLayout(new GridLayout(1, 1));
		this.add(vBox);
	}

	//returns name to be displayed on the tab, when inserted in a wizard tabbed pane
	@Override
	public String getName() {
		return "Stations";
	}

	//adds a new class to the table and, simultaneously to the underlying model data structure
	private void addStation() {
		data.addStation(Defaults.get("stationName") + (++stationNameIndex), Defaults.get("stationType"));
		refreshComponents();
	}

	//synchronizes components to display coherently global number of classes
	private void refreshComponents() {
		stationTable.tableChanged(new TableModelEvent(stationTable.getModel()));
		try {
			stationNumSpinner.setValue(new Integer(data.getStationKeys().size()));
		} catch (NumberFormatException nfe) {
			// Nothing to be done
		}
		if (data.getStationKeys().size() >= MAX_NUMBER_OF_STATIONS) {
			addStation.setEnabled(false);
		} else {
			addStation.setEnabled(true);
		}
	}

	@Override
	public void repaint() {
		if (data != null) {
			refreshComponents();
		}
		super.repaint();
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		comboEditor.clearCache();
	}

	/**
	 * Called when an other panel is selected. This method will set a default station as reference
	 * station of any closed class without it.
	 */
	@Override
	public void lostFocus() {
		Vector classes = classData.getClassKeys();
		// Perform check only if valids stations are created to be reference station
		if (data.getStationKeysNoSourceSink().size() > 0) {
			for (int i = 0; i < classes.size(); i++) {
				Object classKey = classes.get(i);
				// If this is a closed class and it has no reference source or a deleted one
				if (classData.getClassType(classKey) == CLASS_TYPE_CLOSED
						&& (classData.getClassRefStation(classKey) == null || !data.getStationKeys().contains(classData.getClassRefStation(classKey)))) {
					// Sets the first station as reference station
					classData.setClassRefStation(classKey, data.getStationKeysNoSourceSink().get(0));
				}
			}
		}

		// Aborts editing of table
		TableCellEditor editor = stationTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
	}

	/*delete a class from model given the index the class to be deleted is displayed at
	inside the table.*/
	private void deleteStation(int index) {
		data.deleteStation(data.getStationKeys().get(index));
		refreshComponents();
	}

	/*Modify global number of classes for this model all at once.*/
	private void setNumberOfStations(int newNumber) {
		/*If new number is greater than a certain number, don't do anything and cancel
		number modification inside spinner*/
		if (newNumber > MAX_NUMBER_OF_STATIONS) {
			setNumberOfStations(MAX_NUMBER_OF_STATIONS);
			return;
		}
		/*If new number is not valid, reset to 0*/
		if (newNumber < 0) {
			setNumberOfStations(0);
			return;
		}
		int oldNumber = data.getStationKeys().size();
		/*If new number is greater than former one, just add */
		if (newNumber > oldNumber) {
			for (int i = oldNumber; i < newNumber; i++) {
				addStation();
			}
		} else if (newNumber < oldNumber) {
			/*otherwise, just delete*/
			for (int i = oldNumber - 1; i >= newNumber; i--) {
				deleteStation(i);
			}
		}
		refreshComponents();
	}

	//---------------------------- Table containing classes parameters --------------------------
	/*Table that must display all of data about user classes. Customization of table settings is
	obtained via inheritation of <code>JTable</code> Class.*/

	private class StationTable extends JTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/*This button allow a single userclass to be deleted directly from the table.
		Corresponding value contained into cell must be zero.*/
		JButton deleteButton = new JButton() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			{
				setAction(deleteStation);
				setFocusable(false);
			}
		};

		/*Set of column dimensions*/
		int[] columnSizes = new int[] { 120, 120, 18 };

		//Sets a table model for visualization and editing of data
		public void setModel(StationTableModel tabMod) {
			super.setModel(tabMod);
			sizeColumnsAndRows();
			setRowHeight(ROW_HEIGHT);
			setDefaultRenderer(String.class, new jmt.gui.exact.table.DisabledCellRenderer());
		}

		//returns a component to be contained inside a table column(or cell)
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 1) {
				return comboEditor.getRenderer();
			} else if (column == getColumnCount() - 1) {
				if (isCellEditable(row, column)) {
					return new ButtonCellEditor(deleteButton);
				} else {
					return new DisabledCellRenderer();
				}
			} else {
				return getDefaultRenderer(getModel().getColumnClass(column));
			}
		}

		/*returns customized editor for table cells.*/
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return comboEditor.getEditor(stationTypes);
			} else if (column == getColumnCount() - 1) {
				return new ButtonCellEditor(deleteButton);
			} else {
				return super.getCellEditor(row, column);
			}
		}

		//set sizes for columns and rows of this table.
		private void sizeColumnsAndRows() {
			for (int i = 0; i < columnSizes.length && i < getColumnCount(); i++) {
				this.getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
				if (i == columnSizes.length - 1) {
					//delete button and containing table cells as well, must be square
					this.getColumnModel().getColumn(i).setMaxWidth(columnSizes[i]);
					this.setRowHeight(columnSizes[i]);
				}
			}
		}

		@Override
		public boolean isCellEditable(int row, int column) {
			// Avoid deletion of sources and sinks
			String stationType = data.getStationType(data.getStationKeys().get(row));
			return !((column == getColumnCount() - 1 || column == 1) && (stationType.equals(STATION_TYPE_SOURCE) || stationType
					.equals(STATION_TYPE_SINK)));
		}
	}

	//------------------------------------Table model for classes panel --------------------------
	/*Table data model to implement customized data editing*/

	private class StationTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		//Names of columns contained in table. Columns containing buttons have empty names
		String[] columnNames = new String[] { "Name", "Type", "" };

		//Class declarations for this table's columns.
		Class[] colClasses = new Class[] { String.class, JComboBox.class, JButton.class };

		/**Creates a new instance of class table model*/
		public StationTableModel() {
			super();
		}

		/**returns number of rows to be displayed in the table. In this case, global
		 * number of classes*/
		public int getRowCount() {
			if (data.getStationKeys() != null) {
				return data.getStationKeys().size();
			} else {
				return 0;
			}
		}

		/**Returns total number of columns*/
		public int getColumnCount() {
			return columnNames.length;
		}

		/**Returns name for each column (given its index) to be displayed
		 * inside table header*/
		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex < columnNames.length) {
				return columnNames[columnIndex];
			} else {
				return null;
			}
		}

		/**Returns class describing data contained in specific column.*/
		@Override
		public Class getColumnClass(int columnIndex) {
			if (columnIndex < colClasses.length) {
				return colClasses[columnIndex];
			} else {
				return Object.class;
			}
		}

		/**Tells wether data contained in a specific cell(given row and column index)
		 * is editable or not. In this case distribution column is not editable, as
		 * editing functionality is implemented via edit button*/
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			//can edit station type only if type is contained in combo box list
			if (columnIndex == 1) {
				String type = data.getStationType(data.getStationKeys().get(rowIndex));
				for (Object stationType : stationTypes) {
					if (stationType.equals(type)) {
						return true;
					}
				}
				return false;
			}
			return true;
		}

		/**retrieves value to be displayed in table cell from the underlying model
		 * data structure implementation.*/
		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = data.getStationKeys().get(rowIndex);
			switch (columnIndex) {
				case (0): {
					return data.getStationName(key);
				}
				case (1): {
					return data.getStationType(key);
				}
				default: {
					return null;
				}
			}
		}

		/**Puts edited values to the underlying data structure for model implementation*/
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object key = data.getStationKeys().get(rowIndex);
			switch (columnIndex) {
				case (0): {
					data.setStationName((String) aValue, key);
					break;
				}
				case (1): {
					data.setStationType((String) aValue, key);
					break;
				}
			}
		}

		@Override
		public void addTableModelListener(TableModelListener l) {
		}

		@Override
		public void removeTableModelListener(TableModelListener l) {
		}

	}

}
