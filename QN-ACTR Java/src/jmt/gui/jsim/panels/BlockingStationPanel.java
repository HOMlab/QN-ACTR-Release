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
import java.awt.event.ActionListener;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.BlockingRegionDefinition;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.panels.BlockingRegionParameterPanel;
import jmt.gui.common.panels.WarningScrollTable;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Blocking Region Station Definition Panel</p>
 * <p>Description: This panel is used to specify stations to be inserted into a
 * blocking region. This panel includes BlockingRegionParameterPanel to
 * specify class dependent constraints.</p>
 *
 * @author Bertoli Marco
 *         Date: 10-mag-2006
 *         Time: 16.53.41
 */
public class BlockingStationPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/** Blocking region definition data structure */
	protected BlockingRegionDefinition bd;
	/** Class definition data structure */
	protected ClassDefinition cd;
	/** Station definition data structure */
	protected StationDefinition sd;
	/** Current region key */
	protected Object regionKey;
	/** Inner panel to specify class constraints */
	protected BlockingRegionParameterPanel blockingPanel;

	/** Panel used to display stations */
	protected JPanel stationPanel;

	/** Table used to display stations */
	protected JTable stationTable;

	/** Spinner used to select number of stations */
	protected JSpinner stationNumSpinner;

	/** Button used to add a stations */
	protected JButton addStation;

	/** Vector used to store stations in a given region. This is needed as
	 * data structure uses a set which is sorted */
	protected Vector<Object> stations;

	/** Used for station selection renderers */
	protected ImagedComboBoxCellEditorFactory comboFactory;

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Blocking Region Station Definition";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		if (blockingPanel != null) {
			blockingPanel.lostFocus();
		}
		// Aborts editing of table
		TableCellEditor editor = stationTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}

	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		if (blockingPanel != null) {
			blockingPanel.gotFocus();
		}
		stations.clear();
		stations.addAll(bd.getBlockingRegionStations(regionKey));
		comboFactory.clearCache();
		update();
	}

	/**
	 * @param cd class definition data structure
	 * @param brd blocking region definition data structure
	 * @param key search's key for given blocking region
	 */
	public BlockingStationPanel(ClassDefinition cd, StationDefinition sd, BlockingRegionDefinition brd, Object key) {
		this.cd = cd;
		this.bd = brd;
		this.sd = sd;
		this.regionKey = key;
		stations = new Vector<Object>();
		comboFactory = new ImagedComboBoxCellEditorFactory(sd);
		initComponent();
		addActions();
	}

	/**
	 * Sets data for this panel
	 * @param cd class definition data structure
	 * @param brd blocking region definition data structure
	 * @param key search's key for given blocking region
	 */
	public void setData(ClassDefinition cd, StationDefinition sd, BlockingRegionDefinition brd, Object key) {
		this.cd = cd;
		this.sd = sd;
		this.bd = brd;
		this.regionKey = key;
		comboFactory.setData(sd);
		blockingPanel.setData(cd, brd, key);
		stationPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Stations in " + bd.getRegionName(regionKey)));
	}

	/**
	 * Initialize all gui related stuff
	 */
	private void initComponent() {
		setLayout(new GridLayout(2, 1));
		stationPanel = new JPanel(new BorderLayout(5, 5));
		stationPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Stations in " + bd.getRegionName(regionKey)));

		// Creates panel with add station button and spinner
		JPanel addPanel = new JPanel(new BorderLayout());
		addStation = new JButton("Add Station");
		addStation.setToolTipText("Adds a station to selected blocking region");
		addStation.setMinimumSize(DIM_BUTTON_S);
		addPanel.add(addStation, BorderLayout.CENTER);
		//build spinner panel
		JPanel spinnerPanel = new JPanel();
		JLabel spinnerDescrLabel = new JLabel("Stations:");
		stationNumSpinner = new JSpinner();
		stationNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		spinnerPanel.add(spinnerDescrLabel);
		spinnerPanel.add(stationNumSpinner);
		addPanel.add(spinnerPanel, BorderLayout.SOUTH);
		// Creates a tmp panel to put addStation panel on the northeast corner
		JPanel tmpPanel = new JPanel(new BorderLayout());
		tmpPanel.add(addPanel, BorderLayout.NORTH);
		stationPanel.add(tmpPanel, BorderLayout.EAST);
		// Creates table to display stations
		stationTable = new StationTable();
		WarningScrollTable warning = new WarningScrollTable(stationTable, WARNING_STATIONS);
		warning.addCheckVector(sd.getStationKeysNoSourceSink());
		stationPanel.add(warning);
		add(stationPanel);

		// Creates blocking region panel
		blockingPanel = new BlockingRegionParameterPanel(cd, bd, regionKey);
		// Hides unneeded global properties (specified by table)
		blockingPanel.setGlobalVisible(false);
		add(blockingPanel);
	}

	/**
	 * Adds acrion listeners to gui elements
	 */
	private void addActions() {
		// Add button
		addStation.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				add();
				update();
			}
		});

		stationNumSpinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				try {
					int i = ((Integer) stationNumSpinner.getValue()).intValue();
					// Avoid useless computations
					if (i == stations.size()) {
						return;
					}

					// Limit to maximum number of insertable stations
					if (i - stations.size() > bd.getBlockableStationKeys().size()) {
						i = bd.getBlockableStationKeys().size() + stations.size();
					}

					if (i < stations.size() && i >= 0) {
						while (i < stations.size()) {
							// Removes last element.
							Object o = stations.remove(stations.size() - 1);
							bd.removeRegionStation(regionKey, o);
						}
					} else if (i > stations.size()) {
						while (i > stations.size()) {
							add();
						}
					}
					update();
				} catch (ClassCastException e1) {
					e1.printStackTrace();
				}
			}
		});
	}

	/**
	 * Updates table and spinner values to reflect number of stations
	 */
	protected void update() {
		// Fires a table change event
		stationTable.tableChanged(new TableModelEvent(stationTable.getModel()));
		// Updates spinner value
		stationNumSpinner.setValue(new Integer(stations.size()));
		// Check if stations can be added
		addStation.setEnabled(bd.getBlockableStationKeys().size() > 0);
	}

	/**
	 * Adds a new station to current blocking region. Takes the first available station
	 */
	protected void add() {
		if (bd.getBlockableStationKeys().size() > 0) {
			Object key = bd.getBlockableStationKeys().get(0);
			bd.addRegionStation(regionKey, key);
			stations.add(key);
		} else {
			addStation.setEnabled(false);
		}
	}

	/**
	 * Table used to show stations inside a region
	 */
	protected class StationTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/** Cell renderer for deletion */
		private ButtonCellEditor deleteRenderer;

		/**
		 * Builds a new Blocking region Station Table
		 */
		public StationTable() {
			super(new StationTableModel());
			setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			delete.setFocusable(false);
			deleteRenderer = new ButtonCellEditor(delete);
			getColumnModel().getColumn(1).setMaxWidth(ROW_HEIGHT);
			getColumnModel().setColumnSelectionAllowed(false);
			setRowHeight(ROW_HEIGHT);
		}

		/**
		 * Returns an appropriate editor for the cell specified by
		 * <code>row</code> and <code>column</code>. If the
		 * <code>TableColumn</code> for this column has a non-null editor,
		 * returns that.  If not, finds the class of the data in this
		 * column (using <code>getColumnClass</code>)
		 * and returns the default editor for this type of data.
		 * <p/>
		 * <b>Note:</b>
		 * Throughout the table package, the internal implementations always
		 * use this method to provide editors so that this default behavior
		 * can be safely overridden by a subclass.
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
				return new ButtonCellEditor(delete);
			} else {
				// Builds an ordered array with station keys that can be added and current one
				SortedSet<Object> tmp = new TreeSet<Object>(bd.getBlockableStationKeys());
				tmp.add(stations.get(row));
				return comboFactory.getEditor(tmp.toArray());
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
			if (column == 1) {
				return deleteRenderer;
			} else {
				return comboFactory.getRenderer();
			}
		}

		/** Button to delete selected station */
		private JButton delete = new JButton(new AbstractAction() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			{
				putValue(Action.SHORT_DESCRIPTION, "Delete");
				putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
			}

			/**
			 * Invoked when an action occurs.
			 */
			public void actionPerformed(ActionEvent e) {
				int index = stationTable.getSelectedRow();
				if (index >= 0 && index < stationTable.getRowCount()) {
					Object key = stations.get(index);
					stations.remove(key);
					bd.removeRegionStation(regionKey, key);
					BlockingStationPanel.this.update();
				}
			}
		});
	}

	/**
	 * Model for table used to show stations inside a region
	 */
	protected class StationTableModel extends AbstractTableModel {

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
			return 2;
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
			return stations.size();
		}

		/**
		 * Returns true if the cell at <code>rowIndex</code> and
		 * <code>columnIndex</code>
		 * is editable.  Otherwise, <code>setValueAt</code> on the cell will not
		 * change the value of that cell.
		 *
		 * @param    rowIndex    the row whose value to be queried
		 * @param    columnIndex    the column whose value to be queried
		 * @return true if the cell is editable
		 * @see #setValueAt
		 */
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return columnIndex == 1 || bd.getBlockableStationKeys().size() > 0;
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
			if (columnIndex == 0) {
				return stations.get(rowIndex);
			} else {
				return null;
			}
		}

		/**
		 * Sets the value in the cell at <code>columnIndex</code> and
		 * <code>rowIndex</code> to <code>aValue</code>.
		 *
		 * @param    aValue         the new value
		 * @param    rowIndex     the row whose value is to be changed
		 * @param    columnIndex the column whose value is to be changed
		 */
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			if (columnIndex == 0) {
				// If value changed
				if (stations.get(rowIndex) != aValue) {
					bd.removeRegionStation(regionKey, stations.get(rowIndex));
					stations.set(rowIndex, aValue);
					bd.addRegionStation(regionKey, aValue);
				}
			}
		}

		/**
		 * Returns the name of the column at <code>columnIndex</code>.  This is used
		 * to initialize the table's column header name.  Note: this name does
		 * not need to be unique; two columns in a table can have the same name.
		 *
		 * @return the name of the column
		 * @param    columnIndex    the index of the column
		 */
		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex == 0) {
				return "Station name";
			} else {
				return "";
			}
		}
	}

}
