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

package jmt.gui.jaba.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.data.ArrayUtils;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.exact.panels.ForceUpdatablePanel;
import jmt.gui.exact.table.ComboBoxCell;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;
import jmt.gui.exact.table.ListOp;
import jmt.gui.jaba.JabaConstants;
import jmt.gui.jaba.JabaModel;
import jmt.gui.jaba.JabaWizard;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 */

/**
 * 2nd panel: stations number, names, types
 */
public final class StationsPanel extends WizardPanel implements JabaConstants, ForceUpdatablePanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JabaWizard ew;
	private HoverHelp help;
	private static final String helpText = "<html>In this panel you can define the number of stations in the system and their properties.<br><br>"
			+ " To edit values, single-click on the desired cell"
			+ " and start typing.<br> To select stations click or drag on the row headers.<br> <b>For a list of the available operations right-click"
			+ " on the table</b>.<br>" + " Pressing DELETE removes all selected stations from the system.</html>";

	private int stations;
	private int pop;

	//if true, LD stations are allowed
	private boolean LD_enabled_system;

	private String[] stationNames;
	private int[] stationTypes;
	private int nameCounter = 1;

	private List<ListOp> stationOps;
	private boolean hasDeletes;
	private boolean deleting = false;

	private JSpinner stationSpinner = new JSpinner(new SpinnerNumberModel(1, 1, MAX_STATIONS, 1));

	private StationTable stationTable;

	private ChangeListener spinnerListener = new ChangeListener() {
		public void stateChanged(ChangeEvent ce) {
			if (!deleting) {
				updateSizes();
			}
		}
	};

	private AbstractAction deleteStation = new AbstractAction("Delete selected stations") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false));
			putValue(Action.SHORT_DESCRIPTION, "Deletes selected stations from the system");
		}

		public void actionPerformed(ActionEvent e) {
			deleteSelectedStations();
		}
	};

	private AbstractAction deleteOneStation = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Deletes this station");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Close"));
		}

		public void actionPerformed(ActionEvent e) {
		}
	};

	private AbstractAction addStation = new AbstractAction("New Station") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Adds a new Station to Model");
		}

		public void actionPerformed(ActionEvent e) {
			addStation();
		}
	};

	public StationsPanel(JabaWizard ew) {
		this.ew = ew;
		help = ew.getHelp();

		stationOps = new ArrayList<ListOp>();
		sync();
		makeNames();
		initComponents();
	}

	private void sync() {

		hasDeletes = false;
		stationOps.clear();

		/* sync status with data object */
		/* arrays are copied to ensure data object consistency is preserved */
		JabaModel data = ew.getData();
		synchronized (data) {
			stations = data.getStations();
			pop = data.getMaxpop();

			//OLD
			// closedSystem = data.isClosed();

			//NEW
			//@author Stefano Omini
			//if true, load dependent stations are allowed
			//(at the moment, only single class closed models allow LD stations)
			LD_enabled_system = data.isClosed() && !data.isMultiClass();
			//end NEW

			//TODO: sostituire LD??
			stationNames = ArrayUtils.copy(data.getStationNames());
			stationTypes = ArrayUtils.copy(data.getStationTypes());
		}

		stationSpinner.setValue(new Integer(stations));
	}

	@Override
	public void gotFocus() {
		sync();
		stationTable.update();
	}

	@Override
	public void lostFocus() {
		commit();
		//release();
	}

	/**
	 * make up names for null entries
	 */
	private void makeNames() {
		for (int i = 0; i < stationNames.length; i++) {
			if (stationNames[i] == null) {
				stationNames[i] = "Station" + (++nameCounter);
			}
		}
	}

	/**
	 * resize internal data structures according to new values. intended to be called from a listener.
	 */
	private void updateSizes() {
		setNumberOfStations(((Integer) stationSpinner.getValue()).intValue());
	}

	private void addStation() {
		setNumberOfStations(stations + 1);
	}

	private void setNumberOfStations(int number) {
		stationTable.stopEditing();
		stations = number;

		stationNames = ArrayUtils.resize(stationNames, stations, null);
		makeNames();

		stationTypes = ArrayUtils.resize(stationTypes, stations, STATION_LI);

		stationTable.update();
		if (!deleting) {
			stationOps.add(ListOp.createResizeOp(stations));
		}

		stationSpinner.setValue(new Integer(stations));
		stationTable.updateDeleteCommand();
	}

	/**
	 * Set up the panel contents and layout
	 */
	private void initComponents() {
		stationSpinner.addChangeListener(spinnerListener);

		stationTable = new StationTable();

		/* and now some Box black magic */

		Box stationSpinnerBox = Box.createHorizontalBox();
		//OLD
		//DEK (Federico Granata) 26-09-2003
		//JLabel spinnerLabel = new JLabel("<html><font size=\"4\">Set the number of stations (1-" + MAX_STATIONS + "):</font></html>");
		//NEW
		//@author Stefano
		JLabel spinnerLabel = new JLabel(DESCRIPTION_STATIONS);

		//spinnerLabel.setMaximumSize(new Dimension(300, 18));
		stationSpinnerBox.add(spinnerLabel);
		stationSpinnerBox.add(Box.createHorizontalStrut(10));
		Box numberBox = Box.createVerticalBox();

		JPanel spinnerPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		JLabel numberLabel = new JLabel("Number:");
		stationSpinner.setMaximumSize(new Dimension(600, 18));
		spinnerPanel.add(numberLabel);
		spinnerPanel.add(stationSpinner);
		numberBox.add(spinnerPanel);

		numberBox.add(new JButton(addStation));

		numberBox.setMaximumSize(new Dimension(150, 50));

		stationSpinnerBox.add(numberBox);
		//END

		Box stationBox = Box.createVerticalBox();
		stationBox.add(Box.createVerticalStrut(20));
		stationBox.add(stationSpinnerBox);
		stationBox.add(Box.createVerticalStrut(10));
		JScrollPane stationTablePane = new JScrollPane(stationTable);
		stationTablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		stationTablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		stationBox.add(stationTablePane);
		stationBox.add(Box.createRigidArea(new Dimension(10, 20)));

		Box totalBox = Box.createHorizontalBox();
		totalBox.add(Box.createHorizontalStrut(20));
		totalBox.add(stationBox);
		totalBox.add(Box.createHorizontalStrut(20));

		setLayout(new BorderLayout());
		add(totalBox, BorderLayout.CENTER);

	}

	@Override
	public String getName() {
		return "Stations";
	}

	private void commit() {
		if (stationSpinner.getEditor().getComponent(0).hasFocus()) {
			try {
				stationSpinner.commitEdit();
				updateSizes();
			} catch (java.text.ParseException e) {
			}
		}

		stationTable.stopEditing();

		JabaModel data = ew.getData();
		synchronized (data) {

			if (hasDeletes) {
				playbackStationOps(data); //play back ops on the data object
			} else {
				data.resize(stations, data.getClasses());
			}
			data.setStationNames(stationNames);
			data.setStationTypes(stationTypes);

			//NEW
			//@author Stefano Omini
			sync();
			//end NEW
		}

	}

	@Override
	public boolean canFinish() {
		return checkLD() && !areThereDuplicates();
	}

	private void deleteSelectedStations() {
		int[] selectedRows = stationTable.getSelectedRows();
		int nrows = selectedRows.length;
		if (nrows == 0) {
			return;
		}
		int left = stationTable.getRowCount() - nrows;
		if (left < 1) {
			stationTable.removeRowSelectionInterval(selectedRows[nrows - 1], selectedRows[nrows - 1]);
			deleteSelectedStations();
			return;
		}
		deleteStations(selectedRows);
	}

	private void deleteStations(int[] idx) {
		deleting = true;
		Arrays.sort(idx);
		for (int i = idx.length - 1; i >= 0; i--) {
			deleteStation(idx[i]);
		}
		updateSizes();
		deleting = false;
	}

	private void deleteStation(int i) {
		stations--;
		stationSpinner.setValue(new Integer(stations));

		stationNames = ArrayUtils.delete(stationNames, i);
		stationTypes = ArrayUtils.delete(stationTypes, i);

		stationOps.add(ListOp.createDeleteOp(i));
		hasDeletes = true;

	}

	private void playbackStationOps(JabaModel data) {
		for (int i = 0; i < stationOps.size(); i++) {
			ListOp lo = stationOps.get(i);
			if (lo.isDeleteOp()) {
				data.deleteStation(lo.getData());
			}
			if (lo.isResizeOp()) {
				data.resize(lo.getData(), data.getClasses());
			}

		}
	}

	@Override
	public boolean canGoBack() {
		checkLD();
		if (areThereDuplicates()) {
			return false;
		}
		return true;
	}

	@Override
	public boolean canGoForward() {
		checkLD();
		if (areThereDuplicates()) {
			return false;
		}
		return true;
	}

	/**
	 * @return true if the system contains LD stations
	 */
	private boolean isLD() {
		for (int i = 0; i < stations; i++) {
			if (stationTypes[i] == STATION_LD) {
				return true;
			}
		}
		return false;
	}

	//checks population of classes for ld stations
	private boolean checkLD() {
		stationTable.stopEditing();
		if (pop < 1 && isLD()) {
			JOptionPane
					.showMessageDialog(
							this,
							"<html><center>A system with zero customers cannot have load dependent stations.<br>Increase the number of customers or remove all load dependent stations.</center></html>",
							"Warning", JOptionPane.WARNING_MESSAGE);
			return false;
		}
		return true;
	}

	//checks for presence of classes with same name
	private boolean areThereDuplicates() {

		boolean thereAreDupl = false;
		for (int i = 0; i < stationNames.length; i++) {
			for (int j = i + 1; j < stationNames.length; j++) {
				thereAreDupl = thereAreDupl || stationNames[i].equalsIgnoreCase(stationNames[j]);
			}
		}
		if (thereAreDupl) {
			JOptionPane.showMessageDialog(this,
					"<html><center>Two or more stations in this system are identified by the same name.<br>Please modify names.</center></html>",
					"Warning", JOptionPane.WARNING_MESSAGE);
			return true;
		} else {
			return false;
		}
	}

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);

	}

	//NEW Federico Dall'Orso
	//Methods added to implement forcing of data refresh
	public void retrieveData() {
		sync();
	}

	public void commitData() {
		commit();
	}

	//END

	/* ------------------------------------------------------------------
	   The StationTable is a fairly complex object that would be probably better of as an outer class.
	   However, it is very specialized and it needs access to the data structures of the StationsPanel,
	   so having it as an inner class is *much* more practical
	   ------------------------------------------------------------------
	   */

	/**
	 * the nifty station table
	 */
	private class StationTable extends ExactTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private TableCellEditor LD_disabled_StationTypeEditor, LD_enabled_StationTypeEditor;
		//BEGIN Federico Dall'Orso 8/3/2005
		//NEW
		private ComboBoxCell LD_disabled_StationTypeCell, LD_enabled_StationTypeCell;
		//14/3/2005
		private JButton deleteButton;
		private ButtonCellEditor deleteButtonCellRenderer;

		//END Federico Dall'Orso

		StationTable() {
			super(new StationTableModel());

			//BEGIN Federico Dall'Orso 8/3/2005
			//station type cell renderers
			//NEW
			LD_disabled_StationTypeCell = new ComboBoxCell(STATION_TYPENAMES);

			LD_disabled_StationTypeCell.getComboBox().setEnabled(false);
			LD_enabled_StationTypeCell = new ComboBoxCell(STATION_TYPENAMES_LD_ENABLED);

			LD_enabled_StationTypeCell.getComboBox().setEnabled(false);
			//14/3/2005
			deleteButton = new JButton(deleteOneStation);
			deleteButtonCellRenderer = new ButtonCellEditor(deleteButton);
			enableDeletes();
			rowHeader.setRowHeight(18);
			setRowHeight(18);
			//END Federico Dall'Orso

			/* a station type cell editor for open/mixed systems */
			JComboBox stationTypeBox = new JComboBox(STATION_TYPENAMES);
			stationTypeBox.setEditable(false);
			stationTypeBox.setEnabled(false);
			LD_disabled_StationTypeEditor = new DefaultCellEditor(stationTypeBox);
			stationTypeBox.setEnabled(false);

			/* a station type cell editor for closed systems */
			JComboBox LD_enabled_StationTypeBox = new JComboBox(STATION_TYPENAMES_LD_ENABLED);
			LD_enabled_StationTypeBox.setEditable(false);
			LD_enabled_StationTypeBox.setEnabled(false);
			LD_enabled_StationTypeEditor = new DefaultCellEditor(LD_enabled_StationTypeBox);
			LD_enabled_StationTypeBox.setEnabled(false);

			setColumnSelectionAllowed(false);
			setRowSelectionAllowed(true);

			setDisplaysScrollLabels(true);

			installKeyboardAction(getInputMap(), getActionMap(), deleteStation);
			mouseHandler = new ExactTable.MouseHandler(makeMouseMenu());
			mouseHandler.install();

			help.addHelp(this,
					"Click or drag to select stations; to edit data single-click and start typing. Right-click for a list of available operations");
			help.addHelp(moreRowsLabel, "There are more stations: scroll down to see them");
			help.addHelp(selectAllButton, "Click to select all stations");
			tableHeader.setToolTipText(null);
			rowHeader.setToolTipText(null);
			help.addHelp(rowHeader, "Click, SHIFT-click or drag to select stations");
		}

		//BEGIN Federico Dall'Orso 14/3/2005
		/*enables deleting operations with last column's button*/
		private void enableDeletes() {
			deleteOneStation.setEnabled(stations > 1);
			/*It seems the only way to implement row deletion...*/
			this.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					if ((columnAtPoint(e.getPoint()) == getColumnCount() - 1) && getRowCount() > 1) {
						setRowSelectionInterval(rowAtPoint(e.getPoint()), rowAtPoint(e.getPoint()));
						deleteSelectedStations();
					}
				}
			});
			getColumnModel().getColumn(getColumnCount() - 1).setMinWidth(20);
			getColumnModel().getColumn(getColumnCount() - 1).setMaxWidth(20);
		}

		//END Federico Dall'Orso 14/3/2005

		@Override
		protected void installKeyboard() {
		}

		@Override
		protected void installMouse() {
		}

		@Override
		protected JPopupMenu makeMouseMenu() {
			JPopupMenu menu = new JPopupMenu();
			menu.add(deleteStation);
			return menu;
		}

		/**
		 * Overridden to ensure proper handling of station type column
		 */
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) { //station type
				/* select the right editor */

				if (LD_enabled_system) {
					return LD_enabled_StationTypeEditor;
				} else {
					return LD_disabled_StationTypeEditor;
				}
			} else {
				return super.getCellEditor(row, column);
			}
		}

		//BEGIN Federico Dall'Orso 8/3/2005
		//NEW
		/**Returns combobox-styled cellrenderer if a multiple choice cell is to be rendered.
		 * @return cell renderer*/
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			//if this is type column, I must render it as a combo box instead of a jtextfield
			if (column == 1) {
				if (LD_enabled_system) {
					return LD_enabled_StationTypeCell;
				} else {
					return LD_disabled_StationTypeCell;
				}
			} else if (column == getColumnCount() - 1) {
				return deleteButtonCellRenderer;
			} else {
				return new DefaultTableCellRenderer();
			}
		}

		//END Federico Dall'Orso 8/3/2005

		//BEGIN Federico Dall'Orso 14/3/2005
		//NEW
		//Updates appearence of last column's buttons
		void updateDeleteCommand() {
			deleteOneStation.setEnabled(stations > 1);
			getColumnModel().getColumn(getColumnCount() - 1).setMinWidth(20);
			getColumnModel().getColumn(getColumnCount() - 1).setMaxWidth(20);
		}

		//END Federico Dall'Orso 14/3/2005

		@Override
		protected void updateActions() {
			deleteStation.setEnabled(stations > 1 && getSelectedRowCount() > 0);
			deleteOneStation.setEnabled(stations > 1);
		}

	}

	/**
	 * the model backing the station table
	 */
	private class StationTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private Object[] prototypes = { "10000", new String(new char[15]), new String(new char[15]), "" };

		@Override
		public Object getPrototype(int columnIndex) {
			return prototypes[columnIndex + 1];
		}

		public int getRowCount() {
			return stations;
		}

		public int getColumnCount() {
			return 3;
		}

		@Override
		public String getColumnName(int index) {
			switch (index) {
				case 0:
					return "Name";
				case 1:
					return "Type";
				default:
					return null;
			}
		}

		@Override
		protected Object getRowName(int rowIndex) {
			return new Integer(rowIndex + 1);
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			switch (columnIndex) {
				case 0://name
					return stationNames[rowIndex];
				case 1://type
					return STATION_TYPENAMES_LD_ENABLED[stationTypes[rowIndex]];
				default:
					return null;
			}
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			switch (columnIndex) {
				case 0: //name
					stationNames[rowIndex] = (String) value;
					break;
				case 1: //type
					for (int i = 0; i < STATION_TYPENAMES_LD_ENABLED.length; i++) {
						if (value == STATION_TYPENAMES_LD_ENABLED[i]) { //literal strings are canonical objects, hence == is ok
							stationTypes[rowIndex] = i;
							break;
						}
					}
					break;
				default:
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			switch (columnIndex) {
				case 0:
					return true;
				case 1:
					return false;
				default:
					return false;
			}
		}
	}

}