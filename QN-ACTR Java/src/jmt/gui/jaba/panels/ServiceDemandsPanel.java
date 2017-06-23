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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import java.util.EventObject;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.table.TableCellEditor;

import jmt.framework.data.ArrayUtils;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.panels.ForceUpdatablePanel;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;
import jmt.gui.jaba.JabaConstants;
import jmt.gui.jaba.JabaModel;
import jmt.gui.jaba.JabaWizard;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 */

/**
 * 4th panel: service times
 */
public final class ServiceDemandsPanel extends WizardPanel implements JabaConstants, ForceUpdatablePanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// Bertoli Marco - Used to show only two decimal digits
	private static DecimalFormat formatter = new DecimalFormat("#0.00");

	private JabaWizard ew;
	private HoverHelp help;
	private static final String helpText = "<html>In this panel you can edit service demands of LI and delay stations for each class.<br><br>"
			+ " To enter values, single-click on desired cell"
			+ " and start typing.<br> To select multiple cells drag mouse on them; click or drag on"
			+ " row/column headers to select the whole of rows/columns.<br> <b>For a list of available operations right-click"
			+ " on the table</b>; all operations except pasting affect selected cells.<br>"
			+ " To copy one value to multiple cells click on the cell containing the value, select"
			+ " target cells by dragging and then select <b>\"Fill\"</b>.<br><br>"
			+ " To edit service demands of an LD station double-click anywhere on its row.<br></html>";

	private boolean zeroLD;
	private int classes, stations;
	private String[] classNames;
	private String[] stationNames;
	private int[] stationTypes;
	private double[][][] serviceDemands;

	private STTable stTable;

	private AbstractAction SWITCH_TO_ST_V = new AbstractAction("Service Times and Visits") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Input by Service Times and Visits");
		}

		public void actionPerformed(ActionEvent e) {
			ew.switchFromSDtoSTV();
		}
	};

	public ServiceDemandsPanel(JabaWizard ew) {
		this.ew = ew;
		help = ew.getHelp();

		/* sync status with data object */
		sync();
		initComponents();
	}

	/**
	 * gets status from data object
	 */
	public void sync() {
		/* arrays are copied to ensure data object consistency is preserved */
		JabaModel data = ew.getData();
		synchronized (data) {
			zeroLD = (data.isLd() && (data.getMaxpop() == 0));
			classes = data.getClasses();
			stations = data.getStations();

			classNames = data.getClassNames();
			stationNames = data.getStationNames();
			stationTypes = data.getStationTypes();

			serviceDemands = ArrayUtils.copy3per2(data.getServiceTimes(), data.getVisits());
		}
	}

	/**
	 * Set up the panel contents and layout
	 */
	private void initComponents() {

		stTable = new STTable();

		/* and now some Box black magic */

		Box hBox = Box.createHorizontalBox();
		hBox.add(Box.createHorizontalStrut(20));
		//Horizontal box containing Description label and buttons
		Box descrBox = Box.createVerticalBox();
		descrBox.add(new JLabel(DESCRIPTION_SERVICEDEMANDS));
		descrBox.add(Box.createHorizontalStrut(10));
		descrBox.add(new JButton(SWITCH_TO_ST_V));
		descrBox.setPreferredSize(new Dimension(220, 1000));
		descrBox.setMinimumSize(new Dimension(200, 200));

		hBox.add(descrBox);
		hBox.add(Box.createHorizontalStrut(10));
		JScrollPane visitTablePane = new JScrollPane(stTable);
		visitTablePane.setPreferredSize(new Dimension(1000, 1000));
		visitTablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		visitTablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		hBox.add(visitTablePane);
		hBox.add(Box.createHorizontalStrut(20));

		Box totalBox = Box.createVerticalBox();
		totalBox.add(Box.createVerticalStrut(20));
		totalBox.add(hBox);
		totalBox.add(Box.createVerticalStrut(20));

		setLayout(new BorderLayout());
		add(totalBox, BorderLayout.CENTER);

	}

	@Override
	public String getName() {
		return "Service Demands";
	}

	private void commit() {

		stTable.stopEditing();

		JabaModel data = ew.getData();
		synchronized (data) {
			data.setServiceTimes(serviceDemands);
			data.setVisits(createUnitaryVisits());
		}
	}

	private double[][] createUnitaryVisits() {
		double[][] visits = new double[serviceDemands.length][];
		for (int i = 0; i < serviceDemands.length; i++) {
			visits[i] = new double[serviceDemands[i].length];
			for (int j = 0; j < serviceDemands[i].length; j++) {
				if (serviceDemands[i][j][0] != 0) {
					visits[i][j] = 1;
				} else {
					visits[i][j] = 0;
				}
			}
		}
		return visits;
	}

	@Override
	public void gotFocus() {
		sync();
		stTable.updateStructure();
	}

	@Override
	public void lostFocus() {
		commit();
		//release();
	}

	/**
	 * Make sure we can't finish if we are editing LD data
	 */
	@Override
	public boolean canFinish() {
		return !stTable.isLDEditing();
	}

	/**
	 * Make sure we can't switch tabs if we are editing LD data
	 */
	@Override
	public boolean canGoBack() {
		return !stTable.isLDEditing();
	}

	/**
	 * Make sure we can't switch tabs if we are editing LD data
	 */
	@Override
	public boolean canGoForward() {
		return !stTable.isLDEditing();
	}

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);

	}

	/**{@see ForceUpdatablePanel} for further details*/
	public void retrieveData() {
		this.sync();
	}

	/**{@see ForceUpdatablePanel} for further details*/
	public void commitData() {
		this.commit();
	}

	private class STTable extends ExactTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		STTable() {
			super(new STTableModel());
			autoResizeMode = AUTO_RESIZE_OFF;

			setDisplaysScrollLabels(true);

			setRowSelectionAllowed(true);
			setColumnSelectionAllowed(true);
			setClipboardTransferEnabled(true);

			help.addHelp(this,
					"Click or drag to select cells; to edit data single-click and start typing. Right-click for a list of available operations");
			help.addHelp(moreColumnsLabel, "There are more classes: scroll right to see them");
			help.addHelp(moreRowsLabel, "There are more stations: scroll down to see them");
			help.addHelp(selectAllButton, "Click to select all cells");
			tableHeader.setToolTipText(null);
			help.addHelp(tableHeader, "Click, SHIFT-click or drag to select columns");
			rowHeader.setToolTipText(null);
			help.addHelp(rowHeader, "Click, SHIFT-click or drag to select rows");

		}

		/**
		 * We don't want selectable columns
		 */
		/*public boolean getColumnSelectionAllowed() {return false;} */

		/**
		 * A little hack to make LD rows selectable
		 * @return true iif the focused cell is on a LD row
		 */
		/*public boolean getRowSelectionAllowed() {
			if (stationTypes[selectionModel.getAnchorSelectionIndex()]==STATION_LD) return true;
			return false;
		} */

		/**
		 * @return true if the LDEditor window is currently open
		 */
		public boolean isLDEditing() {
			return (false);
		}

		/**
		 * the LDEditor needs to be treated in a special way.
		 */
		@Override
		public Component prepareEditor(TableCellEditor editor, int row, int column) {
			return super.prepareEditor(editor, row, column);
		}

		/**
		 * If the request is to edit ld times in a system with zero customers, shows a warning messages and returns false.
		 * Otherwise passes request to superclass method
		 */
		@Override
		public boolean editCellAt(int row, int col, EventObject e) {
			if (zeroLD && (stationTypes[row] == STATION_LD)) {
				JOptionPane.showMessageDialog(ServiceDemandsPanel.this,
						"<html><center>Cannot edit LD service times in a system with zero customers</center></html>", "Warning",
						JOptionPane.WARNING_MESSAGE);
				return false;
			}
			return super.editCellAt(row, col, e);
		}
	}

	/**
	 * the model backing the service times table.
	 * Rows represent stations, columns classes.
	 */
	private class STTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		STTableModel() {
			prototype = "LD Settings Button";
			rowHeaderPrototype = "Station10000";
		}

		public int getRowCount() {
			return stations;
		}

		public int getColumnCount() {
			return classes;
		}

		@Override
		public String getColumnName(int index) {
			return classNames[index];
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			switch (stationTypes[rowIndex]) {
				case STATION_LI:
				case STATION_DELAY:
					return formatter.format(serviceDemands[rowIndex][columnIndex][0]);
				case STATION_LD:
					return "LD";
				default:
					return null;
			}
		}

		@Override
		protected Object getRowName(int rowIndex) {
			return stationNames[rowIndex];
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			if (value instanceof String) { //coming from the defaultEditor
				//if ("LD".equals((String)value)) return;
				try {
					double newVal = Double.parseDouble((String) value);
					if (newVal <= 0.01) {
						serviceDemands[rowIndex][columnIndex][0] = 0.01;
					} else {
						serviceDemands[rowIndex][columnIndex][0] = newVal;
					}
				} catch (NumberFormatException e) {
				}
			} else if (value instanceof double[][]) { // coming from the LDEditor
				serviceDemands[rowIndex] = (double[][]) value;
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

		@Override
		public void clear(int row, int col) {
			if (stationTypes[row] == STATION_LD) {
				return;
			}
			serviceDemands[row][col][0] = 0.01;
		}

		/**
		 * Copy the contents of a cell to an area. Works directly on the data set.<br>
		 * Does nothing if the source cell is in an LD row; LD rows are skipped
		 */
		@Override
		public void copyCellToArea(int sourceRow, int sourceCol, int rowFrom, int rowTo, int colFrom, int colTo) {

			if (stationTypes[sourceRow] == STATION_LD) {
				return;
			}

			double source = serviceDemands[sourceRow][sourceCol][0];

			for (int row = rowFrom; row <= rowTo; row++) {
				if (!(stationTypes[row] == STATION_LD)) {
					for (int col = colFrom; col <= colTo; col++) {
						serviceDemands[row][col][0] = source;
					}
				}
			}
		}

	}

}
