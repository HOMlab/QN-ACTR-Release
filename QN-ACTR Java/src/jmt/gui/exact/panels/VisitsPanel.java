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
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import jmt.framework.data.ArrayUtils;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 */

/**
 * 3rd panel: visits
 */
public final class VisitsPanel extends WizardPanel implements ExactConstants, ForceUpdatablePanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ExactWizard ew;
	private HoverHelp help;
	private static final String helpText = "<html>In this panel you can edit the station visits for each class.<br><br>"
			+ " To enter values, single-click on the desired cell"
			+ " and start typing.<br> To select multiple cells drag the mouse on them; click or drag on"
			+ " row/column headers to select whole rows/columns.<br> <b>For a list of the available operations right-click"
			+ " on the table</b>; all operations except pasting affect selected cells.<br>"
			+ " To copy one value to multiple cells click on the cell containing the value, select the"
			+ " target cells by dragging and select <b>\"Fill\"</b>.<br><br></html>";

	private int classes, stations;
	private String[] classNames;
	private String[] stationNames;
	private double[][] visits;

	private VisitTable visitTable;

	public VisitsPanel(ExactWizard ew) {
		this.ew = ew;
		help = ew.getHelp();

		/* sync status with data object */
		sync();
		initComponents();
	}

	/**
	 * gets status from data object
	 */
	private void sync() {
		/* arrays are copied to ensure data object consistency is preserved */
		ExactModel data = ew.getData();
		synchronized (data) {
			classes = data.getClasses();
			stations = data.getStations();

			classNames = data.getClassNames();
			stationNames = data.getStationNames();
			visits = ArrayUtils.copy2(data.getVisits());
		}
	}

	/**
	 * Set up the panel contents and layout
	 */
	private void initComponents() {

		visitTable = new VisitTable();

		JPanel totalBox = new JPanel(new BorderLayout(10, 10));

		//Horizontal box containing Description label and buttons
		JLabel descrLabel = new JLabel(DESCRIPTION_VISITS);
		JPanel descrBox = new JPanel(new BorderLayout());
		descrBox.setPreferredSize(new Dimension(200, 1000));
		descrBox.add(descrLabel, BorderLayout.NORTH);

		JScrollPane visitTablePane = new JScrollPane(visitTable);
		visitTablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		visitTablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		totalBox.add(visitTablePane, BorderLayout.CENTER);
		totalBox.add(descrBox, BorderLayout.WEST);

		setLayout(new BorderLayout());
		add(totalBox, BorderLayout.CENTER);
		add(Box.createVerticalStrut(20), BorderLayout.NORTH);
		add(Box.createVerticalStrut(20), BorderLayout.SOUTH);
		add(Box.createHorizontalStrut(20), BorderLayout.EAST);
		add(Box.createHorizontalStrut(20), BorderLayout.WEST);

	}

	@Override
	public String getName() {
		return "Visits";
	}

	private void commit() {

		visitTable.stopEditing();

		ExactModel data = ew.getData();
		synchronized (data) {
			if (data.setVisits(visits)) {
				data.recalculateWhatifValues();
			}
		}
	}

	@Override
	public void gotFocus() {
		sync();
		visitTable.updateStructure();
	}

	@Override
	public void lostFocus() {
		commit();
		//release();
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

	private class VisitTable extends ExactTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		VisitTable() {
			super(new VisitTableModel());
			autoResizeMode = AUTO_RESIZE_OFF;

			setDisplaysScrollLabels(true);
			help.addHelp(this,
					"Click or drag to select cells; to edit data single-click and start typing. Right-click for a list of available operations");
			help.addHelp(moreColumnsLabel, "There are more classes: scroll right to see them");
			help.addHelp(moreRowsLabel, "There are more stations: scroll down to see them");
			help.addHelp(selectAllButton, "Click to select all cells");
			tableHeader.setToolTipText(null);
			help.addHelp(tableHeader, "Click, SHIFT-click or drag to select columns");
			rowHeader.setToolTipText(null);
			help.addHelp(rowHeader, "Click, SHIFT-click or drag to select rows");

			setRowSelectionAllowed(true);
			setColumnSelectionAllowed(true);
			setClipboardTransferEnabled(true);

		}

	}

	/**
	 * the model backing the visit table.
	 * Rows represent stations, columns classes.
	 */
	private class VisitTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		VisitTableModel() {
			prototype = new Double(1000);
			rowHeaderPrototype = "Station1000";
		}

		public int getRowCount() {
			return stations;
		}

		public int getColumnCount() {
			return classes;
		}

		@Override
		protected Object getRowName(int rowIndex) {
			return stationNames[rowIndex];
		}

		@Override
		public String getColumnName(int index) {
			return classNames[index];
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			return new Double(visits[rowIndex][columnIndex]);
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			try {
				double newVal = Double.parseDouble((String) value);
				if (newVal >= 0) {
					visits[rowIndex][columnIndex] = newVal;
				}
			} catch (NumberFormatException e) {
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

		@Override
		public void clear(int row, int col) {
			visits[row][col] = 0;
		}

		/**
		 * Copy the contents of a cell to an area. Works directly on the data set.
		 */
		@Override
		public void copyCellToArea(int sourceRow, int sourceCol, int rowFrom, int rowTo, int colFrom, int colTo) {
			double source = visits[sourceRow][sourceCol];

			for (int row = rowFrom; row <= rowTo; row++) {
				for (int col = colFrom; col <= colTo; col++) {
					visits[row][col] = source;
				}
			}
		}

	}
}
