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

import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.panels.WarningScrollTable;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 29-lug-2005
 * Time: 11.28.50
 * Modified by Bertoli Marco 9-oct-2005
 */
public class RefSourcePanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected ClassDefinition classData;
	protected StationDefinition stationData;
	protected SimulationDefinition simData;

	protected WarningScrollTable warning;

	private RefSourceTable refSourceTable;

	protected ImagedComboBoxCellEditorFactory combos;

	public RefSourcePanel(ClassDefinition classes, StationDefinition stations, SimulationDefinition sim) {
		super();
		combos = new ImagedComboBoxCellEditorFactory(stations);
		setData(classes, stations, sim);
		initComponents();
		refreshComponents();
	}

	private void initComponents() {
		refSourceTable = new RefSourceTable();
		setLayout(new BorderLayout());
		//setBorder(new TitledBorder(new EtchedBorder(), "Reference Stations"));
		warning = new WarningScrollTable(refSourceTable, WARNING_CLASS_STATION);
		warning.addCheckVector(classData.getClassKeys());
		warning.addCheckVector(stationData.getStationKeys());
		add(warning, BorderLayout.CENTER);
	}

	public void setData(ClassDefinition classes, StationDefinition stations, SimulationDefinition sim) {
		classData = classes;
		stationData = stations;
		simData = sim;
		combos.setData(stations);
		refreshComponents();
	}

	@Override
	public void repaint() {
		refreshComponents();
		super.repaint();
	}

	private void refreshComponents() {
		if (refSourceTable != null) {
			refSourceTable.tableChanged(new TableModelEvent(refSourceTable.getModel()));
		}
		if (warning != null) {
			warning.clearCheckVectors();
			warning.addCheckVector(classData.getClassKeys());
			warning.addCheckVector(stationData.getStationKeys());
		}
	}

	protected class RefSourceTable extends JTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		int[] columnWidths = new int[] { 80, 100 };

		public RefSourceTable() {
			super();
			setModel(new RefSourceTableModel());
			for (int i = 0; i < getColumnCount(); i++) {
				getColumnModel().getColumn(i).setPreferredWidth(columnWidths[i]);
			}
			setRowHeight(ROW_HEIGHT);
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return combos.getEditor(stationData.getStationKeysNoSourceSink());
			} else {
				return super.getCellEditor(row, column);
			}
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 1) {
				return combos.getRenderer();
			} else {
				return super.getCellRenderer(row, column);
			}
		}
	}

	protected class RefSourceTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public int getRowCount() {
			return classData.getClassKeys().size();
		}

		public int getColumnCount() {
			return 2;
		}

		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex == 0) {
				return "Class";
			} else if (columnIndex == 1) {
				return "Reference Station";
			} else {
				return null;
			}
		}

		@Override
		public Class getColumnClass(int columnIndex) {
			return String.class;
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			if (columnIndex == 0) {
				return false;
			}
			if (classData.getClassType(classData.getClassKeys().get(rowIndex)) == CLASS_TYPE_OPEN) {
				return false;
			}
			return true;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = classData.getClassKeys().get(rowIndex);
			if (columnIndex == 0) {
				return classData.getClassName(key);
			} else if (columnIndex == 1) {
				return classData.getClassRefStation(key);
			} else {
				return null;
			}
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object key = classData.getClassKeys().get(rowIndex);
			if (columnIndex == 1 && aValue != null) {
				classData.setClassRefStation(key, aValue);
			}
		}

	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		combos.clearCache();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Reference station";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = refSourceTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
	}
}
