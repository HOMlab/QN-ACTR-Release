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

package jmt.gui.jmodel.panels;

import java.awt.Color;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.table.editors.ColorCellEditor;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.jmodel.JMODELConstants;
import jmt.gui.jmodel.definitions.JmodelClassDefinition;
import jmt.gui.jmodel.definitions.JmodelStationDefinition;
import jmt.gui.jsim.panels.ClassesPanel;

/**
 * <p>Title: JModel User Classes Panel</p>
 * <p>Description: Panel used to edit user classes properties. Extends JSIM ClassesPanel.
 * In some points it is a bit tricky, as original ClassesPanel was not meant to be extended.
 * I preferred not to change ClassesPanel source code, nor copy and modificate it as
 * at the time of writing it was in early development stage and I didn't want to collide
 * with the author.</p>
 * 
 * @author Bertoli Marco
 *         Date: 14-giu-2005
 *         Time: 9.42.15
 */
public class jmodelClassesPanel extends ClassesPanel implements JMODELConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected JmodelClassDefinition classdefinition;
	protected JmodelStationDefinition stationdefinition;

	protected ImagedComboBoxCellEditorFactory stations;

	/**
	 * Construct a new jmodelClassesPanel
	 * @param cd a reference to the underlayng data structure
	 */
	public jmodelClassesPanel(JmodelClassDefinition cd, JmodelStationDefinition sd) {
		super();
		classdefinition = cd;
		stationdefinition = sd;
		stations = new ImagedComboBoxCellEditorFactory(sd);
		classTable = new jmodelClassTable();
		initComponents();
		classTable.getTableHeader().setReorderingAllowed(false);
		setData(cd);
	}

	/**Sets data model for this panel.
	 * Instantly all of the panel components are assigned their specific value.
	 * @param cd: data for class definition.*/
	@Override
	public void setData(ClassDefinition cd) {
		data = cd;
		classTable.setModel(new jmodelClassTableModel());
		classNumSpinner.setValue(new Integer(data.getClassKeys().size()));
	}

	/**
	 * Adds a new class
	 */
	@Override
	protected void addClass() {
		classdefinition.addClass();
		refreshComponents();
	}

	/**
	 * Overrides ClassTable. It is a table containing class parameters.
	 * Added suppport for "Color" column and rererence station combobox
	 */
	protected class jmodelClassTable extends ClassTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public jmodelClassTable() {
			super();
			columnSizes = new int[] { 8, 100, 60, 28, 38, 140, 39, 90, 18 };
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 0) {
				return new ColorCellEditor();
			} else if (column == 2) {
				return super.getCellRenderer(row, 1);
			} else if (column == 6) {
				/*if distribution column contains null value, no editor must be displayed,
				as this class is a closed one (e.g. described by population)*/
				if (getValueAt(row, column - 1) != null) {
					return new ButtonCellEditor(editDistributionButton);
				} else {
					return getDefaultRenderer(String.class);
				}
			}
			// Column for refercence source
			else if (column == 7) {
				return stations.getRenderer();
			} else if (column == 8) {
				return new ButtonCellEditor(deleteButton);
			} else {
				return getDefaultRenderer(getModel().getColumnClass(column));
			}
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 0) {
				return new ColorCellEditor();
			} else if (column == 6 && getValueAt(row, column - 1) != null) {
				return new ButtonCellEditor(new JButton(editDistribution));
			} else if (column == 7 && getValueAt(row, 2).equals("Open")) {
				return stations.getEditor(stationdefinition.getStationKeysSource());
			} else if (column == 7 && getValueAt(row, 2).equals("Closed")) {
				return stations.getEditor(stationdefinition.getStationKeysNoSourceSink());
			} else if (column == 8) {
				return new ButtonCellEditor(new JButton(deleteClass));
			} else {
				return super.getCellEditor(row, column - 1);
			}
		}
	}

	/**
	 * Define a custom table model. Overrides ClassTableModel to add support for "Color" column
	 */
	protected class jmodelClassTableModel extends ClassTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public jmodelClassTableModel() {
			super();
			columnNames = new String[] { "Color", "Name", "Type", "Priority", "Population", "Interarrival Time Distribution", "",
					"Reference Station", "" };
			colClasses = new Class[] { Color.class, String.class, JComboBox.class, String.class, String.class, String.class, Object.class,
					JComboBox.class, JButton.class };
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			if (columnIndex == 0) {
				return classdefinition.getClassColor(classdefinition.getClassKeys().get(rowIndex));
			} else if (columnIndex == 7) {
				return classdefinition.getClassRefStation(classdefinition.getClassKeys().get(rowIndex));
			} else {
				return super.getValueAt(rowIndex, columnIndex - 1);
			}
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			if (columnIndex == 0) {
				classdefinition.setClassColor(classdefinition.getClassKeys().get(rowIndex), (Color) aValue);
			} else if (columnIndex == 7) {
				classdefinition.setClassRefStation(classdefinition.getClassKeys().get(rowIndex), aValue);
			} else {
				super.setValueAt(aValue, rowIndex, columnIndex - 1);
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			if (columnIndex == 4 && (getValueAt(rowIndex, 2).equals("Open"))) {
				return false;
			}
			if (columnIndex == 6 && (getValueAt(rowIndex, 2).equals("Closed"))) {
				return false;
			}
			if (columnIndex == 5) {
				return false;
			}
			return true;
		}
	}
}
