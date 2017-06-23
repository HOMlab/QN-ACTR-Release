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
import java.awt.Dimension;
import java.util.Vector;

import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ComboBoxCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.editors.RoutingProbabilitiesEditor;
import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 8-lug-2005
 * Time: 10.20.54
 * Modified by Bertoli Marco 7-oct-2005
 *                           9-jan-2006  --> ComboBoxCellEditor
 */
public class RoutingSectionPanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private StationDefinition stationData;
	private ClassDefinition classData;
	private Object stationKey;
	private JSplitPane mainPanel;

	private RoutingSelectionTable routingStrategies;
	private RoutingProbabilitiesEditor routingProbEditor;

	/** Used to display classes with icon */
	protected ImagedComboBoxCellEditorFactory classEditor;

	public RoutingSectionPanel(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		classEditor = new ImagedComboBoxCellEditorFactory(cd);
		setData(sd, cd, stationKey);
		routingStrategies = new RoutingSelectionTable();
		routingProbEditor = new RoutingProbabilitiesEditor(stationData, stationKey, null);
		initComponents();
	}

	public void setData(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		stationData = sd;
		classData = cd;
		this.stationKey = stationKey;
		classEditor.setData(cd);
		if (routingStrategies != null) {
			routingStrategies.tableChanged(new TableModelEvent(routingStrategies.getModel()));
		}
	}

	//Francesco D'Aquino
	public void setSelectedClass(Object classKey) {
		Vector temp = classData.getClassKeys();
		int i;
		for (i = 0; i < temp.size(); i++) {
			if (temp.get(i) == classKey) {
				break;
			}
		}
		routingStrategies.setRowSelectionInterval(i, i);
	}

	protected void initComponents() {
		this.setLayout(new BorderLayout());
		//building mainPanel
		mainPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		mainPanel.setDividerSize(4);
		mainPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		//layout of main panel
		WarningScrollTable jsp = new WarningScrollTable(routingStrategies, WARNING_CLASS);
		//routing strategy selector on the left, routing probabilities editor on the right
		jsp.setBorder(new TitledBorder(new EtchedBorder(), "Routing Strategies"));
		mainPanel.setResizeWeight(.80); // Gives more space to left component
		jsp.setMinimumSize(new Dimension(180, 100));
		mainPanel.setLeftComponent(jsp);
		routingProbEditor.setMinimumSize(new Dimension(160, 100));
		mainPanel.setRightComponent(routingProbEditor);
		add(mainPanel, BorderLayout.CENTER);
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = routingStrategies.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
		if (routingProbEditor != null) {
			routingProbEditor.stopEditing();
		}
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		classEditor.clearCache();
		// Select first routing strategy
		if (routingStrategies != null && routingStrategies.getRowCount() > 0) {
			routingStrategies.setRowSelectionInterval(0, 0);
		}
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Routing Section";
	}

	protected class RoutingSelectionTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public RoutingSelectionTable() {
			super();
			setModel(new RoutingSelectionTableModel());
			sizeColumns();
			setRowHeight(18);
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return ComboBoxCellEditor.getEditorInstance(RoutingStrategy.findAll());
			} else {
				return super.getCellEditor(row, column);
			}
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 0) {
				return classEditor.getRenderer();
			} else if (column == 1) {
				return ComboBoxCellEditor.getRendererInstance();
			} else {
				return super.getCellRenderer(row, column);
			}
		}

		private void sizeColumns() {
			int[] columnSizes = ((RoutingSelectionTableModel) getModel()).columnSizes;
			for (int i = 0; i < columnSizes.length; i++) {
				getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
			}
		}

		@Override
		public void valueChanged(ListSelectionEvent e) {
			super.valueChanged(e);
			int row = getSelectedRow();
			if (row != -1) {
				if (routingProbEditor != null) {
					routingProbEditor.stopEditing();
					routingProbEditor.setData(stationData, stationKey, classData.getClassKeys().get(row));
					RoutingSectionPanel.this.doLayout();
					RoutingSectionPanel.this.repaint();
				}
			}
		}

	}

	protected class RoutingSelectionTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] columnNames = new String[] { "Class", "Routing Strategy" };
		public int[] columnSizes = new int[] { 70, 100 };
		private Class[] columnClasses = new Class[] { String.class, Object.class };

		public int getRowCount() {
			return classData.getClassKeys().size();
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
			return columnIndex == 1;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = indexToKey(rowIndex);
			if (columnIndex == 0) {
				return key;
			} else if (columnIndex == 1) {
				return stationData.getRoutingStrategy(stationKey, key);
			} else {
				return null;
			}
		}

		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			if (columnIndex == 1) {
				Object classKey = indexToKey(rowIndex);
				Object value = aValue;
				if (aValue instanceof ProbabilityRouting) {
					value = ((ProbabilityRouting) aValue).clone();
				}
				if (!value.equals(stationData.getRoutingStrategy(stationKey, classKey))) {
					stationData.setRoutingStrategy(stationKey, classKey, value);
				}
				routingProbEditor.setData(stationData, stationKey, classKey);
				doLayout();
				repaint();
				RoutingSectionPanel.this.doLayout();
			}
		}

		private Object indexToKey(int index) {
			return classData.getClassKeys().get(index);
		}
	}
}
