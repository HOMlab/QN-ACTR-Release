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
import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;

import javax.swing.Box;
import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.panels.WarningScrollTable;
import jmt.gui.exact.table.DisabledCellRenderer;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 21-giu-2005
 * Time: 16.44.47
 * To change this template use Options | File Templates.
 */
public class ConnectionsPanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private StationDefinition data;
	private ConnectionTable connTab;

	public ConnectionsPanel(StationDefinition sd) {
		data = sd;
		initComponents();
	}

	/**
	 * Set up the panel contents and layout
	 */
	private void initComponents() {

		connTab = new ConnectionTable();

		//create margins for this panel.
		Box vBox = Box.createVerticalBox();
		Box hBox = Box.createHorizontalBox();
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(hBox);
		vBox.add(Box.createVerticalStrut(20));
		hBox.add(Box.createHorizontalStrut(20));

		//build central panel
		JPanel componentsPanel = new JPanel(new BorderLayout(0, 5));
		//new BoxLayout(componentsPanel, BoxLayout.Y_AXIS);

		//build upper part of central panel
		JLabel descrLabel = new JLabel(CONNECTIONS_DESCRIPTION);

		//add all panels to the mail panel
		componentsPanel.add(descrLabel, BorderLayout.NORTH);
		componentsPanel.add(new WarningScrollTable(connTab, WARNING_STATIONS), BorderLayout.CENTER);
		hBox.add(componentsPanel);
		hBox.add(Box.createHorizontalStrut(20));
		this.setLayout(new GridLayout(1, 1));
		this.add(vBox);
	}

	@Override
	public String getName() {
		return "Connections";
	}

	@Override
	public void repaint() {
		if (connTab != null) {
			connTab.updateStructure();
		}
		super.repaint();
	}

	public void setData(StationDefinition sd) {
		this.data = sd;
		repaint();
	}

	private class ConnectionTable extends ExactTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		ConnectionTable() {
			super(new ConnTableModel());
			autoResizeMode = AUTO_RESIZE_OFF;

			createRenderers();
			createEditors();
			setDisplaysScrollLabels(true);

			setRowSelectionAllowed(false);
			setColumnSelectionAllowed(false);
			setClipboardTransferEnabled(false);

		}

		protected void createRenderers() {
			final TableCellRenderer boolRend = getDefaultRenderer(Boolean.class);
			setDefaultRenderer(Boolean.class, new DisabledCellRenderer() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				@Override
				public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
					Component cbox = boolRend.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
					cbox.setEnabled(true);
					if (value == null) {
						Component disabled = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
						cbox.setBackground(disabled.getBackground());
						cbox.setEnabled(false);
					}
					return cbox;
				}
			});
		}

		protected void createEditors() {
			setDefaultEditor(Boolean.class, new DefaultCellEditor(new JCheckBox()) {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				@Override
				public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
					Component c = super.getTableCellEditorComponent(table, value, isSelected, row, column);
					if (value instanceof Boolean) {
						c.setBackground(Color.WHITE);
						c.setVisible(false);
					}
					return c;
				}
			});
		}
	}

	/**
	 * the model backing the service times table.
	 * Rows represent source stations, columns target stations.
	 */
	private class ConnTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		ConnTableModel() {
			prototype = "Station10000";
			rowHeaderPrototype = "Station10000";
		}

		public int getRowCount() {
			return data.getStationKeys().size();
		}

		public int getColumnCount() {
			return data.getStationKeys().size();
		}

		@Override
		public Class getColumnClass(int columnIndex) {
			if (columnIndex >= 0) {
				return Boolean.class;
			} else {
				return super.getColumnClass(columnIndex);
			}
		}

		@Override
		public String getColumnName(int index) {
			if (index >= 0 && data.getStationKeys().size() > 0) {
				return data.getStationName(getStationKey(index));
			} else {
				return "";
			}
		}

		@Override
		public Object getPrototype(int i) {
			if (i == -1) {
				return rowHeaderPrototype;
			} else {
				return prototype;
			}
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			Object row = getStationKey(rowIndex), col = getStationKey(columnIndex);
			boolean areConnected = data.areConnected(row, col);
			boolean areConnectable = data.areConnectable(row, col);
			if (!areConnectable) {
				return null;
			}
			return new Boolean(areConnected);
		}

		@Override
		protected Object getRowName(int rowIndex) {
			return data.getStationName(getStationKey(rowIndex));
		}

		//returns search key of a station given its index in table
		private Object getStationKey(int index) {
			return data.getStationKeys().get(index);
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			if (value instanceof Boolean) {
				boolean b = ((Boolean) value).booleanValue();
				data.setConnected(getStationKey(rowIndex), getStationKey(columnIndex), b);
			}
		}

		@Override
		public void clear(int row, int col) {
			data.setConnected(getStationKey(row), getStationKey(col), false);
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			Object row = getStationKey(rowIndex), col = getStationKey(columnIndex);
			boolean areConnectable = data.areConnectable(row, col);
			if (!areConnectable) {
				return false;
			} else {
				return true;
			}
		}

	}

}
