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

package jmt.gui.exact.table;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

/**

 * @author alyf (Andrea Conti)
 * Date: 12-set-2003
 * Time: 22.44.09

 */

/**
 * A single-column JTable impersonating a table's row headers and listening to clicks for column selection
 */
public class RowHeader extends JTable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private int width = 80; //default header width

	private JTable table;
	private MouseRowSelector mrs;
	private TableCellRenderer cellRenderer;
	protected boolean listenerInstalled;

	/**
	 * make a RowHeader with default model (displaying row numbers)
	 */
	public RowHeader() {
		this(new DefaultRowHeaderModel());
	}

	/**
	 * make a RowHeader with the specified model.<br>
	 * If the supplied model does not implement <code>ModelHandlesRowHeader</code> the RowHeader will ignore it and use its default model showing row numbers.<br>
	 * If the model implements <code>PrototypedTableModel</code> The RowHeader will try to automatically determine its width, otherwise a default width will be used.
	 * @see jmt.gui.exact.table.ModelHandlesRowHeader
	 * @see jmt.gui.exact.table.PrototypedTableModel
	 */
	public RowHeader(TableModel dm) {

		super(new DefaultRowHeaderModel());
		if (dm instanceof ModelHandlesRowHeader) {
			setModel(dm);
		}

		cellRenderer = tableHeader.getDefaultRenderer();
		setFocusable(false);
		setToolTipText("Click or drag to select rows");
		mrs = new MouseRowSelector();

		installListener();

		/* try to automatically determine width */
		if (dataModel instanceof PrototypedTableModel) {
			PrototypedTableModel pm = (PrototypedTableModel) dataModel;
			width = cellRenderer.getTableCellRendererComponent(this, pm.getPrototype(-1), false, false, 0, 0).getPreferredSize().width;
		}
	}

	@Override
	public void createDefaultColumnsFromModel() {
		TableModel m = getModel();
		if (m != null) {
			// Remove any current columns
			TableColumnModel cm = getColumnModel();
			while (cm.getColumnCount() > 0) {
				cm.removeColumn(cm.getColumn(0));
			}

			// Create one column only
			TableColumn newColumn = new TableColumn(0);
			addColumn(newColumn);
		}
	}

	@Override
	public TableCellRenderer getCellRenderer(int row, int column) {
		return cellRenderer;
	}

	public void update() {
		/* mimic a "table changed" event from the model */
		tableChanged(new TableModelEvent(dataModel));
	}

	public void updateRows(int firstRow, int lastRow) {
		tableChanged(new TableModelEvent(dataModel, firstRow, lastRow));
	}

	public void install(JTable table, JScrollPane jsp) {
		if (this.table != null) {
			throw new RuntimeException("already installed");
		}

		JViewport jv = new JViewport() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			/* keeps the #@£$*& thing from growing */
			@Override
			public Dimension getPreferredSize() {
				Dimension d = super.getPreferredSize();
				d.width = width;
				return d;
			}
		};
		jv.setView(this);
		jsp.setRowHeader(jv);

		this.table = table;
		if (dataModel instanceof DefaultRowHeaderModel) {
			((DefaultRowHeaderModel) dataModel).setTable(table);
		}
		update();
	}

	public void uninstall() {

		if (dataModel instanceof DefaultRowHeaderModel) {
			((DefaultRowHeaderModel) dataModel).setTable(null);
		}
		removeMouseMotionListener(mrs);
		removeMouseListener(mrs);

		table = null;
		//update();
	}

	public void setAllowsClickRowSelection(boolean b) {
		if (b) {
			installListener();
		} else {
			uninstallListener();
		}
	}

	private void installListener() {
		if (!listenerInstalled) {
			addMouseListener(mrs);
			addMouseMotionListener(mrs);
			setToolTipText("Click or drag to select rows");
			listenerInstalled = true;
		}
	}

	private void uninstallListener() {
		if (listenerInstalled) {
			removeMouseMotionListener(mrs);
			removeMouseListener(mrs);
			setToolTipText("");
			listenerInstalled = false;
		}
	}

	private void stopEditing(JTable table) {
		if (table.isEditing()) {
			if (!table.getCellEditor().stopCellEditing()) {
				table.getCellEditor().cancelCellEditing();
			}
		}
	}

	public void setWidth(int width) {
		this.width = width;
		resizeAndRepaint();
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public Component prepareRenderer(TableCellRenderer renderer, int row, int column) {
		Object value = dataModel.getValueAt(row, -1);
		return renderer.getTableCellRendererComponent(this, value, false, false, row, column);
	}

	@Override
	public boolean isCellEditable(int row, int col) {
		return false;
	}

	/**
	 * a default data model displaying row numbers
	 */
	public static class DefaultRowHeaderModel extends AbstractTableModel implements ModelHandlesRowHeader, PrototypedTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected static final Object prototype = new Integer(999);

		protected JTable table;

		protected void setTable(JTable table) {
			this.table = table;
		}

		public int getRowCount() {
			if (table != null) {
				return table.getRowCount();
			}
			return 0;
		}

		@Override
		public String getColumnName(int column) {
			return " ";
		}

		public int getColumnCount() {
			return 1;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			return new Integer(rowIndex + 1);
		}

		public Object getPrototype(int i) {
			return prototype;
		}

	}

	private class MouseRowSelector extends MouseAdapter implements MouseMotionListener {
		private int row0 = -1;

		@Override
		public void mousePressed(MouseEvent e) {
			stopEditing(table);
			row0 = rowAtPoint(e.getPoint());
			int row0b = row0;
			if (row0 >= 0) {
				if (e.isShiftDown()) {
					row0b = table.getSelectionModel().getAnchorSelectionIndex();
				}
				table.setRowSelectionInterval(row0b, row0);
				table.setColumnSelectionInterval(0, table.getColumnCount() - 1);
				//selectionModel.setAnchorSelectionIndex(0);
				//columnModel.getSelectionModel().setAnchorSelectionIndex(col0);
				table.requestFocus();
			}
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			row0 = -1;
		}

		public void mouseDragged(MouseEvent e) {
			if (e.isShiftDown()) {
				return; //ignore drag when shift is pressed
			}
			int row1 = rowAtPoint(e.getPoint());
			if (row0 >= 0 && row1 >= 0) {
				table.setRowSelectionInterval(row0, row1);
				table.setColumnSelectionInterval(0, table.getColumnCount() - 1);
			}
		}

		public void mouseMoved(MouseEvent e) {
		}
	}

}
