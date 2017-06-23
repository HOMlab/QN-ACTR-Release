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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumnModel;

/**

 * @author alyf (Andrea Conti)
 * Date: 15-set-2003
 * Time: 19.04.55

 */

/**
 * a JTableHeader that can handle column selection
 */
public class ClickableTableHeader extends JTableHeader {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected boolean listenerInstalled;

	protected MouseColumnSelector mcs;

	public ClickableTableHeader() {
		this(null);
	}

	public ClickableTableHeader(TableColumnModel cm) {
		super(cm);
		reorderingAllowed = false;

		//Dall'Orso 21/12/2004
		//BEGIN
		resizingAllowed = true;
		//END

		mcs = new MouseColumnSelector();

		installListener();
	}

	public void setAllowsClickColumnSelection(boolean b) {
		if (b) {
			installListener();
		} else {
			uninstallListener();
		}
	}

	private void installListener() {
		if (!listenerInstalled) {
			addMouseListener(mcs);
			addMouseMotionListener(mcs);
			setToolTipText("Click or drag to select columns");
			listenerInstalled = true;
		}
	}

	private void uninstallListener() {
		if (listenerInstalled) {
			removeMouseMotionListener(mcs);
			removeMouseListener(mcs);
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

	private class MouseColumnSelector extends MouseAdapter implements MouseMotionListener {
		private int col0 = -1;

		@Override
		public void mousePressed(MouseEvent e) {
			stopEditing(table);
			col0 = columnAtPoint(e.getPoint());
			int col0b = col0;
			if (col0 >= 0) {
				if (e.isShiftDown()) {
					col0b = columnModel.getSelectionModel().getAnchorSelectionIndex();
				}
				table.setColumnSelectionInterval(col0b, col0);
				table.setRowSelectionInterval(0, table.getRowCount() - 1);
				//selectionModel.setAnchorSelectionIndex(0);
				//columnModel.getSelectionModel().setAnchorSelectionIndex(col0);
				table.requestFocus();
			}
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			col0 = -1;
		}

		public void mouseDragged(MouseEvent e) {
			if (e.isShiftDown()) {
				return; //ignore drag when shift is pressed
			}
			int col1 = columnAtPoint(e.getPoint());
			if (col0 >= 0 && col1 >= 0) {
				table.setColumnSelectionInterval(col0, col1);
				table.setRowSelectionInterval(0, table.getRowCount() - 1);
			}
		}

		public void mouseMoved(MouseEvent e) {
		}
	}

}
