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

package jmt.gui.exact.ld;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.AbstractCellEditor;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.table.TableCellEditor;

/**

 * @author alyf (Andrea Conti)
 * Date: 13-set-2003
 * Time: 10.34.29

 */

/**
 * An editor for LD service times. Uses a JDialog to perform editing.
 */
public class LDEditor extends AbstractCellEditor implements TableCellEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private Frame owner;

	private int clicksToEdit = 2;
	LDEditingWindow ldWindow;

	public LDEditor(Frame owner) {
		this.owner = owner;
		ldWindow = new LDEditingWindow(owner, this);
	}

	/**
	 * stops cell editing. calls fireEditingStopped()
	 */
	@Override
	public boolean stopCellEditing() {
		return false;
	}

	/**
	 * cancels cell editing. calls fireEditingCanceled()
	 */
	@Override
	public void cancelCellEditing() {
		return;
	}

	/**
	 * @return the edited matrix of service times
	 */
	public Object getCellEditorValue() {
		return ldWindow.getServiceTimes();
	}

	/**
	 *  sets the data for the editor window
	 */
	public void setStatus(String title, String stationName, String[] classNames, double[][] serviceTimes) {
		ldWindow.setStatus(title, stationName, classNames, serviceTimes);
	}

	/**
	 * should never be called
	 */
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {

		throw new RuntimeException("LDEditor cannot be used as a normal editor!");
	}

	/**
	 * Initializes the editing window and sets it up at the correct location;
	 */
	public void startEditing(JTable table, int row) {
		showWindow(table, row);
	}

	private void showWindow(JTable table, int row) {

		/* determine correct location */
		//TODO: throw away half of this rubbish
		Point offset = new Point();
		Point tablebase = table.getLocationOnScreen();
		Point framebase = owner.getLocationOnScreen();

		Rectangle cellRect = table.getCellRect(row, 0, true);
		int middle = table.getHeight() / 2;

		int deltax, deltay, width;

		deltay = tablebase.y - framebase.y;

		Component c = table.getParent();
		if (c instanceof JViewport) { //we are in a JScrollPane
			c = c.getParent();
			deltax = c.getLocationOnScreen().x - framebase.x;
			width = c.getWidth();
		} else {
			deltax = tablebase.x - framebase.x;
			width = table.getWidth();
		}

		offset.x = cellRect.x + deltax;

		if (cellRect.y > middle) {
			offset.y = cellRect.y - ldWindow.getHeight() + deltay;
		} else {
			offset.y = cellRect.y + cellRect.height + deltay;
		}
		//ldWindow.setBase(framebase);
		//ldWindow.setOffset(offset);

		ldWindow.centerWindow(width, ldWindow.getHeight());
		ldWindow.show();
	}

	/**
	 * borrowed from <code>DefaultCellEditor</code>
	 */
	@Override
	public boolean isCellEditable(EventObject e) {
		if (e instanceof MouseEvent) {
			return ((MouseEvent) e).getClickCount() >= clicksToEdit;
		}
		return true;
	}

	/**
	 * @see javax.swing.AbstractCellEditor#fireEditingCanceled()
	 */
	public void editingCanceled() {
		fireEditingCanceled();
	}

	/**
	 * @see javax.swing.AbstractCellEditor#fireEditingStopped()
	 */
	public void editingStopped() {
		fireEditingStopped();
	}

	public int getClicksToEdit() {
		return clicksToEdit;
	}

	public void setClicksToEdit(int clicksToEdit) {
		this.clicksToEdit = clicksToEdit;
	}

}
