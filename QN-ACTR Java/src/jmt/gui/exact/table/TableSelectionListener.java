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

import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;

/**

 * @author alyf (Andrea Conti)
 * Date: 14-set-2003
 * Time: 18.53.05

 */

/**
 * an object that intercepts all selection events for a table
 */
public abstract class TableSelectionListener implements ListSelectionListener {

	private ListSelectionModel rowSelectionModel, colSelectionModel;

	public static final int TYPE_ROW = 1;
	public static final int TYPE_COLUMN = 2;

	protected JTable table;

	/**
	 * row selection events
	 */
	public final void valueChanged(ListSelectionEvent e) {
		Object source = e.getSource();
		if (source == rowSelectionModel) {
			selectionChanged(table, e, TYPE_ROW);
		}
		if (source == colSelectionModel) {
			selectionChanged(table, e, TYPE_COLUMN);
		}
	}

	/**
	 * called whenever the table selection changes
	 */
	protected abstract void selectionChanged(JTable table, ListSelectionEvent e, int type);

	/**
	 * Add this object to a JTable
	 */
	public void install(JTable table) {
		this.table = table;
		rowSelectionModel = table.getSelectionModel();
		rowSelectionModel.addListSelectionListener(this);

		colSelectionModel = table.getColumnModel().getSelectionModel();
		colSelectionModel.addListSelectionListener(this);
	}

	public void uninstall() {
		table = null;
		rowSelectionModel.removeListSelectionListener(this);
		colSelectionModel.removeListSelectionListener(this);
	}

	/* these are not used */
	public void columnAdded(TableColumnModelEvent e) {
	}

	public void columnMarginChanged(ChangeEvent e) {
	}

	public void columnMoved(TableColumnModelEvent e) {
	}

	public void columnRemoved(TableColumnModelEvent e) {
	}
}
