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

import javax.swing.table.AbstractTableModel;

/**

 * @author alyf (Andrea Conti)
 * Date: 16-set-2003
 * Time: 13.45.59

 */

/**
 * An utility abstract class implementing a framework for RowHeader-aware TableModels with identical column prototypes. Subclassers must
 * fill in <code>prototype</code> and <code>rowHeaderPrototype</code> and implement <code>getRowName()</code> and <code>getValueAtImpl()</code>.
 * The latter two need <b>not</b> be row-header aware. For tables with different column types you must override <code>getPrototype</code> to get a correct behavior
 */
public abstract class ExactTableModel extends AbstractTableModel implements ModelHandlesRowHeader, PrototypedTableModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Object prototype;
	protected Object rowHeaderPrototype;

	public final Object getValueAt(int rowIndex, int columnIndex) {
		switch (columnIndex) {
			case -1:
				return getRowName(rowIndex);
			default:
				return getValueAtImpl(rowIndex, columnIndex);
		}
	}

	public Object getPrototype(int columnIndex) {
		switch (columnIndex) {
			case -1:
				return rowHeaderPrototype;
			default:
				return prototype;
		}
	}

	/**
	 * @return the object at (rowIndex, columnIndex)
	 */
	protected abstract Object getValueAtImpl(int rowIndex, int columnIndex);

	/**
	 * @return the header for row <code>rowIndex</code>
	 */
	protected abstract Object getRowName(int rowIndex);

	public void clear(int row, int col) {
		setValueAt(null, row, col);
	}

	/**
	 * Copies the valu cell to all selected cells. Default implementation does absolutely nothing.
	 */
	public void copyCellToArea(int sourceRow, int sourceCol, int rowFrom, int rowTo, int colFrom, int colTo) {
	}

}
