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

import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;

/**

 * @author alyf (Andrea Conti)
 * Date: 12-set-2003
 * Time: 22.01.55

 */

/**
 * a slightly more user-friendly JTextField-based DefaultCellEditor.
 * Note that you should *not* double-click cells to edit them. Just select the cells (with one
 * click) and start typing.
 */
public class ExactCellEditor extends DefaultCellEditor {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public ExactCellEditor() {
		super(new JTextField());
	}

	/**
	 * calls JTextField.selectAll() before returning it
	 */
	@Override
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		JTextField c = (JTextField) super.getTableCellEditorComponent(table, value, isSelected, row, column);
		c.selectAll();
		return c;
	}

}