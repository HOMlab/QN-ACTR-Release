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

package jmt.framework.gui.table.editors;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 24-mag-2005
 * Time: 15.40.00
 * This cell editor displays a button. It is possible to insert a customized button in
 * a cell to implement various functionalities. As this class implements both cell
 * editor and cell renderer interfaces, it is possible to obtain a proper visualization
 * of this component, simply by overriding <code>getCellRenderer</code> and <code>
 * getCellEditor</code> in <code>JTable class</code>.
 */
public class ButtonCellEditor extends AbstractCellEditor implements TableCellEditor, ActionListener, TableCellRenderer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JButton button;

	/**Creates a new instance of <code>ButtonCellEditor</code> given a customized <code>JButton
	 * </code> to be inserted in table.
	 * @param jbutt: button to be displayed in specific cell*/
	public ButtonCellEditor(JButton jbutt) {
		button = jbutt;
		jbutt.addActionListener(this);
	}

	/**Returns a value to be updated in the table model. This method must be overridden in
	 * implementing classes as it always return <code>null</code> value.
	 * @return : value to be updated inside the table model*/
	public Object getCellEditorValue() {
		return null;
	}

	/**This method is called whenever an action event is invoked by this editor's component*/
	public void actionPerformed(ActionEvent e) {
		fireEditingStopped();
	}

	/**Returns component to edit table cell value. Refer to
	 * {@link javax.swing.table.TableCellEditor} for further informations about this method
	 * */
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		return button;
	}

	/**Returns component to be rendered in the cell table.  Refer to
	 * {@link javax.swing.table.TableCellEditor} for further informations about this method
	 * */
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
		return button;
	}

}
