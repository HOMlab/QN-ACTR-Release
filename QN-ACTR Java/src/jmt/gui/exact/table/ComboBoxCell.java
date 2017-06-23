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

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 8-mar-2005
 * Time: 8.41.48
 * This class allows multiple-choice table cells to be rendered as combobox instead of
 * simple text fields. This gives users a more understandable view of a table GUI.
 */
public class ComboBoxCell extends DefaultCellEditor implements TableCellRenderer, TableCellEditor, ActionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JComboBox jcb;

	public ComboBoxCell(Object[] valueSet) {
		super(new JComboBox(valueSet));
		jcb = (JComboBox) getComponent();
		jcb.setBackground(Color.WHITE);
		jcb.addActionListener(this);
	}

	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
		jcb.setSelectedItem(value);
		if (isSelected && !hasFocus) {
			jcb.setBackground(table.getSelectionBackground());
			jcb.setForeground(table.getSelectionForeground());
		} else {
			jcb.setBackground(table.getBackground());
			jcb.setForeground(table.getForeground());
		}
		return jcb;
	}

	/**Returns combo box contained into this renderer. This provides further customization
	 * capabilities.
	 * @return :ComboBox to be rendered.
	 * */
	public JComboBox getComboBox() {
		return (JComboBox) super.getComponent();
	}

	@Override
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		return jcb;
	}

	public void actionPerformed(ActionEvent e) {
		fireEditingStopped();
	}

}
