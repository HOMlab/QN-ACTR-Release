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

import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.help.HoverHelp;

/**

 * @author alyf (Andrea Conti)
 * Date: 20-set-2003
 * Time: 13.10.36

 */

/**
 * A "reduced" ExactTable for showing unmodifiable results
 */
public class ResultsTable extends ExactTable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected HoverHelp help;

	public ResultsTable(ExactTableModel dm, HoverHelp help) {
		super(dm);
		autoResizeMode = AUTO_RESIZE_OFF;
		this.help = help;

		setDisplaysScrollLabels(true);

		tableHeader.setToolTipText(null);
		rowHeader.setToolTipText(null);

		if (help != null) {
			help.addHelp(this, "Click or drag to select cells; Right-click for a list of available operations");
			help.addHelp(moreColumnsLabel, "There are more classes: scroll right to see them");
			help.addHelp(moreRowsLabel, "There are more stations: scroll down to see them");
			help.addHelp(selectAllButton, "Click to select all cells");
			help.addHelp(tableHeader, "Click, SHIFT-click or drag to select columns");
			help.addHelp(rowHeader, "Click, SHIFT-click or drag to select rows");
		}

		setRowSelectionAllowed(true);
		setColumnSelectionAllowed(true);
		setClipboardTransferEnabled(true);

		setDefaultRenderer(Object.class, new DisabledCellRenderer());
	}

	//NEW Federico Dall'Orso
	@Override
	public TableCellRenderer getCellRenderer(int row, int column) {
		final TableCellRenderer cellRenderer = super.getCellRenderer(row, column);
		return new TableCellRenderer() {
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component label = cellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				Color col = Color.WHITE;
				if (row == 0 || column == 0) {
					col = new Color(255, 255, 210);
				}
				if (!isSelected && !hasFocus) {
					label.setBackground(col);
				}
				return label;
			}
		};
	}

	//END

	@Override
	protected void installKeyboard() {
		InputMap im = getInputMap();
		ActionMap am = getActionMap();
		installKeyboardAction(im, am, COPY_ACTION);
	}

	@Override
	protected JPopupMenu makeMouseMenu() {
		JPopupMenu menu = new JPopupMenu();
		menu.add(COPY_ACTION);
		return menu;
	}

	/**
	 * @return false
	 */
	@Override
	public boolean isCellEditable(int row, int column) {
		return false;
	}

}
