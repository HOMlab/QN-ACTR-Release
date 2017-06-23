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

import javax.swing.JTable;
import javax.swing.UIManager;

/**

 * @author alyf (Andrea Conti)
 * Date: 16-set-2003
 * Time: 14.20.09

 */

/**
 * A CellRenderer displaying null cells as disabled
 */
public class DisabledCellRenderer extends ExactCellRenderer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final Color disabledFGColor = UIManager.getColor("Panel.background");
	private final Color disabledBGColor = disabledFGColor;

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col) {

		setForeground(null);
		setBackground(null);

		/* called for its side effects */
		super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);

		/* cells containing a null values are drawn as disabled and cannot be focused */
		if (value == null) {
			setForeground(disabledFGColor);
			setBackground(disabledBGColor);
			setBorder(noFocusBorder);
		}
		return this;
	}

}
