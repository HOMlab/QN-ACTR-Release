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

package jmt.gui.common.editors;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.SwingConstants;

import jmt.gui.exact.table.DisabledCellRenderer;

/**
 * <p>Title: Gray Cell Renderer</p>
 * <p>Description: A Cell renderer that displays uneditable cells as not enable (grayed) and
 * allign number and infinity symbol right and centers strings. Null values are painted with
 * background color.</p>
 * 
 * @author Bertoli Marco
 *         Date: 14-ott-2005
 *         Time: 19.16.29
 */
public class GrayCellRenderer extends DisabledCellRenderer {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col) {
		Component renderer = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);

		// Paints infinity symbol as a number
		if (value instanceof String && "\u221E".equals(value)) {
			super.setHorizontalAlignment(SwingConstants.RIGHT);
		}
		if (!table.isCellEditable(row, col)) {
			renderer.setEnabled(false);
		} else {
			renderer.setEnabled(true);
		}
		return renderer;
	}

}
