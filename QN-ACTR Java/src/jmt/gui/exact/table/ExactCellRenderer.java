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
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;

import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.text.NumberFormatter;

/**
 * A CellRenderer displaying numbers and string with correct alignment and format
 */
public class ExactCellRenderer extends DefaultTableCellRenderer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static DecimalFormatSymbols dfs = new DecimalFormatSymbols();

	static {
		dfs.setDecimalSeparator('.');
	}

	private DecimalFormat df = new DecimalFormat("0.000000;-0.000000", dfs);
	private NumberFormatter nf = new NumberFormatter(df);

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col) {

		//Federico Dall'Orso 22/6/2005
		//OLD
		/* called side effects
		super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);

		/* adjust alignment */
		/*
		if (value instanceof Number) {
			setHorizontalAlignment(SwingConstants.RIGHT);
		} else {
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		return this;
		*/
		//NEW
		if (value instanceof Number) {
			super.setHorizontalAlignment(SwingConstants.RIGHT);
		} else if (value instanceof Boolean) {
			return super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
		} else {
			super.setHorizontalAlignment(SwingConstants.CENTER);
		}
		Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);

		return c;
		//END Federico Dall'Orso
	}

	@Override
	public void setValue(Object o) {
		if (o instanceof Double) {
			try {
				setText(nf.valueToString(o));
			} catch (ParseException e) {
				throw new RuntimeException(e);
			}
		} else {
			super.setValue(o);
		}
	}

}
