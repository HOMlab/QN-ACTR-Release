/**    
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.gui.exact.panels;

import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactWizard;
import jmt.gui.exact.table.ExactTableModel;

public class SysPowerPanel extends SolutionPanel {

	private static final long serialVersionUID = 1L;

	private double[][] classAggs;
	private double[] globalAgg;

	public SysPowerPanel(ExactWizard ew) {
		super(ew);
		// TODO Auto-generated constructor stub
		helpText = "<html>System Power</html>";
		name = "System Power";
	}

	@Override
	protected void sync() {
		super.sync();
		classAggs = data.getPerClassSP();
		globalAgg = data.getGlobalSP();
	}

	@Override
	protected String getDescriptionMessage() {
		return ExactConstants.DESCRIPTION_SYSPOWER;
	}

	@Override
	protected ExactTableModel getTableModel() {
		// TODO Auto-generated method stub
		return new SPTableModel();
	}

	private class SPTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		SPTableModel() {
			prototype = new Double(1000);
			rowHeaderPrototype = "Station1000";
		}

		public int getRowCount() {
			//return stations+1;
			return 1;
			//end NEW

		}

		public int getColumnCount() {
			return classes + 1;
			//end NEW
		}

		@Override
		protected Object getRowName(int rowIndex) {
			if (rowIndex == 0) {
				return "<html><i>Aggregate</i></html>";
			}
			return "-";
		}

		@Override
		public String getColumnName(int index) {
			if (index == 0) {
				return "<html><i>Aggregate</i></html>";
			}
			return classNames[index - 1];
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			double d = 99999;//initialized with dummy
			Object ret;
			//NEW Dall'Orso
			if (rowIndex > 0 && columnIndex > 0) {
				ret = new String("-");
			} else if (rowIndex == 0 && columnIndex > 0) {
				d = classAggs[columnIndex - 1][iteration];
				ret = new Double(d);
			} else if (rowIndex > 0 && columnIndex == 0) {
				ret = new String("-");
			} else {
				d = globalAgg[iteration];
				ret = new Double(d);
			}
			//END
			if (d < 0) {
				return null; //causes the renderer to display a gray cell
			}

			return ret;
		}

	}

}
