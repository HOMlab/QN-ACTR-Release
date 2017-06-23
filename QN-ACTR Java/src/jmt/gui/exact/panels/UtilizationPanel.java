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

package jmt.gui.exact.panels;

import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactWizard;
import jmt.gui.exact.table.ExactTableModel;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 */

/**
 * 8th panel: utilization
 */
public final class UtilizationPanel extends SolutionPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private double[][][] util;
	//NEW Federico Dall'Orso
	private double[][] stationAggs, classAggs;
	private double[] globalAgg;

	//END

	public UtilizationPanel(ExactWizard ew) {
		super(ew);
		helpText = "<html>Utilization</html>";
		name = "Utilization";
	}

	/**
	 * gets status from data object
	 */
	@Override
	protected void sync() {
		super.sync();
		util = data.getUtilization();
		//NEW Federico Dall'Orso
		stationAggs = data.getPerStationU();
		classAggs = data.getPerClassU();
		globalAgg = data.getGlobalU();
		//END
	}

	@Override
	protected ExactTableModel getTableModel() {
		return new UTableModel();
	}

	@Override
	protected String getDescriptionMessage() {
		return ExactConstants.DESCRIPTION_UTILIZATIONS;
	}

	/**
	 * the model backing the visit table.
	 * Rows represent stations, columns classes.
	 */
	private class UTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		UTableModel() {
			prototype = new Double(1000);
			rowHeaderPrototype = "Station1000";
		}

		public int getRowCount() {
			if (util == null) {
				return 0;
			}
			//OLD
			/*
			return stations;
			*/
			//NEW Federico Dall'Orso
			return stations + 1;
			//END
		}

		public int getColumnCount() {
			if (util == null) {
				return 0;
			}
			//OLD
			/*
			if (isSingle) return 1;
			return classes;
			*/
			//NEW
			//@author Dall'Orso
			return classes + 1;
			//end NEW
		}

		@Override
		protected Object getRowName(int rowIndex) {
			if (rowIndex == 0) {
				return "<html>-</html>";
			}
			return stationNames[rowIndex - 1];
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
			double d = 0;
			//OLD
			/*
			d = util[rowIndex][columnIndex];
			*/
			//NEW Dall'Orso
			if (columnIndex > 0 && rowIndex > 0) {
				d = util[rowIndex - 1][columnIndex - 1][iteration];
			} else if (columnIndex == 0 && rowIndex > 0) {
				d = stationAggs[rowIndex - 1][iteration];
			} else if (columnIndex > 0 && rowIndex == 0) {
				d = classAggs[columnIndex - 1][iteration];
			} else {
				d = globalAgg[iteration];
			}
			//END
			if (d < 0) {
				return "-"; //causes the renderer to display a gray cell
			}
			return new Double(d);
		}

	}
}
