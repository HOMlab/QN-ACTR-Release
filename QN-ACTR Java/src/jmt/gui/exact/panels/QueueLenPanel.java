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
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of QueueLength to Customer Number
 * @version Date: Aug-2009
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of Customer Number to Number of Customers
 * @version Date: Sep-2009
 * 
 * @author Ashanka
 * Cleaned code by removing unnecesary comments of old code.
 * @version Date: Sep-2009
 */

/**
 * 5th panel: queue length
 */
public final class QueueLenPanel extends SolutionPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private double[][][] queueLen;
	//NEW Dall'Orso
	double[][] classAggs, stationAggs;
	double[] globalAgg;

	//END

	public QueueLenPanel(ExactWizard ew) {
		super(ew);
		helpText = "<html>Number of Customers</html>";
		name = "Number of Customers";
	}

	/**
	 * gets status from data object
	 */
	@Override
	protected void sync() {
		super.sync();
		queueLen = data.getQueueLen();
		//NEW Federico Dall'Orso
		classAggs = data.getPerClassQ();
		stationAggs = data.getPerStationQ();
		globalAgg = data.getGlobalQ();
		//END
	}

	@Override
	protected ExactTableModel getTableModel() {
		return new QLTableModel();
	}

	@Override
	protected String getDescriptionMessage() {
		return ExactConstants.DESCRIPTION_QUEUELENGTHS;
	}

	/**
	 * the model backing the visit table.
	 * Rows represent stations, columns classes.
	 */
	private class QLTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		QLTableModel() {
			prototype = new Double(1000);
			rowHeaderPrototype = "Station1000";
		}

		public int getRowCount() {
			if (queueLen == null) {
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
			if (queueLen == null) {
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
				return "<html><i>Aggregate</i></html>";
			} else {
				return stationNames[rowIndex - 1];
			}
		}

		@Override
		public String getColumnName(int index) {
			if (index == 0) {
				return "<html><i>Aggregate</i></html>";
			} else {
				return classNames[index - 1];
			}
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			double d = 0;
			//OLD
			/*
			d = queueLen[rowIndex][columnIndex];
			*/
			//NEW Federico Dall'Orso
			if (rowIndex > 0 && columnIndex > 0) {
				d = queueLen[rowIndex - 1][columnIndex - 1][iteration];
			} else if (rowIndex == 0 && columnIndex > 0) {
				d = classAggs[columnIndex - 1][iteration];
			} else if (rowIndex > 0 && columnIndex == 0) {
				d = stationAggs[rowIndex - 1][iteration];
			} else {
				d = globalAgg[iteration];
			}
			//END
			if (d < 0) {
				return null; //causes the renderer to display a gray cell
			}
			return new Double(d);
		}

	}
}
