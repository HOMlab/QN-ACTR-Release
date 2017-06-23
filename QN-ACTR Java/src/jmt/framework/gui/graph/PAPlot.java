package jmt.framework.gui.graph;

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

import java.util.Vector;

import ptolemy.plot.Plot;

/**
 * <p>Title: PAPlot</p>
 * <p>Description: a simple plot panel, created extanding the Plot class</p>
 *
 * @author Francesco D'Aquino
 *         Date: 10-feb-2006
 *         Time: 16.17.48
 */
public class PAPlot extends Plot {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private final int VALUE = 0;

	int valuesNumber;
	double[] x;
	double[] values;
	double[] lowerBounds;
	double[] upperBounds;
	boolean[] isolatedBoundPointsStatus;
	boolean[] notValidBoundPointsStatus;
	private String xLabel;
	private String yLabel;
	boolean connected;
	boolean[] status;

	public PAPlot(Vector values, Vector<Number> parameterValues, String xLabel, String yLabel) {
		valuesNumber = values.size();
		if (valuesNumber == 1) {
			this.setMarksStyle("dots", VALUE);
		} else {
			this.setMarksStyle("none", VALUE);
		}
		this.xLabel = xLabel;
		this.yLabel = yLabel;
		initialize(values, parameterValues, this.xLabel, this.yLabel);
	}

	/**
	 * Initializes all vectors, checking for not valid bound values
	 * @param values the vector containing the measures obtained by simulation
	 * @param parameterValues the vector containing the values assumed by the varying parameter
	 * @param xLabel the x axis label
	 * @param yLabel the y axis label
	 */
	public void initialize(Vector values, Vector<Number> parameterValues, String xLabel, String yLabel) {
		notValidBoundPointsStatus = new boolean[valuesNumber];
		isolatedBoundPointsStatus = new boolean[valuesNumber];
		this.values = new double[valuesNumber];
		lowerBounds = new double[valuesNumber];
		upperBounds = new double[valuesNumber];
		//initialize the arrays
		x = new double[valuesNumber];
		for (int i = 0; i < valuesNumber; i++) {
			MeasureValue thisValue = (MeasureValue) values.get(i);
			Object temp = parameterValues.get(i);
			if (temp instanceof Long) { //seed parametric analysis: do not use the
				this.x[i] = i; //values of parameter, only enumerate
			} else if (temp instanceof Double) {
				Double x = (Double) (parameterValues.get(i));
				this.x[i] = x.doubleValue();
			}
			this.values[i] = thisValue.getMeanValue();
			lowerBounds[i] = thisValue.getLowerBound();
			upperBounds[i] = thisValue.getUpperBound();
		}
		//find the indexes of not valid bounds
		for (int i = 0; i < valuesNumber; i++) {
			if (!((upperBounds[i] == 0) && (this.values[i] != 0))) {
				if (Double.isInfinite(upperBounds[i])) {
					notValidBoundPointsStatus[i] = true;
				}
			} else {
				notValidBoundPointsStatus[i] = true;
			}
		}
		setLabels(xLabel, yLabel);
	}

	/**
	 * Gets the not valid bounds
	 * @return a boolean array containing for each value the state of bounds validity
	 */
	public boolean[] getPlottedPointStatus() {
		return notValidBoundPointsStatus;
	}

	/**
	 * Draws the plot, with bounds if enabled
	 * @param withBounds
	 */
	public void drawPlot(boolean withBounds) {
		connected = true;
		for (int i = 0; i < valuesNumber; i++) {
			if (withBounds) {
				if (!notValidBoundPointsStatus[i]) {
					addPointWithErrorBars(VALUE, x[i], values[i], lowerBounds[i], upperBounds[i], true);
				} else {
					addPoint(VALUE, x[i], this.values[i], true);
				}
			} else {
				addPoint(VALUE, x[i], this.values[i], true);
			}
		}
	}

	/**
	 * Clears the plot
	 */
	public void clear() {
		clear(VALUE);
	}

	/**
	 * Sets the labels for axis
	 * @param xLabel the labels for x axis
	 * @param yLabel the label for y axis
	 */
	public void setLabels(String xLabel, String yLabel) {
		setXLabel(xLabel);
		setYLabel(yLabel);
	}

	/**
	 * Gets the maximum parameter value
	 * @return the maximum parameter value
	 */
	public double getPlotXMax() {
		double max = -Double.MAX_VALUE;
		for (int i = 0; i < valuesNumber; i++) {
			double thisVal = x[i];
			if (thisVal > max) {
				max = thisVal;
			}
		}
		return max;
	}

	/**
	 * Gets the minimum parameter value
	 * @return the minimum parameter value
	 */
	public double getPlotXMin() {
		double min = Double.MAX_VALUE;
		for (int i = 0; i < valuesNumber; i++) {
			double thisVal = x[i];
			if (thisVal < min) {
				min = thisVal;
			}
		}
		return min;
	}

	/**
	 * Gets the maximum represented value. Not valid bounds are not considered
	 * @return the maximum represented value
	 */
	public double getPlotYMax() {
		double max = -Double.MAX_VALUE;
		for (int i = 0; i < valuesNumber; i++) {
			if (!notValidBoundPointsStatus[i]) {
				//check upper bound
				if (upperBounds[i] > max) {
					max = upperBounds[i];
				}
			} else {
				//check value
				if (values[i] > max) {
					max = values[i];
				}
			}
		}
		return max;
	}

	/**
	 * Gets the minimum represented value. Not valid bounds are not considered
	 * @return the minimum represented value
	 */
	public double getPlotYMin() {
		double min = Double.MAX_VALUE;
		for (int i = 0; i < valuesNumber; i++) {
			if (!notValidBoundPointsStatus[i]) {
				//check lower bound
				if (lowerBounds[i] < min) {
					min = lowerBounds[i];
				}
			} else {
				//check the value
				if (values[i] < min) {
					min = values[i];
				}
			}
		}
		return min;
	}

}
