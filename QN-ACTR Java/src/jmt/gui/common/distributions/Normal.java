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

package jmt.gui.common.distributions;

import javax.swing.ImageIcon;

import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Normal Distribution</p>
 * <p>Description: Normal distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 7-lug-2005
 *         Time: 17.46.19
 */
public class Normal extends Distribution {
	/**
	 * Construct a new Normal Distribution
	 */
	public Normal() {
		super("Normal", "jmt.engine.random.Normal", "jmt.engine.random.NormalPar", "Normal distribution");
		hasMean = true;
		hasC = true;
		isNestable = true;
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Distribution.Parameter[] setParameters() {
		// Creates parameter array
		Distribution.Parameter[] parameters = new Distribution.Parameter[2];
		// Sets parameter mean
		parameters[0] = new Distribution.Parameter("mean", "\u03BC", Double.class, new Double(2));
		// Checks value of mean must be equal greater then 0
		parameters[0].setValueChecker(new Distribution.ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() >= 0) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter standardDeviation
		parameters[1] = new Distribution.Parameter("standardDeviation", "\u03C3", Double.class, new Double(1));
		// Checks value of standardDeviation must be greater then 0
		parameters[1].setValueChecker(new Distribution.ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 0) {
					return true;
				} else {
					return false;
				}
			}
		});

		return parameters;
	}

	/**
	 * Set illustrating figure in distribution panel
	 * user to understand meaning of parameters.
	 * @return illustrating figure
	 */
	@Override
	protected ImageIcon setImage() {
		return JMTImageLoader.loadImage("Normal");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "norm(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
				+ FormatNumber(((Double) parameters[1].getValue()).doubleValue()) + ")";
	}

	/**
	 * Sets the mean for this distribution
	 * @param value mean value
	 */
	@Override
	public void setMean(double value) {
		setCM(value, c);
	}

	/**
	 * Sets the variation coefficient C for this distribution
	 * @param value variation coefficient C value
	 */
	@Override
	public void setC(double value) {
		setCM(mean, value);
	}

	/**
	 * Sets Mean and C values
	 * @param mean mean value
	 * @param c c value
	 */
	protected void setCM(double mean, double c) {
		// sigma = c^2*mean^2
		// Backups old parameters to restore them upon a false result
		Object oldm = getParameter("mean").getValue();
		Object olds = getParameter("standardDeviation").getValue();
		if (getParameter("mean").setValue(new Double(mean)) && getParameter("standardDeviation").setValue(new Double(c * c * mean * mean))) {
			this.mean = mean;
			this.c = c;
		} else {
			getParameter("mean").setValue(oldm);
			getParameter("standardDeviation").setValue(olds);
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		mean = ((Double) getParameter("mean").getValue()).doubleValue();
		c = Math.sqrt(((Double) getParameter("standardDeviation").getValue()).doubleValue()) / mean;
	}
}
