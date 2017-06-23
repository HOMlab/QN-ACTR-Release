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
 * <p>Title: Gamma Distribution</p>
 * <p>Description: Gamma distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 6-lug-2005
 *         Time: 15.01.39
 */
public class Gamma extends Distribution {
	/**
	 * Construct a new Gamma Distribution
	 */
	public Gamma() {
		super("Gamma", "jmt.engine.random.GammaDistr", "jmt.engine.random.GammaDistrPar", "Gamma distribution");
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
		// Sets parameter alpha
		parameters[0] = new Distribution.Parameter("alpha", "\u03B1", Double.class, new Double(4));
		// Checks value of alpha must greater then 0
		parameters[0].setValueChecker(new Distribution.ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 0) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter lambda
		parameters[1] = new Distribution.Parameter("lambda", "\u03BB", Double.class, new Double(0.5));
		// Checks value of k must be greater then 0
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
		return JMTImageLoader.loadImage("Gamma");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "gam(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
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
		// lambda = mean * c^2 && alpha = 1 / (c*c)
		// Backups old parameters to restore them upon a false result
		Object oldl = getParameter("lambda").getValue();
		Object olda = getParameter("alpha").getValue();
		if (getParameter("lambda").setValue(new Double(mean * c * c)) && getParameter("alpha").setValue(new Double(1 / (c * c)))) {
			this.mean = mean;
			this.c = c;
		} else {
			getParameter("lambda").setValue(oldl);
			getParameter("alpha").setValue(olda);
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		mean = ((Double) getParameter("alpha").getValue()).doubleValue() * ((Double) getParameter("lambda").getValue()).doubleValue();
		c = 1 / Math.sqrt(((Double) getParameter("alpha").getValue()).doubleValue());
	}

}
