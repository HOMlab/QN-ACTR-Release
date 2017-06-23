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
 * <p>Title: Pareto Distribution</p>
 * <p>Description: Pareto distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 28-giu-2005
 *         Time: 10.47.42
 */
public class Pareto extends Distribution {
	/**
	 * Construct a new Pareto Distribution
	 */
	public Pareto() {
		super("Pareto", "jmt.engine.random.Pareto", "jmt.engine.random.ParetoPar", "Pareto distribution");
		hasMean = true;
		hasC = true;
		isNestable = true;
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Parameter[] setParameters() {
		// Creates parameter array
		Parameter[] parameters = new Parameter[2];
		// Sets parameter alpha
		parameters[0] = new Parameter("alpha", "\u03B1", Double.class, new Double(3));
		// Checks value of alpha must greater or equal then 2
		parameters[0].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() >= 2) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter k
		parameters[1] = new Parameter("k", "k", Double.class, new Double(1));
		// Checks value of k must be greater then 0
		parameters[1].setValueChecker(new ValueChecker() {
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
		return JMTImageLoader.loadImage("Pareto");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "par(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
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
		// k = mean - mean /  (1+sqrt(1+1/c^2)) && alpha = 1 + sqrt(1 + 1 / c^2)
		// Backups old parameters to restore them upon a false result
		Object oldk = getParameter("k").getValue();
		Object olda = getParameter("alpha").getValue();
		if (getParameter("k").setValue(new Double(mean - mean / (1 + Math.sqrt(1 + 1 / (c * c)))))
				&& getParameter("alpha").setValue(new Double(1 + Math.sqrt(1 + 1 / (c * c))))) {
			this.mean = mean;
			this.c = c;
		} else {
			getParameter("k").setValue(oldk);
			getParameter("alpha").setValue(olda);
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		double a, k;
		a = ((Double) getParameter("alpha").getValue()).doubleValue();
		k = ((Double) getParameter("k").getValue()).doubleValue();
		mean = a * k / (a - 1);
		c = 1 / Math.sqrt(a * a - 2 * a);
	}
}
