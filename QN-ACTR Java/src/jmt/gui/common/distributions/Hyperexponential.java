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
 * <p>Title: Hyperexponential Distribution</p>
 * <p>Description: Hyperexponential distribution data structure n� 1. This one has parameters
 * (p, lambda1, lambda2)</p>
 * 
 * @author Bertoli Marco
 *         Date: 11-lug-2005
 *         Time: 18.36.53
 */
public class Hyperexponential extends Distribution {
	/**
	 * Construct a new Hyperexponential Distribution
	 */
	public Hyperexponential() {
		super("Hyperexponential", "jmt.engine.random.HyperExp", "jmt.engine.random.HyperExpPar", "Hyperexponential distribution");
		hasC = true;
		hasMean = true;
		isNestable = true;
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Parameter[] setParameters() {
		// Creates parameter array
		Parameter[] parameters = new Parameter[3];
		// Sets parameter p
		parameters[0] = new Parameter("p", "p", Double.class, new Double(0.6));
		// Checks value of lambda must be greater than 0 and lesser than 1
		parameters[0].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 0 && d.doubleValue() < 1) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter lambda1
		parameters[1] = new Parameter("lambda1", "\u03BB1", Double.class, new Double(0.5));
		// Checks value of lambda must be greater than 0
		parameters[1].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() >= 0) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter lambda2
		parameters[2] = new Parameter("lambda2", "\u03BB2", Double.class, new Double(0.2));
		// Checks value of lambda must be greater than 0
		parameters[2].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() >= 0) {
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
		return JMTImageLoader.loadImage("Hyperexponential");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "hyp(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
				+ FormatNumber(((Double) parameters[1].getValue()).doubleValue()) + "; "
				+ FormatNumber(((Double) parameters[2].getValue()).doubleValue()) + ")";
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
		// Uses morse's two step algorithm
		double l1, l2, p;
		p = .5 * (1.0 - Math.sqrt((c * c - 1.0) / (c * c + 1.0)));
		l1 = 2.0 * p / mean;
		l2 = 2.0 * (1.0 - p) / mean;

		if (p > 0 && p < 1 && l1 > 0 && l2 > 0 && mean > 0) {
			getParameter(0).setValue(new Double(p));
			getParameter(1).setValue(new Double(l1));
			getParameter(2).setValue(new Double(l2));
			this.c = c;
			this.mean = mean;
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		double p, l1, l2;
		p = ((Double) getParameter(0).getValue()).doubleValue();
		l1 = ((Double) getParameter(1).getValue()).doubleValue();
		l2 = ((Double) getParameter(2).getValue()).doubleValue();
		mean = p / l1 + (1.0 - p) / l2;
		c = Math.sqrt(2.0 * (p / (l1 * l1) + (1.0 - p) / (l2 * l2)) - mean * mean) / mean;
	}
}
