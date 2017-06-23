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
 * <p>Title: Poisson Distribution</p>
 * <p>Description: Poisson distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 11-lug-2005
 *         Time: 15.27.13
 */
public class Poisson extends Distribution {
	/**
	 * Construct a new Poisson Distribution
	 */
	public Poisson() {
		super("Poisson", "jmt.engine.random.Poisson", "jmt.engine.random.PoissonPar", "Poisson distribution");
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
		Parameter[] parameters = new Parameter[1];
		// Sets parameter lambda
		parameters[0] = new Parameter("mean", "\u03BB", Double.class, new Double(0.5));
		// Checks value of lambda must be greater than 0
		parameters[0].setValueChecker(new ValueChecker() {
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
		return JMTImageLoader.loadImage("Poisson");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "poisson(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + ")";
	}

	/**
	 * Sets the mean for this distribution
	 * @param value mean value
	 */
	@Override
	public void setMean(double value) {
		// mean = lambda
		if (getParameter(0).setValue(new Double(value))) {
			mean = value;
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		mean = ((Double) getParameter(0).getValue()).doubleValue();
	}
}
