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
 * <p>Title: Uniform Distribution</p>
 * <p>Description: Uniform distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 11-lug-2005
 *         Time: 15.35.17
 */
public class Uniform extends Distribution {
	/**
	 * Construct a new Uniform Distribution
	 */
	public Uniform() {
		super("Uniform", "jmt.engine.random.Uniform", "jmt.engine.random.UniformPar", "Uniform distribution");
		hasC = true;
		hasMean = true;
		isNestable = true;
		this.setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Distribution d = (Distribution) value;
				if (((Double) d.getParameter("min").getValue()).doubleValue() < ((Double) d.getParameter("max").getValue()).doubleValue()) {
					return true;
				} else {
					return false;
				}
			}
		});
	}

	/**
	 * Returns precondition that parameters' values must satisfy for this distribution to be valid
	 * @return Message describing distribution's preconditions
	 */
	@Override
	public String getPrecondition() {
		return "value of 'max' must be greater than value of 'min'";
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Distribution.Parameter[] setParameters() {
		// Creates parameter array
		Distribution.Parameter[] parameters = new Distribution.Parameter[2];
		// Sets parameter min
		parameters[0] = new Distribution.Parameter("min", "min", Double.class, new Double(0));
		// Checks value of min must be greater or equal to 0
		parameters[0].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() >= 0) {
					return true;
				} else {
					return false;
				}
			}
		});

		// Sets parameter max
		parameters[1] = new Distribution.Parameter("max", "max", Double.class, new Double(1));
		// Checks value of max must be greater or equal to 0
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

		return parameters;
	}

	/**
	 * Set illustrating figure in distribution panel
	 * user to understand meaning of parameters.
	 * @return illustrating figure
	 */
	@Override
	protected ImageIcon setImage() {
		return JMTImageLoader.loadImage("Uniform");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "U(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
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
		double max = mean + mean * c / Math.sqrt(3);
		double min = mean - mean * c / Math.sqrt(3);
		// Backups old parameters to restore them upon a false result
		Object oldmax = getParameter("max").getValue();
		Object oldmin = getParameter("min").getValue();
		if (getParameter("max").setValue(new Double(max)) && getParameter("min").setValue(new Double(min))) {
			this.mean = mean;
			this.c = c;
		} else {
			getParameter("max").setValue(oldmax);
			getParameter("min").setValue(oldmin);
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		double max = ((Double) getParameter("max").getValue()).doubleValue();
		double min = ((Double) getParameter("min").getValue()).doubleValue();
		mean = (max + min) / 2;
		c = (max - min) / (max + min) * Math.sqrt(3);
	}

}
