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
 * <p>Title: Erlang distribution</p>
 * <p>Description: Erlang distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 6-lug-2005
 *         Time: 14.40.47
 */
public class Erlang extends Distribution {
	/**
	 * Construct a new Erlang Distribution
	 */
	public Erlang() {
		super("Erlang", "jmt.engine.random.Erlang", "jmt.engine.random.ErlangPar", "Erlang distribution");
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
		parameters[0] = new Parameter("alpha", "\u03B1", Double.class, new Double(0.8));
		// Checks value of alpha must greater then 0
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

		// Sets parameter r
		parameters[1] = new Parameter("r", "r", Double.class, new Double(4));
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
	 * @return illustrating figure
	 */
	@Override
	protected ImageIcon setImage() {
		return JMTImageLoader.loadImage("Erlang");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "erl(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + "; "
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
		// alpha = 1 / (c*c*mean) && r = 1 / (c*c)
		// Backups old parameters to restore them upon a false result
		Object oldr = getParameter("r").getValue();
		Object olda = getParameter("alpha").getValue();
		if (getParameter("r").setValue(new Double(1 / (c * c))) && getParameter("alpha").setValue(new Double(1 / (c * c * mean)))) {
			this.mean = mean;
			this.c = c;
		} else {
			getParameter("r").setValue(oldr);
			getParameter("alpha").setValue(olda);
		}
	}

	/**
	 * This method is called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	@Override
	public void updateCM() {
		mean = ((Double) getParameter("r").getValue()).doubleValue() / ((Double) getParameter("alpha").getValue()).doubleValue();
		c = 1 / Math.sqrt(((Double) getParameter("r").getValue()).doubleValue());
	}
}
