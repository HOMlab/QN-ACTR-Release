/*
 * Created on Oct 31, 2006
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

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

//import jmt.gui.common.distributions.Distribution.Parameter;
//import jmt.gui.common.distributions.Distribution.ValueChecker;
import javax.swing.ImageIcon;

import jmt.gui.common.resources.JMTImageLoader;

/**
* <p>Title: MMPP(2) Distribution</p>
* <p>Description: MMPP(2) distribution data structure</p>
* 
* @author Casale Giuliano
* @date   Octover 24, 2008
*/

public class MMPP2 extends Distribution {
	/**
	 * Construct a new MMPP2 distribution
	 */
	public MMPP2() {
		super("Burst (MMPP2)", // name in GUI
				"jmt.engine.random.MMPP2Distr", "jmt.engine.random.MMPP2Par", "MMPP(2)");
		hasMean = false;
		hasC = false;
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */

	@Override
	protected Parameter[] setParameters() {
		// Creates parameter array
		Parameter[] parameters = new Parameter[4]; //MMPP(2) is specified by four rates

		/* arrival rate of jobs if MMPP is in state 0 */
		parameters[0] = new Parameter("lambda0", "\u03BB0", //unicode character for lambda0
				Double.class, new Double(1.0));
		// Checks value of k must be greater then 0
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

		/* arrival rate of jobs if MMPP is in state 1 */
		parameters[1] = new Parameter("lambda1", "\u03BB1", //unicode character for lambda1
				Double.class, new Double(2.0));
		// Checks value of k must be greater then 0
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

		/* rate of jump from state 0 to state 1 */
		parameters[2] = new Parameter("sigma0", "\u03C30", //unicode character for sigma0 
				Double.class, new Double(0.5));
		// Checks value of alpha must greater or equal then 2
		parameters[2].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 0.0) {
					return true;
				} else {
					return false;
				}
			}
		});

		/* rate of jump from state 1 to state 0 */
		parameters[3] = new Parameter("sigma1", "\u03C31", //unicode character for sigma1
				Double.class, new Double(0.1));
		// Checks value of k must be greater then 0
		parameters[3].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 0.0) {
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
		return JMTImageLoader.loadImage("MMPP2");
	}

	/**
	 * Return distribution short description
	 * @return distribution short description
	 */
	@Override
	public String toString() {
		return "mmpp2(" + FormatNumber(((Double) parameters[0].getValue()).doubleValue()) + ","
				+ FormatNumber(((Double) parameters[1].getValue()).doubleValue()) + ","
				+ FormatNumber(((Double) parameters[2].getValue()).doubleValue()) + ","
				+ FormatNumber(((Double) parameters[3].getValue()).doubleValue()) + ")";
	}

}
