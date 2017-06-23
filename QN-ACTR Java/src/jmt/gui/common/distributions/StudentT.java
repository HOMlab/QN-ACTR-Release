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
 * <p>Title: Student's t-distribution</p>
 * <p>Description: Student's t-distribution data structure</p>
 * 
 * @author Bertoli Marco
 *         Date: 11-lug-2005
 *         Time: 15.14.13
 */
public class StudentT extends Distribution {
	/**
	 * Construct a new Student's t-distribution
	 */
	public StudentT() {
		super("StudentT", "jmt.engine.random.StudentT", "jmt.engine.random.StudentTPar", "Student's t-distribution");
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
		// Sets parameter nu (freedom grades)
		parameters[0] = new Parameter("freedom", "\u03C5", Double.class, new Double(2));
		// Checks value of freedom must be greater than 0 and integral
		parameters[0].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Double d = (Double) value;
				if (d.doubleValue() > 1 && Math.floor(d.doubleValue()) == d.doubleValue()) {
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
		return JMTImageLoader.loadImage("StudentT");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "studT(" + ((Double) parameters[0].getValue()).longValue() + ")";
	}
}
