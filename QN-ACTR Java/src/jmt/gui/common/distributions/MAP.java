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
* <p>Title: Constant Distribution</p>
* <p>Description: MAP distribution data structure</p>
* 
* @author Casale Giuliano
*         Date: 16-Dc-2006
*         Time: 20.02.30
*/
public class MAP extends Distribution {
	/**
	 * Construct a new MAP distribution
	 */
	public MAP() {
		super("MAP", "jmt.engine.random.MAP", "jmt.engine.random.MAPPar", "MAP");
		hasMean = false;
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

		parameters[0] = new Parameter("D0", "D0 Matrix", String.class, new String("[-1,0.1;0.5,-1]"));
		// Checks value of alpha must greater or equal then 2
		parameters[0].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				return true; // disable check value
			}
		});

		parameters[1] = new Parameter("D1", "D1 Matrix", String.class, new String("[0.9,0;0.1,0.4]"));
		// Checks value of alpha must greater or equal then 2
		parameters[1].setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				return true; // disable check value
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
		return JMTImageLoader.loadImage("MMPP2");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "MAP(" + (String) parameters[0].getValue() + "," + (String) parameters[1].getValue() + ")";
	}

}
