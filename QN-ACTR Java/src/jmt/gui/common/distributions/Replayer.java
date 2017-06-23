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

import java.io.File;

import javax.swing.ImageIcon;

import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Replayer virtual distribution</p>
 * <p>Description: This distribution will repeat previously generated data
 * reading them cyclically from a data file.</p>
 * 
 * @author Bertoli Marco
 *         Date: 12-lug-2005
 *         Time: 11.29.26
 */
public class Replayer extends Distribution {
	/**
	 * Construct a new Replayer Distribution
	 */
	public Replayer() {
		super("Replayer", "jmt.engine.random.Replayer", "jmt.engine.random.ReplayerPar", "Replayer distribution");
		this.setValueChecker(new ValueChecker() {
			public boolean checkValue(Object value) {
				Distribution d = (Distribution) value;
				if ((new File((String) d.getParameter(0).getValue())).exists()) {
					return true;
				} else {
					return false;
				}
			}
		});

		isNestable = true;
	}

	/**
	 * Returns precondition that parameters' values must satisfy for this distribution to be valid
	 * @return Message describing distribution's preconditions
	 */
	@Override
	public String getPrecondition() {
		return "specified 'fileName' does not exists. Try to provide a correct path.";
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Distribution.Parameter[] setParameters() {
		// Creates parameter array
		Parameter[] parameters = new Parameter[1];
		// Sets parameter fileName
		parameters[0] = new Parameter("fileName", "fileName", String.class, "");
		return parameters;
	}

	/**
	 * Set illustrating figure in distribution panel
	 * user to understand meaning of parameters.
	 * @return illustrating figure
	 */
	@Override
	protected ImageIcon setImage() {
		return JMTImageLoader.loadImage("Replayer");
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		return "Replayer";
	}
}
