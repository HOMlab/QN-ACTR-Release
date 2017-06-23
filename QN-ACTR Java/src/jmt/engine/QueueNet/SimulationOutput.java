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

package jmt.engine.QueueNet;

import java.io.File;
import java.util.LinkedList;

import jmt.engine.dataAnalysis.Measure;
import jmt.engine.simEngine.Simulation;

/**
 * A SimulationOutput object is used to print the results of all the measures.
 *
 * @author Stefano Omini, Bertoli Marco.
 */
public abstract class SimulationOutput {

	protected Simulation sim;
	protected QueueNetwork network;
	protected Measure[] measureList;

	/**
	 * Creates a new instance of MeasureOutput class and obtains the references
	 * to all Measure object.
	 * @param simulation Reference to the simulation to be "outputted".
	 */
	public SimulationOutput(Simulation simulation) {
		this.sim = simulation;
		network = sim.getNetwork();

		LinkedList<Measure> list = network.getMeasures();

		measureList = new Measure[list.size()];

		for (int i = 0; i < measureList.length; i++) {
			measureList[i] = list.get(i);

		}
	}

	/**
	 * Writes the output of the measures.
	 * This method must be overridden.
	 * @return created output file
	 */
	public abstract File writeAllMeasures();

}
