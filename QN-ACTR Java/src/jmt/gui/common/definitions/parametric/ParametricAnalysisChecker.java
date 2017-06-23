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

package jmt.gui.common.definitions.parametric;

import java.util.Vector;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.distributions.Distribution;

/**
 * <p>Title: ParametricAnalysisChecker </p>
 * <p>Description: the main task of this class is to check which parametric analysis simulation
 *           are avaible. It offers also some other methods used to get avaible stations and
 *           classes for some types of parametric analysis.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 1-feb-2006
 *         Time: 11.00.12
 */
public class ParametricAnalysisChecker {
	private ClassDefinition cd;
	private StationDefinition sd;
	private SimulationDefinition simd;

	/**
	 *
	 * @param cd class definition
	 * @param sd simulation definition
	 * @param simd simulation definition
	 */
	public ParametricAnalysisChecker(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		this.cd = cd;
		this.sd = sd;
		this.simd = simd;
	}

	/**
	 * Checks if at least one parametric simulation is avaible.
	 * @return true if at least one parametric simulation is avaible.
	 */
	public boolean canBeEnabled() {
		boolean canBeEnabled = true;
		Vector classes = cd.getClassKeys();
		Vector stations = sd.getStationKeys();
		if ((classes.size() == 0) || (stations.size() == 0) || (getRunnableParametricAnalysis().length == 0)) {
			canBeEnabled = false;
		}
		return canBeEnabled;
	}

	/**
	 * Gets an array of String containing the description of avaible parametric simulations
	 * @return the array
	 */
	public String[] getRunnableParametricAnalysis() {
		String[] runnable;
		Vector<String> runnableVector = new Vector<String>(0, 1);
		//check if "Number of customer" parametric analysis is runnable
		Vector<Object> temp = cd.getClosedClassKeys();
		if (!temp.isEmpty()) {
			runnableVector.add(ParametricAnalysis.PA_TYPE_NUMBER_OF_CUSTOMERS);
		}
		//check if "Population mix" parametric analysis is runnable
		temp = cd.getClosedClassKeys();
		if (temp.size() == 2) {
			runnableVector.add(ParametricAnalysis.PA_TYPE_POPULATION_MIX);
		}
		//check if "Service time" parametric analysis is runnable
		temp = checkForServiceTimesParametricAnalysisAvaibleStations();
		if (!temp.isEmpty()) {
			runnableVector.add(ParametricAnalysis.PA_TYPE_SERVICE_TIMES);
		}
		//check if "Arrival rate" parametric analysis is runnable
		temp = checkForArrivalRatesParametricSimulationAvaibleClasses();
		if (!temp.isEmpty()) {
			runnableVector.add(ParametricAnalysis.PA_TYPE_ARRIVAL_RATE);
		}
		//check if "Seed" parametric analysis is avaible
		if ((!cd.getClassKeys().isEmpty()) && (!sd.getStationKeys().isEmpty())) {
			runnableVector.add(ParametricAnalysis.PA_TYPE_SEED);
		}
		//initilalize runnable array
		runnable = new String[runnableVector.size()];
		for (int i = 0; i < runnable.length; i++) {
			runnable[i] = runnableVector.get(i);
		}
		return runnable;
	}

	/**
	 *  This method has a meaning only if used inside a service times parametric analysis. It
	 *  can be used to get the keys of stations avaible to perform that kind of parametric
	 *  analysis.
	 * @return the Vector containing the keys of avaible stations
	 */
	public Vector<Object> checkForServiceTimesParametricAnalysisAvaibleStations() {
		Vector<Object> avaibleStations = new Vector<Object>(0, 1);
		Vector stations = sd.getStationKeysNoSourceSink();
		Vector classes = cd.getClassKeys();
		for (int i = 0; i < stations.size(); i++) {
			Object thisStation = stations.get(i);
			boolean stationOk = false;
			for (int j = 0; j < classes.size(); j++) {
				Object thisClass = classes.get(j);
				Object temp = sd.getServiceTimeDistribution(thisStation, thisClass);
				if (temp instanceof Distribution) {
					Distribution distr = (Distribution) temp;
					if (distr.hasMean()) {
						stationOk = true;
						break;
					}
				}
			}
			if (stationOk) {
				avaibleStations.add(thisStation);
			}
		}
		return avaibleStations;
	}

	/**
	 * This method has a meaning only if used inside a service times parametric analysis.
	 * It can be used to get the keys of classes avaible to perform that kind of parametric
	 * analysis.
	 * @param stationKey the key of the station whose service times will be varied.
	 * @return a Vector containing the keys of avaible classes
	 */
	public Vector<Object> checkForServiceTimesParametricSimulationAvaibleClasses(Object stationKey) {
		Vector<Object> valid = new Vector<Object>(0, 1);
		Vector classes = cd.getClassKeys();
		for (int j = 0; j < classes.size(); j++) {
			Object thisClass = classes.get(j);
			Object temp = sd.getServiceTimeDistribution(stationKey, thisClass);
			if (temp instanceof Distribution) {
				Distribution distr = (Distribution) temp;
				if (distr.hasMean()) {
					valid.add(thisClass);
				}
			}
		}
		return valid;
	}

	/**
	 * This method has a meaning only if used inside an arrival rate parametric analysis.
	 * It can be used to get the keys of classes avaible to perform that kind of parametric
	 * analysis.
	 * @return a Vector containing the keys of avaible classes
	 */
	public Vector<Object> checkForArrivalRatesParametricSimulationAvaibleClasses() {
		Vector<Object> valid = new Vector<Object>(0, 1);
		Vector classes = cd.getOpenClassKeys();
		for (int j = 0; j < classes.size(); j++) {
			Object thisClass = classes.get(j);
			Object temp = cd.getClassDistribution(thisClass);
			if (temp instanceof Distribution) {
				Distribution distr = (Distribution) temp;
				if (distr.hasMean()) {
					valid.add(thisClass);
				}
			}
		}
		return valid;
	}

}
