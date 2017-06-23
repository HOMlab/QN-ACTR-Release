/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.analytical;

import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;

/**
 * <p><b>Name:</b> ModelApproximator</p> 
 * <p><b>Description:</b> 
 * This class is used to perform any form of FESC (Flow Equivalent Service Center)
 * approximations before model solution.
 * If needed, input model is alterated, than results are adjusted according to used
 * approximation.
 * </p>
 * <p><b>Date:</b> 04/apr/07
 * <b>Time:</b> 16:40:15</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class ModelFESCApproximator {
	private static final int SKIP = -1;
	private ExactModel inputModel, outputModel;
	private int iteration;
	private int[] multipleServerList; // An array with mapping station -> added delay for multiple server handling
	private boolean multipleServerApproximated;

	/**
	 * Builds a new Model Approximator basing on given exact model
	 * @param model input model
	 * @param iteration iteration for what-if results
	 */
	public ModelFESCApproximator(ExactModel model, int iteration) {
		inputModel = model;
		this.iteration = iteration;
	}

	/**
	 * This method will return approximate model that will be solved
	 * @return the model that must be solved with 
	 */
	public ExactModel getModelToBeSolved() {
		initApproximation();
		return outputModel;
	}

	/**
	 * This method will modify original model with the approximated results stored
	 * @return original model with results
	 */
	public ExactModel processModelAfterSolution() {
		postProcessApproximation();
		return inputModel;
	}

	/**
	 * This method will initialize every known approximation
	 */
	private void initApproximation() {
		// Multiserver approximation
		if (inputModel.isMultipleServers() && !inputModel.isOpen()) {
			multipleServerApproximated = true;
			outputModel = new ExactModel(inputModel);
			// Finds multi-server stations (will be replaced with station + delay)
			multipleServerList = new int[inputModel.getStations()];
			int multipleCount = 0;
			for (int i = 0; i < multipleServerList.length; i++) {
				if (inputModel.getStationServers()[i] > 1 && inputModel.getStationTypes()[i] != ExactConstants.STATION_DELAY) {
					multipleServerList[i] = inputModel.getStations() + multipleCount++;
				} else {
					multipleServerList[i] = SKIP;
				}
			}
			// Adds delays
			outputModel.resize(inputModel.getStations() + multipleCount, inputModel.getClasses());
			int[] stationTypes = outputModel.getStationTypes();
			String[] stationNames = outputModel.getStationNames();
			double[][][] serviceTimes = outputModel.getServiceTimes();
			double[][] visits = outputModel.getVisits();
			int[] stationServers = outputModel.getStationServers();

			for (int i = 0; i < multipleServerList.length; i++) {
				if (multipleServerList[i] > 0) {
					int delay = multipleServerList[i]; // Index of delay
					stationTypes[delay] = ExactConstants.STATION_DELAY;
					stationNames[delay] = stationNames[i] + "_MULTISERVER_FESC"; // This is used for debugging purposes
					visits[delay] = visits[i];
					int serverNum = stationServers[i];
					// Service time for station i is Si / n, while for delay is (n-1) / n * Si
					for (int cl = 0; cl < inputModel.getClasses(); cl++) {
						for (int ld = 0; ld < serviceTimes[i][cl].length; ld++) {
							serviceTimes[delay][cl][ld] = serviceTimes[i][cl][ld] * (serverNum - 1.0) / serverNum;
							serviceTimes[i][cl][ld] = serviceTimes[i][cl][ld] / serverNum;
						}
					}
					stationServers[i] = 1;
				}
			}
			outputModel.resetResults();
		} else {
			multipleServerApproximated = false;
			outputModel = inputModel;
		}
	}

	/**
	 * This method will post-process every known approximation
	 */
	private void postProcessApproximation() {
		// Multiserver approximation
		if (multipleServerApproximated) {
			double[][] q = new double[inputModel.getStations()][inputModel.getClasses()];
			double[][] r = new double[inputModel.getStations()][inputModel.getClasses()];
			double[][] u = new double[inputModel.getStations()][inputModel.getClasses()];
			double[][] x = new double[inputModel.getStations()][inputModel.getClasses()];
			for (int st = 0; st < inputModel.getStations(); st++) {
				for (int cl = 0; cl < inputModel.getClasses(); cl++) {
					q[st][cl] = outputModel.getQueueLen()[st][cl][iteration];
					r[st][cl] = outputModel.getResTimes()[st][cl][iteration];
					u[st][cl] = outputModel.getUtilization()[st][cl][iteration] * inputModel.getStationServers()[st];
					x[st][cl] = outputModel.getThroughput()[st][cl][iteration];
					int delay = multipleServerList[st];
					if (delay > 0) {
						q[st][cl] += outputModel.getQueueLen()[delay][cl][iteration];
						r[st][cl] += outputModel.getResTimes()[delay][cl][iteration];
					}
				}
			}
			inputModel.setResults(q, x, r, u, iteration);
		} else {
			// Nothing to be done here....
		}
	}

}
