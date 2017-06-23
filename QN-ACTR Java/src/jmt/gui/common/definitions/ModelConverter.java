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

package jmt.gui.common.definitions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.distributions.Exponential;
import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RandomRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.common.serviceStrategies.LDStrategy;
import jmt.gui.common.serviceStrategies.ZeroStrategy;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;
import Jama.Matrix;

/**
 * <p>Title: Model Converter</p>
 * <p>Description: This class provides methods to convert models between JMODEL, JSIM,
 * JMVA and JABA.</p>
 *
 * @author Bertoli Marco
 *         Date: 20-feb-2006
 *         Time: 12.23.12
 */
public class ModelConverter {
	// --- Methods for conversion JMVA --> JSIM -----------------------------------------------
	/**
	 * Converts a JMVA model to JSIM. Conversion is performed by equalling service times in every
	 * LI station (adjusting visits) and creating FCFS queues with exponential service time distribution.
	 * <br>
	 * Visits are converted with routing probability, to mantain correctness of computated response time values
	 * a "virtual" node called  RefStation is added as a reference station for closed classes and
	 * routed when (in terms of mean values) a single visit is performed in the system. (as visits values are scaled to
	 * compute probability distribution).
	 * <br>
	 * Note that a single router node, called "Router" is used to route jobs through the
	 * entire network, simplyfing its thopology. Stations (LI, LD and Delays) are connected in a parallel form with
	 * the Router.
	 * @param input JMVA model (read only access)
	 * @param output target JSIM or JMODEL model. This is expected to be empty
	 * @return a List with all found warnings during conversion (in String format).
	 */
	public static List<String> convertJMVAtoJSIM(ExactModel input, CommonModel output) {
		// Changes default values, then restores default back at the end of method
		String defaultRouting = Defaults.get("stationRoutingStrategy");
		String defaultQueue = Defaults.get("stationQueueStrategy");
		Defaults.set("stationRoutingStrategy", RandomRouting.class.getName());
		Defaults.set("stationQueueStrategy", CommonConstants.QUEUE_STRATEGY_FCFS);

		ArrayList<String> res = new ArrayList<String>();
		// Keys for unique items
		Object sourceKey = null, sinkKey = null, routerKey, refRouterKey = null;
		// Sums visit for each class
		double[] visitSum = new double[input.getClasses()];
		// Visits matrix (row: stations, column: classes)
		double[][] visits = input.getVisits();

		// Convert classes
		Object[] classKeys = new Object[input.getClasses()];
		for (int i = 0; i < input.getClasses(); i++) {
			String name = input.getClassNames()[i];
			int type = input.getClassTypes()[i];
			// This holds customers if class is closed or arrival rate if open
			double data = input.getClassData()[i];
			Object key;

			visitSum[i] = 1; // Sums visit for each station. This is initialized to one as we
			// count visit to reference station
			for (int j = 0; j < input.getStations(); j++) {
				visitSum[i] += visits[j][i];
			}

			if (type == ExactConstants.CLASS_CLOSED) {
				// Closed class
				key = output.addClass(name, CommonConstants.CLASS_TYPE_CLOSED, 0, new Integer((int) data), null);
			} else {
				// Open class
				Exponential ex = new Exponential();
				ex.setMean(1 / data);
				key = output.addClass(name, CommonConstants.CLASS_TYPE_OPEN, 0, null, ex);
			}
			classKeys[i] = key;
		}

		routerKey = output.addStation("Router", CommonConstants.STATION_TYPE_ROUTER);

		// Creates source, sink and router (if needed)
		if (input.isClosed() || input.isMixed()) {
			refRouterKey = output.addStation("RefStation", CommonConstants.STATION_TYPE_ROUTER);
			// Makes connection between refRouter and router
			output.setConnected(refRouterKey, routerKey, true);
			output.setConnected(routerKey, refRouterKey, true);
			// Gives warning on refStation
			res
					.add("A special node, called \"RefStation\" was added in order to compute correctly the System Response Time and System Throughput of closed classes."
							+ " Its presence is fundamental to compute correctly number of visits at each station for closed classes.");
		}

		if (input.isOpen() || input.isMixed()) {
			sourceKey = output.addStation("Source", CommonConstants.STATION_TYPE_SOURCE);
			sinkKey = output.addStation("Sink", CommonConstants.STATION_TYPE_SINK);
			//Makes connections between source, sink and router
			output.setConnected(sourceKey, routerKey, true);
			output.setConnected(routerKey, sinkKey, true);
		}

		// Convert stations
		Object[] stationKeys = new Object[input.getStations()];
		for (int i = 0; i < input.getStations(); i++) {
			String name = input.getStationNames()[i];
			int type = input.getStationTypes()[i];
			int servers = input.getStationServers()[i];
			double[][] serviceTimes = input.getServiceTimes()[i];
			Object key = null;
			switch (type) {
				case ExactConstants.STATION_DELAY:
					// Delay
					key = output.addStation(name, CommonConstants.STATION_TYPE_DELAY);
					// Sets distribution for each class
					for (int j = 0; j < classKeys.length; j++) {
						Exponential ex = new Exponential();
						ex.setMean(serviceTimes[j][0]);
						output.setServiceTimeDistribution(key, classKeys[j], ex);
					}
					break;
				case ExactConstants.STATION_LI:
					// Load independent
					key = output.addStation(name, CommonConstants.STATION_TYPE_SERVER);
					output.setStationNumberOfServers(new Integer(servers), key);
					output.setStationQueueStrategy(key, CommonConstants.QUEUE_STRATEGY_STATION_PS);
					// Sets distribution for each class
					for (int j = 0; j < classKeys.length; j++) {
						Exponential ex = new Exponential();
						ex.setMean(serviceTimes[j][0]);
						output.setServiceTimeDistribution(key, classKeys[j], ex);
					}
					break;
				case ExactConstants.STATION_LD:
					// Load dependent - this is single class only, but here
					// we support multiclass too (future extensions).
					key = output.addStation(name, CommonConstants.STATION_TYPE_SERVER);
					output.setStationNumberOfServers(new Integer(servers), key);
					output.setStationQueueStrategy(key, CommonConstants.QUEUE_STRATEGY_STATION_PS);
					// Sets distribution for each class
					for (int j = 0; j < classKeys.length; j++) {
						LDStrategy lds = new LDStrategy();
						Object rangeKey = lds.getAllRanges()[0];
						for (int range = 0; range < serviceTimes[j].length; range++) {
							// First range is already available
							if (range > 0) {
								rangeKey = lds.addRange();
							}
							Exponential ex = new Exponential();
							ex.setMean(serviceTimes[j][range]);
							lds.setRangeDistribution(rangeKey, ex);
							lds.setRangeDistributionMean(rangeKey, Double.toString(serviceTimes[j][range]));
						}
						output.setServiceTimeDistribution(key, classKeys[j], lds);
					}
					break;
			}
			stationKeys[i] = key;

			// Make connections with router
			output.setConnected(routerKey, key, true);
			output.setConnected(key, routerKey, true);
		}

		// Sets routing for router
		for (int i = 0; i < classKeys.length; i++) {
			ProbabilityRouting pr = new ProbabilityRouting();
			output.setRoutingStrategy(routerKey, classKeys[i], pr);
			for (int j = 0; j < stationKeys.length; j++) {
				pr.getValues().put(stationKeys[j], new Double(visits[j][i] / visitSum[i]));
			}

			// Sets refRouter as reference station for closed class, sets its routing and avoid put jobs into sink
			if (output.getClassType(classKeys[i]) == CommonConstants.CLASS_TYPE_CLOSED) {
				output.setClassRefStation(classKeys[i], refRouterKey);
				pr.getValues().put(refRouterKey, new Double(1 / visitSum[i]));
				if (sinkKey != null) {
					pr.getValues().put(sinkKey, new Double(0.0));
				}
			}
			// Sets source as reference station for open class and sets sink routing, avoid routing to refRouter
			else {
				output.setClassRefStation(classKeys[i], sourceKey);
				pr.getValues().put(sinkKey, new Double(1 / visitSum[i]));
				if (refRouterKey != null) {
					pr.getValues().put(refRouterKey, new Double(0.0));
				}
			}
		}

		// Create measures
		for (Object classKey : classKeys) {
			for (Object stationKey : stationKeys) {
				// Queue length
				output.addMeasure(SimulationDefinition.MEASURE_QL, stationKey, classKey);
				// Residence Time
				output.addMeasure(SimulationDefinition.MEASURE_RD, stationKey, classKey);
				// Utilization
				output.addMeasure(SimulationDefinition.MEASURE_U, stationKey, classKey);
				// Throughput
				output.addMeasure(SimulationDefinition.MEASURE_X, stationKey, classKey);
			}
		}
		// Restores default values
		Defaults.set("stationRoutingStrategy", defaultRouting);
		Defaults.set("stationQueueStrategy", defaultQueue);

		// Manage preloading
		output.manageJobs();

		// Return warnings
		return res;
	}

	// ----------------------------------------------------------------------------------------

	// --- Methods for conversion JSIM --> JMVA -----------------------------------------------
	/**
	 * Converts a JSIM model to JMVA. Visits are computed from thopology. To work well this
	 * method requires:
	 * <ul>
	 * <li> Reference station to be set for each class
	 * <li> Every station must have at least one incoming connection and one outgoing connection
	 * <li> Routing strategy have to be <code>ProbabilityRouting</code> or <code>RandomRouting</code>
	 * </ul>
	 * @param input JSIM model (read only)
	 * @param output empty JMVA model (write)
	 * @return a vector that enumerates all conversion warnings and how they have been fixed
	 */
	public static List<String> convertJSIMtoJMVA(CommonModel input, ExactModel output) {
		// Normalize probability routing
		input.manageProbabilities();

		// Used to store warnings
		ArrayList<String> res = new ArrayList<String>();
		int classNum, stationNum;
		// Used to iterate on lists
		Iterator<Object> it;

		// Number of classes
		classNum = input.getClassKeys().size();
		Vector<Object> classKeys = input.getClassKeys();

		// Find number of convertible stations
		it = input.getStationKeys().iterator();
		stationNum = 0;
		Vector<Object> stationKeys = new Vector<Object>();
		while (it.hasNext()) {
			Object key = it.next();
			String stationType = input.getStationType(key);
			if (stationType.equals(CommonConstants.STATION_TYPE_DELAY) || stationType.equals(CommonConstants.STATION_TYPE_SERVER)) {
				stationNum++;
				stationKeys.add(key);
			}
			// Show here warning if a station type is discarded (fork and join)
		}

		// Resizes output data structure
		output.resize(stationNum, classNum);

		// Exports class data
		int[] classTypes = new int[classNum];
		String[] classNames = new String[classNum];
		double[] classData = new double[classNum];

		for (int i = 0; i < classNum; i++) {
			Object key = classKeys.get(i);
			classNames[i] = input.getClassName(key);
			if (input.getClassType(key) == CommonConstants.CLASS_TYPE_CLOSED) {
				// Closed class parameters
				classTypes[i] = ExactConstants.CLASS_CLOSED;
				classData[i] = input.getClassPopulation(key).doubleValue();
			} else {
				// Open class parameters
				classTypes[i] = ExactConstants.CLASS_OPEN;
				Distribution d = (Distribution) input.getClassDistribution(key);
				if (d.hasMean()) {
					classData[i] = 1.0 / d.getMean();
				} else {
					classData[i] = 1;
					res.add(input.getClassName(key) + " arrival distribution does not have a valid mean value."
							+ " Arrival rate for that class was set to default value 1");
				}
			}
		}
		// Sets extracted values to output
		output.setClassNames(classNames);
		output.setClassTypes(classTypes);
		output.setClassData(classData);
		classNames = null;
		classTypes = null;
		classData = null;

		// Exports station data
		double[][][] serviceTimes = new double[stationNum][classNum][];
		int[] stationTypes = new int[stationNum];
		String[] stationNames = new String[stationNum];
		int[] stationServers = new int[stationNum];
		for (int st = 0; st < stationNum; st++) {
			Object key = stationKeys.get(st);
			stationNames[st] = input.getStationName(key);
			Integer serverNum = input.getStationNumberOfServers(key);
			if (serverNum != null && serverNum.intValue() > 0) {
				stationServers[st] = serverNum.intValue();
			} else {
				stationServers[st] = 1;
			}
			if (input.getStationType(key).equals(CommonConstants.STATION_TYPE_DELAY)) {
				stationTypes[st] = ExactConstants.STATION_DELAY;
			} else {
				stationTypes[st] = ExactConstants.STATION_LI;
			}

			// Sets service time for each class
			for (int cl = 0; cl < classNum; cl++) {
				Object serv = input.getServiceTimeDistribution(key, classKeys.get(cl));
				if (serv instanceof Distribution) {
					Distribution d = (Distribution) serv;
					serviceTimes[st][cl] = new double[1]; // This is not load dependent
					if (d.hasMean()) {
						serviceTimes[st][cl][0] = d.getMean();
					} else {
						serviceTimes[st][cl][0] = 1;
						res.add(output.getStationNames()[st] + " service time distribution for " + output.getClassNames()[cl]
								+ " does not have a valid mean value." + " Service time is set to default value 1");
					}
				} else if (serv instanceof ZeroStrategy) {
					serviceTimes[st][cl] = new double[1];
					serviceTimes[st][cl][0] = 0;
				} else if (serv instanceof LDStrategy) {
					LDStrategy lds = (LDStrategy) serv;
					if (output.isClosed() && !output.isMultiClass()) {
						int pop = input.getClassPopulation(classKeys.get(cl)).intValue();
						serviceTimes[st][cl] = new double[pop]; // This is load dependent
						stationTypes[st] = ExactConstants.STATION_LD;
						for (int i = 0; i < pop; i++) {
							serviceTimes[st][cl][i] = lds.getMeanValue(i);
						}
					} else {
						serviceTimes[st][cl] = new double[1]; // This is not load dependent
						serviceTimes[st][cl][0] = lds.getMeanValue(1);
						res.add("LD stations are supported only if model is closed and " + "single class. " + stationNames[st]
								+ " was converted to a LI station.");
					}
				}
			}
		}
		// Sets extracted values to output
		output.setStationNames(stationNames);
		stationNames = null;
		output.setStationServers(stationServers);
		stationServers = null;
		output.setStationTypes(stationTypes);
		stationTypes = null;
		output.setServiceTimes(serviceTimes);
		serviceTimes = null;

		// Now calculates visits starting from routing.
		Vector<Object> stations; // This is not equivalent to stationKeys as routers are considered
		double[][] visits = new double[stationNum][classNum];
		double[] vis = null; // array used to store results of mldivide
		for (int cl = 0; cl < classNum; cl++) {
			if (output.getClassTypes()[cl] == ExactConstants.CLASS_OPEN) {
				stations = input.getStationKeysNoSourceSink();
				// Open class, must calculate routing from source
				Object refStat = input.getClassRefStation(classKeys.get(cl));
				double[] p0;
				if (refStat == null) {
					// Reference station for this class was not set
					Vector<Object> sources = input.getStationKeysSource();
					if (sources.size() > 0) {
						refStat = sources.get(0);
						res.add("Reference station for " + output.getClassNames()[cl] + " was " + "not set. " + input.getStationName(refStat)
								+ " was chosen.");
					} else {
						res.add("Reference station for " + output.getClassNames()[cl] + " was " + "not set. " + output.getStationNames()[0]
								+ " was chosen.");
					}
				}

				if (refStat != null) {
					p0 = getRoutingProbability(refStat, classKeys.get(cl), input, stations, res);
				} else {
					p0 = new double[stations.size()];
					p0[0] = 1; // Assumes that all jobs enters in first station
				}
				try {
					Matrix b = new Matrix(p0, 1);
					Matrix P = new Matrix(buildProbabilityMatrix(stations, input, classKeys.get(cl), res));
					// V = (P-eye(3))' \ (-b') where \ is "mldivide"
					Matrix V = P.minus(Matrix.identity(stations.size(), stations.size())).solveTranspose(b.uminus());
					vis = V.getColumnPackedCopy();
				} catch (Exception e) {
					// Matrix is singular
					res.add("Cannot compute correctly visits for " + output.getClassNames()[cl] + " as" + " network thopology was badly specified");
				}

			} else {
				// Closed class, system is indefinded, so sets visits to reference station to
				// 1 and builds a smaller P matrix
				stations = new Vector<Object>(input.getStationKeysNoSourceSink());
				// Finds reference station
				Object refStat = input.getClassRefStation(classKeys.get(cl));
				if (refStat == null) {
					refStat = stations.get(0);
					res.add("Reference station for " + output.getClassNames()[cl] + " was " + "not set. " + input.getStationName(refStat)
							+ " was chosen.");
				}

				// Sets visits to reference station (if allowed) to 1
				if (stationKeys.contains(refStat)) {
					visits[stationKeys.lastIndexOf(refStat)][cl] = 1;
				}
				// Removes reference station from stations vector and computes p0
				stations.remove(refStat);
				double[] p0 = getRoutingProbability(refStat, classKeys.get(cl), input, stations, res);

				try {
					Matrix b = new Matrix(p0, 1);
					Matrix P = new Matrix(buildProbabilityMatrix(stations, input, classKeys.get(cl), res));
					// V = (P-eye(3))' \ (-b') where \ is "mldivide"
					Matrix V = P.minus(Matrix.identity(stations.size(), stations.size())).solveTranspose(b.uminus());
					vis = V.getColumnPackedCopy();
				} catch (Exception e) {
					// Matrix is singular
					res.add("Cannot compute correctly visits for " + output.getClassNames()[cl] + " as" + " network thopology was badly specified");
				}
			}

			// Puts computed values into visits matrix. Rounds at 1e-10 for machine precision issues
			if (vis != null) {
				for (int i = 0; i < vis.length; i++) {
					// Skips not allowed components (Routers, Terminals, Fork, Join....)
					if (stationKeys.contains(stations.get(i))) {
						visits[stationKeys.lastIndexOf(stations.get(i))][cl] = 1e-10 * Math.round(vis[i] * 1e10);
					}
				}
			}
		}
		output.setVisits(visits);
		return res;
	}

	/**
	 * This method will return a reachability vector for given station and given class
	 * @param stationKey search's key for given station
	 * @param classKey search's key for given class
	 * @param model model data structure
	 * @param stations vector with ordered station keys (the same order is used in output array)
	 * @param warnings vector Vector to store warnings found during computation
	 * @return an array with probability to reach each other station starting from given station
	 */
	private static double[] getRoutingProbability(Object stationKey, Object classKey, CommonModel model, List<Object> stations, List<String> warnings) {
		double[] p = new double[stations.size()];
		RoutingStrategy strategy = (RoutingStrategy) model.getRoutingStrategy(stationKey, classKey);
		if (strategy instanceof ProbabilityRouting && !model.getStationType(stationKey).equals(CommonConstants.STATION_TYPE_FORK)) {
			Map routingMap = strategy.getValues();
			Iterator it = routingMap.keySet().iterator();
			while (it.hasNext()) {
				Object dest = it.next();
				if (stations.lastIndexOf(dest) >= 0) {
					p[stations.lastIndexOf(dest)] = ((Double) routingMap.get(dest)).doubleValue();
				}
			}
		} else {
			if (model.getStationType(stationKey).equals(CommonConstants.STATION_TYPE_FORK)) {
				warnings.add("Fork-Join are not supported in JMVA. They are considered as routers.");
			} else if (!(strategy instanceof RandomRouting)) {
				warnings.add("\"" + strategy.getName() + "\" routing strategy in " + model.getClassName(classKey) + " for "
						+ model.getStationName(stationKey) + " is not allowed. This was considered as RandomRouting");
			}

			Vector<Object> links = model.getForwardConnections(stationKey);
			int linksNum = links.size();
			// Now ignores sinks for closed classes
			if (model.getClassType(classKey) == CommonConstants.CLASS_TYPE_CLOSED) {
				for (int i = 0; i < links.size(); i++) {
					if (model.getStationType(links.get(i)).equals(CommonConstants.STATION_TYPE_SINK)) {
						linksNum--;
					}
				}
			}

			double weight = 1.0 / linksNum;
			for (int i = 0; i < links.size(); i++) {
				if (stations.contains(links.get(i))) {
					p[stations.lastIndexOf(links.get(i))] = weight;
				}
			}
		}

		return p;
	}

	/**
	 * Builds a routing probability matrix for a given set of stations
	 * @param stations stations to be considered
	 * @param model data structure
	 * @param classKey search's key for target class
	 * @param warnings Vector where computation warnings must be put
	 * @return computated routing probability matrix
	 */
	private static double[][] buildProbabilityMatrix(List<Object> stations, CommonModel model, Object classKey, List<String> warnings) {
		double[][] matrix = new double[stations.size()][stations.size()];
		double[] tmp;
		for (int i = 0; i < stations.size(); i++) {
			tmp = getRoutingProbability(stations.get(i), classKey, model, stations, warnings);
			System.arraycopy(tmp, 0, matrix[i], 0, tmp.length);
		}
		return matrix;
	}
	// ----------------------------------------------------------------------------------------

	// --- Methods for conversion JABA --> JMVA -----------------------------------------------

	// ----------------------------------------------------------------------------------------

	// --- Methods for conversion JMVA --> JABA -----------------------------------------------

	// ----------------------------------------------------------------------------------------
}
