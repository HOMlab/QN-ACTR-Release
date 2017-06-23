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

import java.util.HashMap;
import java.util.Vector;

import jmt.engine.QueueNet.SimConstants;
import jmt.framework.gui.graph.MeasureValue;
import jmt.gui.common.xml.XMLConstantNames;

/**
 * Created by IntelliJ IDEA.
 * User: francesco
 * Date: 22-feb-2006
 * Time: 12.17.58
 * To change this template use File | Settings | File Templates.
 * 
 * Modified by Ashanka: In PAResultsModel(CommonModel model) for including the label change from "Queue Length" to "Customer Number"
 *                      and from "Customer Number" to "System Customer Number". The later is a System Level Perf Index.
 * 
 * 
 * Modified by Ashanka: In PAResultsModel(CommonModel model) for including the label change from "Customer Number" to "Number of Customers" 
 * 		                and "System Customer Number" to "System Number of Customers".
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 *              
 * Modified by Ashanka (Oct 2010):
 * Patch: To bug fix Multi-Sink Indices.
 * Description: Implemented the throughputTimePerSink and responseTimePerSink for the what if/parameteric simulation.
 */
public class PAResultsModel implements MeasureDefinition {
	private Vector<Measure> measures; // An array with all Measures
	private Vector<Number> parameterValues;
	private Vector<Integer> queueLength = new Vector<Integer>(), queueTime = new Vector<Integer>(), residenceTime = new Vector<Integer>(),
			responseTime = new Vector<Integer>(), utilization = new Vector<Integer>(), throughput = new Vector<Integer>(),
			dropRate = new Vector<Integer>(), systemResponseTime = new Vector<Integer>(), systemThroughput = new Vector<Integer>(),
			customerNumber = new Vector<Integer>(), systemDropRate = new Vector<Integer>();
	//Added by ASHANKA START
	//Added for System power changes for JSIM simulation engine.
	//This adds the index of the measure System Power.	
	private Vector<Integer> systemPower = new Vector<Integer>();
	//Added by ASHANKA STOP

	private Vector<Integer> responseTimePerSink = new Vector<Integer>();
	private Vector<Integer> throughputTimePerSink = new Vector<Integer>();
	
	private HashMap<String, Measure> names = new HashMap<String, Measure>();

	//private boolean[] finished;
	//private boolean simulationFinished = false;

	public PAResultsModel(CommonModel model) {
		Vector<Object> measureKeys = model.getMeasureKeys();
		measures = new Vector<Measure>(measureKeys.size());
		for (int i = 0; i < measureKeys.size(); i++) {
			Object thisMeasure = measureKeys.get(i);
			String stationName = model.getStationName(model.getMeasureStation(thisMeasure));
			String nodeType = XMLConstantNames.NODETYPE_STATION;
			// This can be a network or blocking region measure
			if (stationName == null) {
				stationName = model.getRegionName(model.getMeasureStation(thisMeasure));
				if (stationName == null) {
					stationName = "Network";
				} else {
					nodeType = XMLConstantNames.NODETYPE_REGION;
				}
			}
			String className = model.getClassName(model.getMeasureClass(thisMeasure));
			if (className == null) {
				className = "All classes";
			}
			String type = model.getMeasureType(thisMeasure);
			String measureName = stationName + "_" + className + "_" + type;
			double thisAlpha = model.getMeasureAlpha(thisMeasure).doubleValue();
			double thisPrecision = model.getMeasurePrecision(thisMeasure).doubleValue();
			// Decodes measure type
			type = type.toLowerCase();
			int numType = 0;
			if ((type.startsWith("customer") && type.endsWith("number") && !"".equalsIgnoreCase(stationName)) //condition is for backward compatibility
					|| (type.startsWith("queue") && type.endsWith("length"))// OR condition is for backward compatibility
					|| (type.startsWith("number") && type.endsWith("customers"))) {//present name is "Number of Customers"
				numType = SimConstants.QUEUE_LENGTH;
				queueLength.add(new Integer(i));
			} else if (type.startsWith("utilization")) {
				numType = SimConstants.UTILIZATION;
				utilization.add(new Integer(i));
			} else if (type.startsWith("throughput")&& !type.endsWith("sink")) {
				numType = SimConstants.THROUGHPUT;
				throughput.add(new Integer(i));
			} else if (type.startsWith("response") && type.endsWith("time")) {
				numType = SimConstants.RESPONSE_TIME;
				responseTime.add(new Integer(i));
			} else if (type.startsWith("residence") && type.endsWith("time")) {
				numType = SimConstants.RESIDENCE_TIME;
				residenceTime.add(new Integer(i));
			} else if (type.startsWith("queue") && type.endsWith("time")) {
				numType = SimConstants.QUEUE_TIME;
				queueTime.add(new Integer(i));
			} else if (type.startsWith("system")) {
				if (type.endsWith("throughput")) {
					numType = SimConstants.SYSTEM_THROUGHPUT;
					systemThroughput.add(new Integer(i));
				} else if (type.endsWith("time")) {
					numType = SimConstants.SYSTEM_RESPONSE_TIME;
					systemResponseTime.add(new Integer(i));
				}
				//Added by ASHANKA START
				//Added the following to Include the new
				//performance index called System Power
				else if (type.endsWith("power")) {
					numType = SimConstants.SYSTEM_POWER;
					systemPower.add(new Integer(i));
				}
				//Added by ASHANKA STOP
			} else if ((type.startsWith("customer") && type.endsWith("number") && "".equalsIgnoreCase(stationName))//Backward compatibility condition
					|| (type.startsWith("system") && type.endsWith("number")) //Backward compatibility condition
					|| (type.startsWith("system") && type.endsWith("customers"))) {//Present name of the perf index which is System Number of Customers
				numType = SimConstants.SYSTEM_JOB_NUMBER;
				customerNumber.add(new Integer(i));
			} else if (type.startsWith("drop") && type.endsWith("rate")) {
				numType = SimConstants.DROP_RATE;
				dropRate.add(new Integer(i));
			} else if (type.startsWith("system") && type.endsWith("rate")) {
				numType = SimConstants.SYSTEM_DROP_RATE;
				systemDropRate.add(new Integer(i));
			} else if (type.startsWith("response") && type.endsWith("sink")) {
				numType = SimConstants.RESPONSE_TIME_PER_SINK;
				responseTimePerSink.add(new Integer(i));
			} else if (type.startsWith("throughput") && type.endsWith("sink")) {
				numType = SimConstants.THROUGHPUT_PER_SINK;
				throughputTimePerSink.add(new Integer(i));
			}
			
			measures.add(new Measure(measureName, stationName, className, thisAlpha, thisPrecision, numType, nodeType));
		}
		parameterValues = model.getParametricAnalysisModel().getParameterValues();
	}

	public PAResultsModel(CommonModel model, boolean loadFromFile) {
		measures = new Vector<Measure>();
		parameterValues = model.getParametricAnalysisModel().getParameterValues();
	}

	/**
	 * Adds a new measure into this data structure.
	 * @param name measure name
	 * @param stationName reference station name
	 * @param className reference class name
	 * @param alpha measure alpha
	 * @param precision measure precision
	 * @param type type of the measure
	 */
	public void addMeasure(String name, String stationName, String className, double alpha, double precision, int type, String measureType) {
		Measure tmp = new Measure(name, stationName, className, alpha, precision, type, measureType);
		measures.add(tmp);
		names.put(name, tmp);
		// Adds measure index to the right Vector
		switch (type) {
			case SimConstants.QUEUE_TIME:
				queueTime.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.RESIDENCE_TIME:
				residenceTime.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.RESPONSE_TIME:
				responseTime.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.UTILIZATION:
				utilization.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.THROUGHPUT:
				throughput.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.QUEUE_LENGTH:
				queueLength.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.SYSTEM_RESPONSE_TIME:
				systemResponseTime.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.SYSTEM_JOB_NUMBER:
				customerNumber.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.SYSTEM_THROUGHPUT:
				systemThroughput.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.DROP_RATE:
				dropRate.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.SYSTEM_DROP_RATE:
				systemDropRate.add(new Integer(measures.size() - 1));
				break;
			//Added by ASHANKA START
			//Added for System Power performance Index inclusion in JSIM
			case SimConstants.SYSTEM_POWER:
				systemPower.add(new Integer(measures.size() - 1));
				break;
			//Added by ASHANKA STOP
			case SimConstants.RESPONSE_TIME_PER_SINK:
				responseTimePerSink.add(new Integer(measures.size() - 1));
				break;
			case SimConstants.THROUGHPUT_PER_SINK:
				throughputTimePerSink.add(new Integer(measures.size() - 1));
				break;
			
		}
	}

	public void addSample(int measureIndex, double lowerBound, double meanValue, double upperBound, boolean validity) {
		Measure requested = measures.get(measureIndex);
		requested.addSample(meanValue, upperBound, lowerBound, validity);
	}

	public void addSample(String measureName, double lowerBound, double meanValue, double upperBound, boolean validity) {
		Measure requested = names.get(measureName);
		requested.addSample(meanValue, upperBound, lowerBound, validity);
	}

	public Vector<Number> getParameterValues() {
		return parameterValues;
	}

	/**
	 * Returns total number of measures
	 *
	 * @return number of measures
	 */
	public int getMeasureNumber() {
		return measures.size();
	}

	/**
	 * Returns the station name of a given measure
	 *
	 * @param measureIndex index of the measure
	 * @return station name
	 */
	public String getStationName(int measureIndex) {
		String name = measures.get(measureIndex).stationName;
		//if (name == null) name = "Network";
		return name;
	}

	/**
	 * Returns the class name of a given measure
	 *
	 * @param measureIndex index of the measure
	 * @return class name
	 */
	public String getClassName(int measureIndex) {
		String name = measures.get(measureIndex).className;
		//if (name == null) name = "All classes";
		return name;
	}

	/**
	 * Returns the alpha of a given measure
	 *
	 * @param measureIndex index of the measure
	 * @return alpha
	 */
	public double getAlpha(int measureIndex) {
		return measures.get(measureIndex).alpha;
	}

	/**
	 * Returns the precision of a given measure
	 *
	 * @param measureIndex index of the measure
	 * @return precision
	 */
	public double getPrecision(int measureIndex) {
		return measures.get(measureIndex).precision;
	}

	/**
	 * Returns number of analized samples for a given measure. For parametric
	 * analysis this number is equal to the number of performed steps
	 *
	 * @param measureIndex index of the measure
	 * @return number of analized samples
	 */
	public int getAnalizedSamples(int measureIndex) {
		return (measures.get(measureIndex)).getNumberOfSamples();
	}

	/**
	 * Returns the name of a given measure
	 *
	 * @param measureIndex index of the measure
	 * @return name of the measure
	 */
	public String getName(int measureIndex) {
		return measures.get(measureIndex).name;
	}

	/**
	 * Returns the vector of values of a given measure. Each element of the vector
	 * is an instance of <code>MeasureValue</code> interface.
	 *
	 * @param measureIndex index of the measure
	 * @return vector of values
	 */
	public Vector<MeasureValue> getValues(int measureIndex) {
		return measures.get(measureIndex).getValues();
	}

	/**
	 * Returns the state of a measure
	 *
	 * @param measureIndex index of the measure
	 * @param step the step
	 * @return true if it was computed successfully, false otherwise
	 */
	public boolean getMeasureState(int measureIndex, int step) {
		MeasureValueImpl value = (MeasureValueImpl) (getValues(measureIndex).get(step));
		return value.isValid();
	}

	/**
	 * Returns the type of a measure
	 *
	 * @param measureIndex index of the measure
	 * @return measure type
	 */
	public int getMeasureType(int measureIndex) {
		return measures.get(measureIndex).type;
	}

	/**
	 * Returns the node type of a measure
	 *
	 * @param measureIndex index of the measure
	 * @return measure type
	 */
	public String getNodeType(int measureIndex) {
		return measures.get(measureIndex).nodeType;
	}

	/**
	 * Returns an array with the measureIndex of every queue length measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getQueueLengthMeasures() {
		int[] tmp = new int[queueLength.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = queueLength.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every throughput measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getThroughputMeasures() {
		int[] tmp = new int[throughput.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = throughput.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every queue time measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getQueueTimeMeasures() {
		int[] tmp = new int[queueTime.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = queueTime.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every residence time measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getResidenceTimeMeasures() {
		int[] tmp = new int[residenceTime.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = residenceTime.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every response time measure
	 * @return an array with measures' index
	 */
	public int[] getResponseTimeMeasures() {
		int[] tmp = new int[responseTime.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = responseTime.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every utilization measure
	 * @return an array with measures' index
	 */
	public int[] getUtilizationMeasures() {
		int[] tmp = new int[utilization.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = utilization.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every system response time measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getSystemResponseTimeMeasures() {
		int[] tmp = new int[systemResponseTime.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = systemResponseTime.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every system throughput measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getSystemThroughputMeasures() {
		int[] tmp = new int[systemThroughput.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = systemThroughput.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every customer number measure
	 *
	 * @return an array with measures' index
	 */
	public int[] getCustomerNumberMeasures() {
		int[] tmp = new int[customerNumber.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = customerNumber.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every drop rate measure
	 * @return an array with measures' index
	 */
	public int[] getDropRateMeasures() {
		int[] tmp = new int[dropRate.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = dropRate.get(i).intValue();
		}
		return tmp;
	}

	/**
	 * Returns an array with the measureIndex of every system drop rate measure
	 * @return an array with measures' index
	 */
	public int[] getSystemDropRateMeasures() {
		int[] tmp = new int[systemDropRate.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = systemDropRate.get(i).intValue();
		}
		return tmp;
	}

	// ------------------------- USELESS METHODS --------------------------------

	/**
	 * Not implemented
	 *
	 * @param measureIndex index of the measure that this listener should listen
	 * @param listener     listener to add or null to remove old one.
	 */
	public void addMeasureListener(int measureIndex, MeasureListener listener) {
		//not implemented
	}

	/**
	 * Not implemented
	 *
	 * @param listener listener to be set or null to unset previous one
	 */
	public void setProgressTimeListener(ProgressTimeListener listener) {
		//Not implemented
	}

	/**
	 * Not implemented
	 *
	 * @return true
	 */
	public boolean isSimulationFinished() {
		return true;
	}

	/**
	 * Not implemented, return always 0
	 *
	 * @return 0
	 */
	public double getPollingInterval() {
		return 0;
	}

	/**
	 * Not implemented
	 *
	 * @return 0
	 */
	public double getProgressTime() {
		return 0;
	}

	/**
	 * Returns the state of a measure, that can be MEASURE_IN_PROGRESS, MEASURE_NO_SAMPLES,
	 * MEASURE_FAILED, MEASURE_SUCCESS
	 *
	 * @param measureIndex index of the measure
	 * @return measure state
	 */
	public int getMeasureState(int measureIndex) {
		return 0; //To change body of implemented methods use File | Settings | File Templates.
	}

	// ------------------------- end USELESS METHODS --------------------------------

	/**
	 * Inner class to store parameters of each measure
	 */
	protected class Measure {
		public String name, stationName, className, nodeType;
		public Vector<MeasureValue> values;
		public double alpha, precision;
		public int samples, state, type;

		/**
		 * Construct a new Neasure object
		 * @param name measure name
		 * @param stationName reference station name
		 * @param className reference class name
		 * @param alpha measure alpha
		 * @param precision measure precision
		 * @param type type of the measure
		 */
		public Measure(String name, String stationName, String className, double alpha, double precision, int type, String nodeType) {
			this.name = name;
			this.stationName = stationName;
			this.className = className;
			this.alpha = alpha;
			this.precision = precision;
			//this.state = state;
			this.type = type;
			values = new Vector<MeasureValue>();
			this.nodeType = nodeType;
		}

		/**
		 * Adds a new sample to current measure
		 * @param meanValue mean value of the sample
		 * @param upperBound upper bound of the sample
		 * @param lowerBound lower bound of the sample
		 */
		public void addSample(double meanValue, double upperBound, double lowerBound, boolean validity) {
			MeasureValueImpl val = new MeasureValueImpl(meanValue, upperBound, lowerBound, validity);
			values.add(val);
			samples++;

		}

		/**
		 * Gets the number of sampples for this measure
		 * @return the number of samples
		 */
		public int getNumberOfSamples() {
			return samples;
		}

		/**
		 * Gets the Vector containing measure values. Each element is an instance of
		 * <code>MeasureValue</code>
		 * @return the Vector containing the values of this Measure
		 */
		public Vector<MeasureValue> getValues() {
			return values;
		}

		/**
		 * Gets the Vector containing measure values. Each element is an instance of
		 * <code>MeasureValue</code>
		 * @return the Vector containing the values of this Measure
		 */
		public int getType() {
			return type;
		}
	}

	/**
	 * Inner class that implements Value interface
	 */
	public class MeasureValueImpl implements MeasureValue {
		private double mean, upper, lower;
		boolean valid;

		/**
		 * Creates a new MeasureValue object
		 * @param meanValue mean value of the sample
		 * @param upperBound sample upper bound
		 * @param lowerBound sample lower bound
		 * @param isValid true if the measure could be computed with the requested precision
		 */
		public MeasureValueImpl(double meanValue, double upperBound, double lowerBound, boolean isValid) {
			mean = meanValue;
			upper = upperBound;
			lower = lowerBound;
			valid = isValid;
		}

		public double getUpperBound() {
			return upper;
		}

		public double getLowerBound() {
			return lower;
		}

		public double getMeanValue() {
			return mean;
		}

		public boolean isValid() {
			return valid;
		}
	}

	//Added by ASHANKA START
	public int[] getSystemPowerMeasures() {
		int[] tmp = new int[systemPower.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = systemPower.get(i).intValue();
		}
		return tmp;
	}
	//Added by ASHANKA STOP

	public int[] getResponsetimePerSinkMeasures() {
		int[] tmp = new int[responseTimePerSink.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = responseTimePerSink.get(i).intValue();
		}
		return tmp;
	}

	public int[] getThroughputPerSinkMeasures() {
		int[] tmp = new int[throughputTimePerSink.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = throughputTimePerSink.get(i).intValue();
		}
		return tmp;
	}
}
