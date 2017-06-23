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

import java.util.Vector;

import jmt.engine.QueueNet.SimConstants;
import jmt.engine.dataAnalysis.TempMeasure;
import jmt.framework.gui.graph.MeasureValue;

/**
 * <p>Title: Result's Model data structure</p>
 * <p>Description: This class will collect temporary results from simulation at
 * specified time intervals, and final results. It will not read simulator's generated XML
 * as it provides only final results but will rely on TempMeasure data structures provided
 * by <code>Dispatcher_jSIMschema</code>.</p>
 * 
 * @author Bertoli Marco
 *         Date: 23-set-2005
 *         Time: 15.57.43
 *         
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class.
 */
public class ResultsModel implements MeasureDefinition {
	private TempMeasure[] measures; // An array with all TempMeasures
	private Vector<Integer> queueLength = new Vector<Integer>(), queueTime = new Vector<Integer>(), residenceTime = new Vector<Integer>(),
			responseTime = new Vector<Integer>(), utilization = new Vector<Integer>(), throughput = new Vector<Integer>(),
			dropRate = new Vector<Integer>(), systemResponseTime = new Vector<Integer>(), systemThroughput = new Vector<Integer>(),
			customerNumber = new Vector<Integer>(), systemDropRate = new Vector<Integer>();

	//Added by ASHANKA START
	//Vector ro contain the Measure index of the System Power. 
	//This is done as a part of the addition of System Power
	//inclusion in the JSIM as a performance Index.
	private Vector<Integer> systemPower = new Vector<Integer>();
	//Added by ASHANKA STOP
	
	private Vector<Integer> responseTimePerSink = new Vector<Integer>();
	private Vector<Integer> throughputTimePerSink = new Vector<Integer>();

	private Vector[] measuresVector; // For each TempMeasure holds a Vector with its value at every poll
	private boolean[] finished;
	private MeasureListener[] listeners; // Listener array to notify GUI of measure change events
	private ProgressTimeListener ptlistener = null;
	private boolean simulationFinished = false;
	private double pollingInterval;
	private double progressTime = 0.0;

	/**
	 * Constructs a new ResultsModel
	 * @param pollingInterval measure polling interval
	 */
	public ResultsModel(double pollingInterval) {
		this.pollingInterval = pollingInterval;
	}

	/**
	 * Sets this data structure, provided an array of TempMeasures
	 * @param measures array of TempMeasures, as
	 * returned by <code>Dispatcher_jSIMschema.getTempMeasures()</code>
	 * @param progressTime Progress time of simulation
	 */
	public synchronized void setTempMeasures(TempMeasure[] measures, double progressTime) {
		this.measures = measures;
		measuresVector = new Vector[measures.length];
		finished = new boolean[measures.length];
		listeners = new MeasureListener[measures.length];
		// Now parses measure type and puts each TempMeasure in right vector
		for (int i = 0; i < measures.length; i++) {
			switch (measures[i].getMeasureType()) {
				case SimConstants.QUEUE_TIME:
					queueTime.add(new Integer(i));
					break;
				case SimConstants.RESIDENCE_TIME:
					residenceTime.add(new Integer(i));
					break;
				case SimConstants.RESPONSE_TIME:
					responseTime.add(new Integer(i));
					break;
				case SimConstants.UTILIZATION:
					utilization.add(new Integer(i));
					break;
				case SimConstants.THROUGHPUT:
					throughput.add(new Integer(i));
					break;
				case SimConstants.QUEUE_LENGTH:
					queueLength.add(new Integer(i));
					break;
				case SimConstants.SYSTEM_RESPONSE_TIME:
					systemResponseTime.add(new Integer(i));
					break;
				case SimConstants.SYSTEM_JOB_NUMBER:
					customerNumber.add(new Integer(i));
					break;
				case SimConstants.SYSTEM_THROUGHPUT:
					systemThroughput.add(new Integer(i));
					break;
				case SimConstants.DROP_RATE:
					dropRate.add(new Integer(i));
					break;
				case SimConstants.SYSTEM_DROP_RATE:
					systemDropRate.add(new Integer(i));
					break;
				//Added by ASHANKA START
				//Added as a part of addition of System Power to
				//JSIM as a new Performance Index.
				case SimConstants.SYSTEM_POWER:
					systemPower.add(new Integer(i));
					break;
				//Added by ASHANKA STOP
					
				case SimConstants.RESPONSE_TIME_PER_SINK:
					responseTimePerSink.add(new Integer(i));
					break;
				case SimConstants.THROUGHPUT_PER_SINK:
					throughputTimePerSink.add(new Integer(i));
					break;
			}
			// Adds to allMeasures HashMap a vector to collect all values of this measure
			Vector<MeasureValueImpl> temp_mean = new Vector<MeasureValueImpl>();
			temp_mean.add(new MeasureValueImpl(measures[i]));
			measuresVector[i] = temp_mean;
			finished[i] = false;
		}
		this.progressTime = progressTime;
		// Notifies progress time listener listener
		if (ptlistener != null) {
			ptlistener.timeChanged(progressTime);
		}
	}

	/**
	 * Refresh stored tempMeasures.
	 * PRECONDITION: refresh method MUST HAVE BEEN called on every TempMeasure before calling
	 * this method. So simply this method is designed to be called after every
	 * <code>Dispatcher_jSIMschema.refreshTempMeasures()</code> call.
	 * @param progressTime Progress time of simulation
	 */
	public synchronized void refresh(double progressTime) {
		if (measures == null) {
			return;
		}
		// Simulation finished
		if (progressTime >= 1.0) {
			simulationFinished = true;
		}
		// Notifies progress time listener listener
		if (ptlistener != null) {
			ptlistener.timeChanged(progressTime);
		}
		for (int i = 0; i < measures.length; i++) {
			// If measure is not finished, register new value for that measure
			if (!finished[i]) {
				// If simulation is finished, try to get final value
				if (simulationFinished) {
					while (!measures[i].isFinished()) {
						measures[i].refreshMeasure();
					}
				}
				measuresVector[i].add(new MeasureValueImpl(measures[i]));
				if (measures[i].isFinished()) {
					finished[i] = true;
				}
				// Notifies measure listener (if any)
				if (listeners[i] != null) {
					listeners[i].measureChanged(measuresVector[i], finished[i]);
				}
			}
		}
	}

	/**
	 * Adds a MeasureListener to listen to measure change events for given measure.
	 * Each measure can have ONLY one MeasureListener to avoid unnecessary computational
	 * efforts to manage a pool of listeners.
	 * @param measureIndex index of the measure that this listener should listen
	 * @param listener listener to add or null to remove old one.
	 */
	public synchronized void addMeasureListener(int measureIndex, MeasureListener listener) {
		// Sanity check on parameters
		if (measures == null || measureIndex < 0 || measureIndex >= measures.length) {
			return;
		}
		listeners[measureIndex] = listener;
	}

	/**
	 * Returns total number of measures
	 * @return number of measures
	 */
	public synchronized int getMeasureNumber() {
		return measures.length;
	}

	/**
	 * Returns the station name of a given measure
	 * @param measureIndex index of the measure
	 * @return station name
	 */
	public synchronized String getStationName(int measureIndex) {
		return measures[measureIndex].getNodeName();
	}

	/**
	 * Returns the class name of a given measure
	 * @param measureIndex index of the measure
	 * @return class name
	 */
	public synchronized String getClassName(int measureIndex) {
		return measures[measureIndex].getJobClass();
	}

	/**
	 * Returns the alpha of a given measure
	 * @param measureIndex index of the measure
	 * @return alpha
	 */
	public synchronized double getAlpha(int measureIndex) {
		return 1 - measures[measureIndex].getAlpha();
	}

	/**
	 * Returns the precision of a given measure
	 * @param measureIndex index of the measure
	 * @return precision
	 */
	public synchronized double getPrecision(int measureIndex) {
		return measures[measureIndex].getPrecision();
	}

	/**
	 * Returns the name of a given measure
	 * @param measureIndex index of the measure
	 * @return name of the measure
	 */
	public synchronized String getName(int measureIndex) {
		return measures[measureIndex].getName();
	}

	/**
	 * Returns the node type of a given measure
	 * @param measureIndex index of the measure
	 * @return name of the measure
	 */
	public synchronized String getNodeType(int measureIndex) {
		return measures[measureIndex].getNodeType();
	}

	/**
	 * Returns number of analized samples for a given measure
	 * @param measureIndex index of the measure
	 * @return number of analized samples
	 */
	public synchronized int getAnalizedSamples(int measureIndex) {
		return measures[measureIndex].getNsamples();
	}

	/**
	 * Returns the vector of Temporary values of a given measure. Each element of the vector
	 * is an instance of <code>Value</code> interface.
	 * @param measureIndex index of the measure
	 * @return vector of termporary values until now
	 */
	public synchronized Vector<MeasureValue> getValues(int measureIndex) {
		return measuresVector[measureIndex];
	}

	/**
	 * Returns the state of a measure, that can be MEASURE_IN_PROGRESS, MEASURE_NO_SAMPLES,
	 * MEASURE_FAILED, MEASURE_SUCCESS
	 * @param measureIndex index of the measure
	 * @return measure state
	 */
	public synchronized int getMeasureState(int measureIndex) {
		if (!measures[measureIndex].isFinished()) {
			return MEASURE_IN_PROGRESS;
		} else if (measures[measureIndex].receivedNoSamples()) {
			return MEASURE_NO_SAMPLES;
		} else if (!measures[measureIndex].isSuccessful()) {
			return MEASURE_FAILED;
		} else {
			return MEASURE_SUCCESS;
		}
	}

	/**
	 * Returns the type of a measure
	 * @param measureIndex index of the measure
	 * @return measure type
	 */
	public int getMeasureType(int measureIndex) {
		return measures[measureIndex].getMeasureType();
	}

	/**
	 * Returns an array with the measureIndex of every queue length measure
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

	/**
	 * Returns an array with the measureIndex of every queue time measure
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

	//Added by ASHANKA START
	//Return the Indices of each Measures related to System Power.
	public int[] getSystemPowerMeasures() {
		int[] tmp = new int[systemPower.size()];
		for (int i = 0; i < tmp.length; i++) {
			tmp[i] = systemPower.get(i).intValue();
		}
		return tmp;
	}

	//Added by ASHANKA STOP

	/**
	 * Returns simulation polling interval. This is the time elapsed between two temp values.
	 * @return simulation polling interval in seconds
	 */
	public double getPollingInterval() {
		return pollingInterval;
	}

	/**
	 * Sets a ProgressTimeListener to listen to progress time change events. This is unique.
	 * @param listener listener to be set or null to unset previous one
	 */
	public synchronized void setProgressTimeListener(ProgressTimeListener listener) {
		ptlistener = listener;
	}

	/**
	 * Returns current simulation progress time
	 * @return current progress time
	 */
	public double getProgressTime() {
		return progressTime;
	}

	/**
	 * Returns if simulation has finished, so results are fixed
	 * @return true iff simulation has finished
	 */
	public synchronized boolean isSimulationFinished() {
		return simulationFinished;
	}

	/**
	 * Implementation of Value interface
	 */
	public class MeasureValueImpl implements MeasureValue {
		private double value, upper, lower;

		public MeasureValueImpl(TempMeasure tm) {
			value = tm.getTempMean();
			upper = tm.getUpperBound();
			lower = tm.getLowerBound();
		}

		public double getUpperBound() {
			return upper;
		}

		public double getLowerBound() {
			return lower;
		}

		public double getMeanValue() {
			return value;
		}
	}

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
