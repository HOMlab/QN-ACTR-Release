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

import jmt.framework.gui.graph.MeasureValue;

/**
 * <p>Title: Measure Definition Interface</p>
 * <p>Description: This interface is implemented by each measure definition data structure. It is
 * provided to allow ResultsWindow not to be directly linked to underlayng data structure, allowing
 * two or more different kind of result data structure. Actually it can be useful to have one
 * measure data structure that reads data from the engine and one that loads a saved file.</p>
 * 
 * @author Bertoli Marco
 *         Date: 23-set-2005
 *         Time: 23.14.55
 *         
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 */
public interface MeasureDefinition {
	/**
	 * Constants used fot the getMeasureState method
	 */
	public static final int MEASURE_IN_PROGRESS = 0;
	public static final int MEASURE_SUCCESS = 1;
	public static final int MEASURE_FAILED = 2;
	public static final int MEASURE_NO_SAMPLES = 3;

	/**
	 * Adds a MeasureListener to listen to measure change events for given measure.
	 * Each measure can have ONLY one MeasureListener to avoid unnecessary computational
	 * efforts to manage a pool of listeners.
	 * @param measureIndex index of the measure that this listener should listen
	 * @param listener listener to add or null to remove old one.
	 */
	public void addMeasureListener(int measureIndex, MeasureListener listener);

	/**
	 * Returns total number of measures
	 * @return number of measures
	 */
	public int getMeasureNumber();

	/**
	 * Returns the station name of a given measure
	 * @param measureIndex index of the measure
	 * @return station name
	 */
	public String getStationName(int measureIndex);

	/**
	 * Returns the class name of a given measure
	 * @param measureIndex index of the measure
	 * @return class name
	 */
	public String getClassName(int measureIndex);

	/**
	 * Returns the alpha of a given measure
	 * @param measureIndex index of the measure
	 * @return alpha
	 */
	public double getAlpha(int measureIndex);

	/**
	 * Returns the precision of a given measure
	 * @param measureIndex index of the measure
	 * @return precision
	 */
	public double getPrecision(int measureIndex);

	/**
	 * Returns number of analized samples for a given measure
	 * @param measureIndex index of the measure
	 * @return number of analized samples
	 */
	public int getAnalizedSamples(int measureIndex);

	/**
	 * Returns the name of a given measure
	 * @param measureIndex index of the measure
	 * @return name of the measure
	 */
	public String getName(int measureIndex);

	/**
	 * Returns the vector of Temporary values of a given measure. Each element of the vector
	 * is an instance of <code>Value</code> interface.
	 * @param measureIndex index of the measure
	 * @return vector of termporary values until now
	 */
	public Vector<MeasureValue> getValues(int measureIndex);

	/**
	 * Returns the state of a measure, that can be MEASURE_IN_PROGRESS, MEASURE_NO_SAMPLES,
	 * MEASURE_FAILED, MEASURE_SUCCESS
	 * @param measureIndex index of the measure
	 * @return measure state
	 */
	public int getMeasureState(int measureIndex);

	/**
	 * Returns the type of a measure
	 * @param measureIndex index of the measure
	 * @return measure type
	 */
	public int getMeasureType(int measureIndex);

	/**
	 * Returns an array with the measureIndex of every queue length measure
	 * @return an array with measures' index
	 */
	public int[] getQueueLengthMeasures();

	/**
	 * Returns an array with the measureIndex of every throughput measure
	 * @return an array with measures' index
	 */
	public int[] getThroughputMeasures();

	/**
	 * Returns an array with the measureIndex of every drop rate measure
	 * @return an array with measures' index
	 */
	public int[] getDropRateMeasures();

	/**
	 * Returns an array with the measureIndex of every queue time measure
	 * @return an array with measures' index
	 */
	public int[] getQueueTimeMeasures();

	/**
	 * Returns an array with the measureIndex of every residence time measure
	 * @return an array with measures' index
	 */
	public int[] getResidenceTimeMeasures();

	/**
	 * Returns an array with the measureIndex of every response time measure
	 * @return an array with measures' index
	 */
	public int[] getResponseTimeMeasures();

	/**
	 * Returns an array with the measureIndex of every utilization measure
	 * @return an array with measures' index
	 */
	public int[] getUtilizationMeasures();

	/**
	 * Returns an array with the measureIndex of every system response time measure
	 * @return an array with measures' index
	 */
	public int[] getSystemResponseTimeMeasures();

	/**
	 * Returns an array with the measureIndex of every system throughput measure
	 * @return an array with measures' index
	 */
	public int[] getSystemThroughputMeasures();

	/**
	 * Returns an array with the measureIndex of every system drop rate measure
	 * @return an array with measures' index
	 */
	public int[] getSystemDropRateMeasures();

	/**
	 * Returns an array with the measureIndex of every customer number measure
	 * @return an array with measures' index
	 */
	public int[] getCustomerNumberMeasures();

	//Added by ASHANKA START
	//Added a new performance index for the JSIM Graph Simulation tool
	//This is System Power given by the formula: System/System Response Time
	/**
	 * Returns an array with the measureIndex of every system power measure
	 * @return an array with measures' index
	 */
	public int[] getSystemPowerMeasures();

	//Added by ASHANKA STOP
	
	public int[] getResponsetimePerSinkMeasures();
	public int[] getThroughputPerSinkMeasures();

	/**
	 * Returns the node type of a given measure
	 * @param measureIndex index of the measure
	 * @return name of the measure
	 */
	public String getNodeType(int measureIndex);

	/**
	 * Sets a ProgressTimeListener to listen to progress time change events. This is unique.
	 * @param listener listener to be set or null to unset previous one
	 */
	public void setProgressTimeListener(ProgressTimeListener listener);

	/**
	 * Returns if simulation has finished, so results are fixed
	 * @return true iff simulation has finished
	 */
	public boolean isSimulationFinished();

	/**
	 * Returns simulation polling interval. This is the time elapsed between two temp values.
	 * @return simulation polling interval in seconds
	 */
	public double getPollingInterval();

	/**
	 * Returns current simulation progress time
	 * @return current progress time
	 */
	public double getProgressTime();

	// --- Listener Interfaces ------------------------------------------------------------------------
	/**
	 * Interface used to specify a listener on a measure. This is useful to
	 * implement a GUI with a reactive approch.
	 */
	public interface MeasureListener {
		public void measureChanged(Vector measureValues, boolean finished);
	}

	/**
	 * Interface used to specify a listener on progress time. This is useful to
	 * implement a GUI with a reactive approch.
	 */
	public interface ProgressTimeListener {
		public void timeChanged(double progressTime);
	}
}
