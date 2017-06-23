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

import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 22-lug-2005
 * Time: 17.33.22
 * This interface provides methods for simulation parameters definition.
 * Those include measures and other parameters for simulation setup, such as
 * simulation seed or simulation maximum duration.
 * Modified by Bertoli Marco 3-oct-2005
 *
 * Modified by Francesco D'Aquino
 * 
 * @author Bertoli Marco (new measures)
 * 
 * Modified by Ashanka (Aug 09):
 * Desc: The code to include the changes for label changes from 
 *       1. Queue Length to Customer Number 
 *       2. Number of Customers to System Customer Number 
 * 
 * Modified by Ashanka (Sep 09):
 * Desc: The code to include the changes for label changes from 
 *       1. Customer Number to Number of Customers
 *       2. System Customer Number to System Number of Customers.
 *       
 * Modified by Ashanka (May 2010): 
 * Project: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 */
public interface SimulationDefinition {

	/**Code for response time measure*/
	public static final String MEASURE_RP = "Response Time";
	/**Code for residence time measure*/
	public static final String MEASURE_RD = "Residence Time";
	/**Code for queue length measure*/
	public static final String MEASURE_QL = "Number of Customers";
	/**Code for average queue time measure*/
	public static final String MEASURE_QT = "Queue Time";
	/**Code for average utilization*/
	public static final String MEASURE_U = "Utilization";
	/**Code for average throughput*/
	public static final String MEASURE_X = "Throughput";
	/**Code for average system throughput*/
	public static final String MEASURE_S_X = "System Throughput";
	/**Code for average system response time*/
	public static final String MEASURE_S_RP = "System Response Time";
	/**Code for average system customer number*/
	public static final String MEASURE_S_CN = "System Number of Customers";
	/**Code for average drop rate*/
	public static final String MEASURE_DR = "Drop Rate";
	/**Code for average system drop rate*/
	public static final String MEASURE_S_DR = "System Drop Rate";
	//Added by ASHANKA START
	//Added as a part of process 
	//of including the system power performance index to JSIM
	public static final String MEASURE_S_SP = "System Power";
	
	public static final String MEASURE_X_PER_SINK = "Throughput per Sink";
	public static final String MEASURE_R_PER_SINK = "Response Time per Sink";

	//Added by ASHANKA STOP

	/*------------------------------------------------------------------------------------------
	------------------------------Parameters for measures definition----------------------------
	--------------------------------------------------------------------------------------------*/

	/**Adds a new measure for specified class and station (or region)
	 * @param type: code for measure type.
	 * @param stationKey: specified station (or region) for measure
	 * @param classKey: specified class
	 * @return search key for new measure
	 * */
	public Object addMeasure(String type, Object stationKey, Object classKey);

	/**Adds a new measure for specified class and station
	 * @param type: code for measure type.
	 * @param stationKey: specified station for measure
	 * @param classKey: specified class
	 * @param alpha: alpha parameter for this measure
	 * @param precision: precision parameter for this measure
	 * @return search key for new measure
	 * */
	public Object addMeasure(String type, Object stationKey, Object classKey, Double alpha, Double precision);

	/**Removes a mesure from list, given its search key
	 * @param measureKey: search Key for measure to be removed
	 */
	public void removeMeasure(Object measureKey);

	/**Returns list of measure search keys
	 * @return Vector containing all of the measure search keys
	 */
	public Vector<Object> getMeasureKeys();

	/** Returns type of measure, given measure searchkey.
	 * @param measureKey: search key
	 * @return measure type
	 */
	public String getMeasureType(Object measureKey);

	/**sets type of parameter to be measured
	 * @param newType new type for this measure
	 * @param measureKey search key for this measure
	 */
	public void setMeasureType(String newType, Object measureKey);

	/**Returns search key for class this measure refers to.
	 * @param measureKey search key for measure
	 * @return search key for class
	 */
	public Object getMeasureClass(Object measureKey);

	/**Changes reference class for specified measure, given its search key
	 * @param classKey new class search key
	 * @param measureKey measure search key
	 */
	public void setMeasureClass(Object classKey, Object measureKey);

	/**Returns search key for station a certain measure refers to.
	 * @param measureKey search key for measure
	 * @return search key for station.
	 */
	public Object getMeasureStation(Object measureKey);

	/**Changes reference station for specified measure, given its search key
	 * @param stationKey: search key for station this mesure must be referred to.
	 * @param measureKey: search key for measure.
	 */
	public void setMeasureStation(Object stationKey, Object measureKey);

	/**returns value for alpha parameter of a specific measure, given its search key.
	 * @param measureKey search keyt for station
	 * @return value of alpha parameter
	 */
	public Double getMeasureAlpha(Object measureKey);

	/**Sets value for alpha parameter of a specific measure, given its search key.
	 * @param measureKey search keyt for station
	 * @param alpha value of alpha parameter
	 */
	public void setMeasureAlpha(Double alpha, Object measureKey);

	/**returns value for precision parameter of a specific measure, given its search key.
	 * @param measureKey search keyt for station
	 * @return value of precision parameter
	 */
	public Double getMeasurePrecision(Object measureKey);

	/**Sets value for alpha parameter of a specific measure, given its search key.
	 * @param measureKey search keyt for station
	 * @param precision value of alpha parameter
	 */
	public void setMeasurePrecision(Double precision, Object measureKey);

	/**
	* Tells if a given measure is global or local (i.e. if it's station independent
	* or station dependent)
	* <br>Author: Bertoli Marco
	* @param key search's key for given measure
	* @return true if measure is global
	*/
	public boolean isGlobalMeasure(Object key);
	
	/**
	 * Tells if the Measure is being calculated keeping the Sink as reference,
	 * Is used for the performance indices of
	 * ResponseTimePerSink and ThroughputPerSink.
	 * @param key
	 * @return
	 */
	public boolean isSinkMeasure(Object key);

	/*------------------------------------------------------------------------------------------
	------------------------------ Methods for preloading definition ---------------------------
	--------------------------------------------------------------------------------------------*/

	/**Sets number of jobs to be preloaded at specified station for specified class.
	 * For closed classes, sum of number of preloaded jobs for a class above all stations
	 * must be equal to class population, otherwise update is canceled.
	 * @param jobs number of jobs to be preloaded
	 * @param stationKey search key for station jobs must preloaded into.
	 * @param classKey search key for class of jobs that must be preloaded.
	 */
	public void setPreloadedJobs(Integer jobs, Object stationKey, Object classKey);

	/**Returns number of jobs set to be preloaded in specified station for specified class.
	 * @param stationKey: station into which jobs must be preloaded.
	 * @param classKey: class jobs to be preloaded belong to.
	 * @return number of jobs to be preloaded.
	 */
	public Integer getPreloadedJobs(Object stationKey, Object classKey);

	/**
	 * Returns number of jobs totally allocated for specified class.
	 * @param classKey class jobs to be preloaded belong to.
	 * @return umber of jobs totally to be preloaded.
	 */
	public Integer getPreloadedJobsNumber(Object classKey);

	/**
	 * This method is used to manage number of jobs for every class. If class is closed
	 * all spare jobs will be allocated to its reference source, if for some reasons more
	 * jobs are allocated than max population, they are reduced. Uses this method only
	 * when strictly necessary as is can be slow if the model is big.
	 */
	public void manageJobs();

	/*------------------------------------------------------------------------------------------
	---------------------------- Parameters for simulation definition --------------------------
	--------------------------------------------------------------------------------------------*/
	/**Sets seed for simulation.
	 * @param seed seed of simulation to be started.
	 */
	public void setSimulationSeed(Long seed);

	/**Gets seed of simulation seed.
	 * @return seed for simulation.
	 */
	public Long getSimulationSeed();

	/**
	 * Tells if random seed must be used for simulation
	 * @return true iff random seed have to be used
	 */
	public boolean getUseRandomSeed();

	/**
	 * Sets if random simulation seed must be used (default: true)
	 * @param value truth value
	 */
	public void setUseRandomSeed(boolean value);

	/**Sets maximum duration for simulation in milliseconds.
	 * @param durationSeconds seconds of duration of simulation
	 */
	public void setMaximumDuration(Double durationSeconds);

	/**Returns maximum duration for simulation, expressed in milliseconds.
	 * @return milliseconds of maximum duration for simulation.
	 */
	public Double getMaximumDuration();

	/**
	 * Gets polling interval for temporary measures
	 * @return polling interval for temporary measures
	 */
	public double getPollingInterval();

	/**
	 * Sets polling interval for temporary measures
	 * @param pollingInterval polling interval for temporary measures
	 */
	public void setPollingInterval(double pollingInterval);

	/**
	 * Sets maximum number of simulation samples
	 * @param maxSamples maximum number of simulation samples
	 */
	public void setMaxSimulationSamples(Integer maxSamples);

	/**
	 * Returns maximum number of simulation samples
	 * @return maximum number of simulation samples
	 */
	public Integer getMaxSimulationSamples();

	/**
	 * Tells if statistic check was disabled as simulation stopping criteria
	 * @return the disableStatistic
	 */
	public Boolean getDisableStatistic();

	/**
	 * Sets if statistic check was disabled as simulation stopping criteria
	 * @param disableStatistic the disableStatistic to set
	 */
	public void setDisableStatistic(Boolean disableStatistic);

	// --- Methods to manage simulation results -- Bertoli Marco --------------------------------------------
	/**
	 * Returns last simulation results
	 * @return simulation results or null if no simulation was performed
	 */
	public MeasureDefinition getSimulationResults();

	/**
	 * Sets simulation results
	 * @param results simulation results data structure
	 */
	public void setSimulationResults(MeasureDefinition results);

	/**
	 * Tells if current model contains simulation results
	 * @return true iff <code>getSimulationResults()</code> returns a non-null object
	 */
	public boolean containsSimulationResults();

	// ------------------------------------------------------------------------------------------------------

	// -------------------- Francesco D'Aquino -----------------------------------

	/**
	 * Return true if queue animation is enabled
	 * @return true if the animation is enabled
	 */
	public boolean isAnimationEnabled();

	/**
	 * Enable or disable queue animation
	 * @param isEnabled - set it to true to enable queue animation
	 */
	public void setAnimationEnabled(boolean isEnabled);

	/**
	 * Checks if the parametric analysis has been enabled
	 * @return true if the parametric analysis has been enabled
	 */
	public boolean isParametricAnalysisEnabled();

	/**
	 * Enable / disable parametric analysis
	 * @param enabled
	 */
	public void setParametricAnalysisEnabled(boolean enabled);

	/**
	 * Gets the parametricAnalysisModel if parametric analysis was enabled,
	 * null otherwise
	 * @return the parametricAnalysisModel if parametric analysis was enabled,
	 *         null otherwise
	 */
	public ParametricAnalysisDefinition getParametricAnalysisModel();

	/**
	 * Sets the parametric analysis definition
	 * @param pad the parametric analysis definition to be set
	 */
	public void setParametricAnalysisModel(ParametricAnalysisDefinition pad);

	// -------------------- end Francesco D'Aquino --------------------------------

	/**
	 * Tells model that some data has been changed and need to be saved. This
	 * is used by Parametric Analysis
	 */
	public void setSaveChanged();

}
