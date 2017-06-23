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

package jmt.gui.exact;

import java.util.Arrays;
import java.util.HashSet;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import jmt.framework.data.ArrayUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * An object grouping all the data describing a system.
 * <br><br>
 * WARNING:
 * ACCESS TO THIS OBJECT MUST BE IMPLEMENTED IN A TRANSACTION-LIKE SYNCHRONIZED WAY!
 *
 * @author alyf (Andrea Conti), Stefano Omini, Bertoli Marco
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of QueueLength to Customer Number
 * @version Date: Aug-2009
 * 
 * @author Ashanka
 * Cleaned code by removing unnecesary comments of old code.
 * @version Date: Sep-2009
 * 
 * @author Ashanka
 * Added a backward compatible clause in loadResultsMatrix to open existing JMVA models correctly.
 * If we are looking to open measures of Number of Customers from existing JMVA files then we should look for 
 * Queue length as this was the previous name of the label. 
 * @version Date: Sep-2009
 */
public class ExactModel implements ExactConstants {

	//true if the model is closed
	private boolean closed;
	//true if the model is open
	private boolean open;
	//true if the model contains load dependent stations
	private boolean ld;
	//true if visits are set (otherwise they will be all unitary)
	private boolean unitaryVisits;

	//true if the model has been modified
	private boolean changed;
	//true if results are available
	private boolean hasResults;
	//true if the results are valid (no modify has been made in the model after results computation)
	private boolean resultsOK;
	//description of the model
	private String description;

	/***********************STATIONS AND CLASSES******************************/

	//number of service centers
	private int stations;
	//number of classes
	private int classes;
	//total population (computed as the sum of all closed class populations)
	private int maxpop;

	//class data is class population for closed classes, class arrival rate for open classes
	//dim: classData[classes]
	private double[] classData;
	//station names
	//dim: stationNames[stations]
	private String[] stationNames;
	//station types
	//dim: stationTypes[stations]
	private int[] stationTypes;
	//number of servers of each station
	//dim: stationServers[stations]
	private int[] stationServers;
	//class names
	//dim: classNames[classes]
	private String[] classNames;
	//class types
	//dim: classTypes[classes]
	private int[] classTypes;

	/***********************SERVICE PARAMETERS**************************/

	/**
	 * visits to the service centers
	 * dim: visits[stations][classes]
	 */
	private double[][] visits;
	/**
	 * service times of the service centers
	 * dim: serviceTimes[stations][classes][p]
	 * p=maxpop     if stationTypes[s]==STATION_LD
	 * p=1          otherwise
	 */
	private double[][][] serviceTimes;

	/***********************RESULTS******************************/

	/**
	 * queue lengths
	 * dim: queueLen[stations][classes][iterations]
	 */
	private double[][][] queueLen;

	/**
	 * throughput
	 * dim: throughput[stations][classes][iterations]
	 */
	private double[][][] throughput;

	/**
	 * residence times
	 * dim: resTime[station][classes][iterations]
	 */
	private double[][][] resTimes;

	/**
	 * utilization
	 * dim: util[stations][classes][iterations]
	 */
	private double[][][] util;

	/*****************************************************************/
	//parameters for randomization
	private static final double MAXRAND = 100;
	private static final double MAXRANGE = 10;

	/*****************************************************************/

	/********************** WHAT-IF ANALYSIS *** Bertoli Marco *******/
	/** Number of iterations (1 for normal usage, >1 for what-if analysis) */
	private int iterations = 1;
	/** Index of class selected for what-if analysis. -1 means all classes */
	private int whatIfClass = -1;
	/** Index of station selected for what-if analysis. -1 means all stations */
	private int whatIfStation = -1;
	/**
	 * Type of what-if analysis
	 * @see ExactConstants
	 */
	private String whatIfType;
	/** Array with considered values */
	private double[] whatIfValues;

	/*****************************************************************/

	/**
	 * make an object with default values
	 */
	public ExactModel() {
		setDefaults();
	}

	/**
	 * copy constructor
	 */
	public ExactModel(ExactModel e) {
		closed = e.closed;
		open = e.open;
		unitaryVisits = e.unitaryVisits;
		hasResults = e.hasResults;
		resultsOK = e.resultsOK;
		changed = e.changed;

		stations = e.stations;
		classes = e.classes;
		maxpop = e.maxpop;

		description = e.description;

		stationNames = ArrayUtils.copy(e.stationNames);
		stationTypes = ArrayUtils.copy(e.stationTypes);
		stationServers = ArrayUtils.copy(e.stationServers);

		classNames = ArrayUtils.copy(e.classNames);
		classTypes = ArrayUtils.copy(e.classTypes);
		classData = ArrayUtils.copy(e.classData);

		visits = ArrayUtils.copy2(e.visits);

		serviceTimes = ArrayUtils.copy3(e.serviceTimes);

		// What-if analysis
		iterations = e.iterations;
		whatIfClass = e.whatIfClass;
		whatIfStation = e.whatIfStation;
		whatIfType = e.whatIfType;
		whatIfValues = e.whatIfValues;

		if (hasResults) {
			queueLen = ArrayUtils.copy3(e.queueLen);
			throughput = ArrayUtils.copy3(e.throughput);
			resTimes = ArrayUtils.copy3(e.resTimes);
			util = ArrayUtils.copy3(e.util);
		}
	}

	/**
	 * Clears all the results
	 */
	public void discardResults() {
		queueLen = null;
		throughput = null;
		resTimes = null;
		util = null;
		discardChanges();
	}

	/**
	 * Discards all change elements but does not touch results
	 */
	public void discardChanges() {
		hasResults = false;
		resultsOK = false;
		changed = true;
	}

	/**
	 * sets all the result data for this model.
	 * @throws IllegalArgumentException if any argument is null or not of the correct size
	 */
	public void setResults(double[][][] queueLen, double[][][] throughput, double[][][] resTimes, double[][][] util) {
		if (queueLen == null || queueLen.length != stations || queueLen[0].length != classes) {
			throw new IllegalArgumentException("queueLen must be non null and of size [stations][classes][iterations]");
		}
		if (throughput == null || throughput.length != stations || throughput[0].length != classes) {
			throw new IllegalArgumentException("throughput must be non null and of size [stations][classes][iterations]");
		}
		if (resTimes == null || resTimes.length != stations || resTimes[0].length != classes) {
			throw new IllegalArgumentException("resTimes must be non null and of size [stations][classes][iterations]");
		}
		if (util == null || util.length != stations || util[0].length != classes) {
			throw new IllegalArgumentException("util must be non null and of size [stations][classes][iterations]");
		}
		// non controlla il numero di classi per tutte le stazioni, ma solo per la prima!!
		this.queueLen = ArrayUtils.copy3(queueLen);
		this.throughput = ArrayUtils.copy3(throughput);
		this.resTimes = ArrayUtils.copy3(resTimes);
		this.util = ArrayUtils.copy3(util);
		hasResults = true;
		iterations = queueLen[0][0].length;
		resultsOK = true;
		changed = true;
	}

	/**
	 * sets all the result data for this model. This is called when only one iteration is performed.
	 * @throws IllegalArgumentException if any argument is null or not of the correct size
	 */
	public void setResults(double[][] queueLen, double[][] throughput, double[][] resTimes, double[][] util) {
		resetResults();
		setResults(queueLen, throughput, resTimes, util, 0);
	}

	/**
	 * Sets ResultsOK flag
	 * @param value value of ResultsOK flag
	 */
	public void setResultsOK(boolean value) {
		this.resultsOK = value;
	}

	/**
	 * sets all the result data for this model. This is called on multiple iterations (what-if analysis)
	 * @throws IllegalArgumentException if any argument is null or not of the correct size
	 */
	public void setResults(double[][] queueLen, double[][] throughput, double[][] resTimes, double[][] util, int iteration) {
		if (queueLen == null || queueLen.length != stations || queueLen[0].length != classes) {
			throw new IllegalArgumentException("queueLen must be non null and of size [stations][classes]");
		}
		if (throughput == null || throughput.length != stations || throughput[0].length != classes) {
			throw new IllegalArgumentException("throughput must be non null and of size [stations][classes]");
		}
		if (resTimes == null || resTimes.length != stations || resTimes[0].length != classes) {
			throw new IllegalArgumentException("resTimes must be non null and of size [stations][classes]");
		}
		if (util == null || util.length != stations || util[0].length != classes) {
			throw new IllegalArgumentException("util must be non null and of size [stations][classes]");
		}
		if (iteration >= iterations) {
			throw new IllegalArgumentException("iteration is greater than expected number of iterations");
		}
		// Creates an array for single iteration
		ArrayUtils.copy2to3(queueLen, this.queueLen, iteration);
		ArrayUtils.copy2to3(throughput, this.throughput, iteration);
		ArrayUtils.copy2to3(resTimes, this.resTimes, iteration);
		ArrayUtils.copy2to3(util, this.util, iteration);
		hasResults = true;
		resultsOK = true;
		changed = true;
	}

	/**
	 * Resets arrays used to store results
	 */
	public void resetResults() {
		queueLen = new double[stations][classes][iterations];
		throughput = new double[stations][classes][iterations];
		resTimes = new double[stations][classes][iterations];
		util = new double[stations][classes][iterations];

		hasResults = false;
		changed = true;
	}

	/**
	 * Initialize the object with defaults:
	 * 1 closed class, 1 LI station, 0 customers, all visits to one, all service times to zero, no results
	 */
	public void setDefaults() {
		closed = true;
		hasResults = false;
		resultsOK = false;
		stations = 1;
		classes = 1;

		// perch� � 1 se non ci sono customers?? Lasciare cos�
		maxpop = 1;
		changed = true;

		classData = new double[1];
		//NEW
		//@author Stefano Omini
		classData[0] = 1;
		//end NEW

		stationNames = new String[1];
		stationNames[0] = "Station1";

		stationTypes = new int[1];
		stationTypes[0] = STATION_LI;

		stationServers = new int[1];
		stationServers[0] = 1;

		classNames = new String[1];
		classNames[0] = "Class1";

		classTypes = new int[1];
		classTypes[0] = CLASS_CLOSED;

		visits = new double[1][1];
		visits[0][0] = 1.0;

		serviceTimes = new double[1][1][1];
		//NEW
		//@author Stefano Omini
		serviceTimes[0][0][0] = 0.0;
		//end NEW

		description = "";
	}

	/**
	 * Gets the model description
	 * @return the model description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the model description
	 * @param description the model description
	 * @return true if data was changed, false otherwise
	 */
	public boolean setDescription(String description) {
		if (description.equals(this.description)) {
			return false;
		}
		this.description = description;
		changed = true;
		return true;
	}

	/**
	 * @return true if this object describes a multiclass system
	 */
	public boolean isMultiClass() {
		return (classes > 1);
	}

	/**
	 * @return true if this object describes a closed system
	 */
	public boolean isClosed() {
		return closed;
	}

	/**
	 * @return true if this object describes an open system
	 */
	public boolean isOpen() {
		return open;
	}

	/**
	 * @return true if this object describes a mixed system
	 */
	public boolean isMixed() {
		//mixed = true only if closed = false and open = false
		return !(closed || open);
	}

	/**
	 * @return true if this object describes a system containing LD stations
	 */
	public boolean isLd() {
		return ld;
	}

	/**
	 * @return number of service centers
	 */
	public int getStations() {
		return stations;
	}

	/**
	 * @return number of classes
	 */
	public int getClasses() {
		return classes;
	}

	/**
	 * @return total population
	 */
	public int getMaxpop() {
		return maxpop;
	}

	/**
	 * @return names of the service centers
	 */
	public String[] getStationNames() {
		return stationNames;
	}

	/**
	 * @return the number of servers for each station. For delay stations this parameter is unsensed.
	 */
	public int[] getStationServers() {
		return stationServers;
	}

	/**
	 * @return true if this model contains multiple server for a station.
	 */
	public boolean isMultipleServers() {
		for (int i = 0; i < stations; i++) {
			if (stationServers[i] > 1 && stationTypes[i] != ExactConstants.STATION_DELAY) {
				return true;
			}
		}
		return false;
	}

	/**
	 * sets the number of servers for each station
	 * @param classNames the number of servers of each station
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setStationServers(int[] stationServers) {
		if (stationServers.length != stations) {
			throw new IllegalArgumentException("stationServers.length != stations");
		}
		if (Arrays.equals(this.stationServers, stationServers)) {
			return false;
		}

		this.stationServers = stationServers;
		changed = true;
		return true;
	}

	/**
	 * sets the names of the service centers.
	 * @param stationNames the names of the service centers
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setStationNames(String[] stationNames) {
		if (stationNames.length != stations) {
			throw new IllegalArgumentException("stationNames.length!=stations");
		}
		if (!changed) {
			if (Arrays.equals(this.stationNames, stationNames)) {
				return false;
			}
		}
		this.stationNames = stationNames;
		changed = true;
		return true;
	}

	/**
	 * @return names of the classes
	 */
	public String[] getClassNames() {
		return classNames;
	}

	/**
	 * sets the names of the classes.
	 * @param classNames the names of the classes
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setClassNames(String[] classNames) {
		if (classNames.length != classes) {
			throw new IllegalArgumentException("classNames.length!=classes");
		}
		if (Arrays.equals(this.classNames, classNames)) {
			return false;
		}

		this.classNames = classNames;
		changed = true;
		return true;
	}

	/**
	 * @return data for the classes.
	 */
	public double[] getClassData() {
		return classData;
	}

	/**
	 * sets the data for the classes
	 * @param classData the data for the classes
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setClassData(double[] classData) {
		if (classData.length != classes) {
			throw new IllegalArgumentException("classData.length!=classes");
		}
		if (Arrays.equals(this.classData, classData)) {
			return false;
		}

		this.classData = classData;
		changed = true;
		resultsOK = false;

		// make sure 3rd dimension of serviceTimes is ok
		resize(stations, classes);
		return true;
	}

	/**
	 * @return type of the classes
	 */
	public int[] getClassTypes() {
		return classTypes;
	}

	/**
	 * sets the type of the classes
	 * @param classTypes the type of the classes
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setClassTypes(int[] classTypes) {
		if (classTypes.length != classes) {
			throw new IllegalArgumentException("classTypes.length!=classes");
		}
		if (Arrays.equals(this.classTypes, classTypes)) {
			return false;
		}

		this.classTypes = classTypes;
		closed = calcClosed();
		open = calcOpen();
		changed = true;
		resultsOK = false;
		return true;
	}

	/**
	 * @return type of the stations
	 */
	public int[] getStationTypes() {
		return stationTypes;
	}

	/**
	 * sets the type of the stations
	 * @param stationTypes the type of the stations
	 * @throws IllegalArgumentException if the array is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setStationTypes(int[] stationTypes) {
		if (stationTypes.length != stations) {
			throw new IllegalArgumentException("stationTypes.length!=stations");
		}
		if (Arrays.equals(this.stationTypes, stationTypes)) {
			return false;
		}

		this.stationTypes = stationTypes;
		// adjusts serviceTimes size and recalculates flags
		resize(stations, classes, true);
		changed = true;
		resultsOK = false;
		return true;
	}

	/**
	 * @return the matrix of visits
	 */
	public double[][] getVisits() {
		return visits;
	}

	/**
	 * sets the matrix of visits
	 * @param visits the matrix of visits
	 * @throws IllegalArgumentException if the matrix is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setVisits(double[][] visits) {
		if (visits.length != stations || visits[0].length != classes) {
			throw new IllegalArgumentException("incorrect array dimension");
		}
		if (ArrayUtils.equals2(this.visits, visits)) {
			return false;
		}

		this.visits = visits;
		changed = true;
		resultsOK = false;

		// Checks if visits are all one
		calcUnitaryVisits();
		return true;

	}

	/**
	 * @return the matrix of service times
	 */
	public double[][][] getServiceTimes() {
		return serviceTimes;
	}

	/**
	 * sets the matrix of service times
	 * @param serviceTimes the matrix of service times
	 * @throws IllegalArgumentException if the matrix is not of the correct size
	 * @return true if data was changed, false otherwise
	 */
	public boolean setServiceTimes(double[][][] serviceTimes) {
		if (serviceTimes.length != stations || serviceTimes[0].length != classes) {
			throw new IllegalArgumentException("incorrect array dimension");
		}
		if (ArrayUtils.equals3(this.serviceTimes, serviceTimes)) {
			return false;
		}

		int currSize;
		double[][] subST;

		//validate sizes
		for (int s = 0; s < stations; s++) {
			currSize = (stationTypes[s] == STATION_LD ? maxpop : 1);
			// if a station is LD but customer number is 0, maxpop = 0
			if (currSize == 0) {
				currSize = 1;
			}
			subST = serviceTimes[s];
			for (int c = 0; c < classes; c++) {
				if (subST[c].length != currSize) {
					throw new IllegalArgumentException("Wrong size for station " + stationNames[s]);
				}
			}
		}

		this.serviceTimes = serviceTimes;
		changed = true;
		resultsOK = false;
		return true;
	}

	/**
	 * Resizes the data structures according to specified parameters. Data is preserved as far as possible
	 * @return true if data was changed, false otherwise
	 */
	public boolean resize(int stations, int classes, boolean forceResize) {
		if (stations <= 0 || classes <= 0) {
			throw new IllegalArgumentException("stations and classes must be >0");
		}
		if (forceResize || this.stations != stations || this.classes != classes) {
			//other cases already handled in setXXX methods
			discardResults();

			this.stations = stations;
			this.classes = classes;

			stationNames = ArrayUtils.resize(stationNames, stations, null);
			stationTypes = ArrayUtils.resize(stationTypes, stations, STATION_LI);
			stationServers = ArrayUtils.resize(stationServers, stations, 1);
			ld = calcLD();

			visits = ArrayUtils.resize2(visits, stations, classes, 1.0);

			classNames = ArrayUtils.resize(classNames, classes, null);
			classTypes = ArrayUtils.resize(classTypes, classes, CLASS_CLOSED);
			closed = calcClosed();

			classData = ArrayUtils.resize(classData, classes, 0.0);

			maxpop = calcMaxpop();

			serviceTimes = ArrayUtils.resize3var(serviceTimes, stations, classes, calcSizes(), 0.0);
			// Checks if visits are all one
			calcUnitaryVisits();

			return true;
		} else {
			// Check if population was changed.
			int newMaxpop = calcMaxpop();
			if (newMaxpop != maxpop) {
				maxpop = newMaxpop;
				serviceTimes = ArrayUtils.resize3var(serviceTimes, stations, classes, calcSizes(), 0.0);
				return true;
			}
		}
		return false;
	}

	/**
	 * Resizes the data structures according to specified parameters. Data is preserved as far as possible
	 * @return true if data was changed, false otherwise
	 */
	public boolean resize(int stations, int classes) {
		return resize(stations, classes, false);
	}

	/**
	 * @return queue lengths
	 */
	public double[][][] getQueueLen() {
		return queueLen;
	}

	/**
	 * @return residence times
	 */
	public double[][][] getResTimes() {
		return resTimes;
	}

	/**
	* @return throughputs
	*/
	public double[][][] getThroughput() {
		return throughput;
	}

	/**
	 * @return utilizations
	 */
	public double[][][] getUtilization() {
		return util;
	}

	/**
	 * Removes all LD stations, converting them into LI stations
	 */
	public void removeLD() {
		for (int i = 0; i < stations; i++) {
			if (stationTypes[i] == STATION_LD) {
				stationTypes[i] = STATION_LI;

				//NEW
				//@author Stefano Omini
				//clear old LD service times
				serviceTimes[i] = new double[classes][1];
				for (int c = 0; c < classes; c++) {
					serviceTimes[i][c][0] = 0.0;
				}
				//end NEW
			}
		}
		ld = false;
	}

	/**
	 * This method will find if current visits matrix is unitary or not.
	 * If a value is 0 will check service demand. This is used to show correct
	 * panel layout upon loading of a model
	 */
	private void calcUnitaryVisits() {
		double epsilon = 1e-14;
		for (int i = 0; i < stations; i++) {
			for (int j = 0; j < classes; j++) {
				if ((Math.abs(visits[i][j]) < epsilon && Math.abs(serviceTimes[i][j][0]) > epsilon)
						|| (!(Math.abs(visits[i][j]) < epsilon) && Math.abs(visits[i][j] - 1.0) > epsilon)) {
					unitaryVisits = true;
					return;
				}
			}
		}
		unitaryVisits = false;
	}

	/**
	 * @return true if the model contains only closed stations
	 */
	private boolean calcClosed() {
		for (int i = 0; i < classes; i++) {
			if (classTypes[i] != CLASS_CLOSED) {
				//make sure we stay in a consistent state
				removeLD();
				//Removes LD as multiclass LD is not supported
				return false;
			}
		}
		return true;
	}

	/**
	 * @return true if the model contains only open stations
	 */
	private boolean calcOpen() {
		for (int i = 0; i < classes; i++) {
			if (classTypes[i] != CLASS_OPEN) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @return true if the model contains Load Dependent stations
	 */
	private boolean calcLD() {
		for (int i = 0; i < stations; i++) {
			if (stationTypes[i] == STATION_LD) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @return the total population (sum of the customers of all closed class)
	 */
	private int calcMaxpop() {
		/* sum all the closed classes' customers */
		int maxpop = 0;
		for (int i = 0; i < classes; i++) {
			if (classTypes[i] == CLASS_CLOSED) {
				maxpop += classData[i];
			}
		}
		return maxpop;
	}

	/**
	 * @return the sizes of service times for each station (max pop for LD stations, 1 for LI stations)
	 */
	private int[] calcSizes() {
		int mp = (maxpop > 0 ? maxpop : 1);
		int[] sizes = new int[stations];
		for (int s = 0; s < stations; s++) {
			sizes[s] = (stationTypes[s] == STATION_LD ? mp : 1);
		}
		return sizes;
	}

	/**
	 * Warning: Calling this on large systems *will* result in OutOfMemory errors. You have been warned.
	 * @return a String representation of the parameters of this object
	 */
	public String toString() {
		StringBuffer s = new StringBuffer();
		s.append("stations=").append(stations).append(" classes=").append(classes).append(" pop=").append(maxpop).append(" changed=").append(changed)
				.append(" ld=").append(ld).append(" open=").append(open).append(" closed=").append(closed).append(" hasResults=").append(hasResults)
				.append(" resultsOK=").append(resultsOK).append("\n").append("stationNames=").append(ArrayUtils.toString(stationNames)).append("\n")
				.append("stationTypes=").append(ArrayUtils.toString(stationTypes)).append("\n").append("classNames=").append(
						ArrayUtils.toString(classNames)).append("\n").append("classTypes=").append(ArrayUtils.toString(classTypes)).append("\n")
				.append("classData=").append(ArrayUtils.toString(classData)).append("\n").append("visits=").append(ArrayUtils.toString2(visits))
				.append("\n").append("serviceTimes=").append(ArrayUtils.toString3(serviceTimes)).append("\n");
		if (hasResults) {
			s.append("number of customers=").append(ArrayUtils.toString3(queueLen)).append("\n").append("throughput=").append(
					ArrayUtils.toString3(throughput)).append("\n").append("resTimes=").append(ArrayUtils.toString3(resTimes)).append("\n").append(
					"utilization=").append(ArrayUtils.toString3(util)).append("\n");
		}

		return s.toString();
	}

	/**
	 * Deletes a class
	 * @param i the index of the class
	 */
	public void deleteClass(int i) {

		if (classes < 2) {
			throw new RuntimeException("System must have at least one class");
		}

		classes--;

		classNames = ArrayUtils.delete(classNames, i);
		classTypes = ArrayUtils.delete(classTypes, i);
		classData = ArrayUtils.delete(classData, i);

		visits = ArrayUtils.delete2_2(visits, i);
		serviceTimes = ArrayUtils.delete3_2(serviceTimes, i);

		resize(stations, classes);
		//DEK (Federico Granata) 3-10-2003
		//it was considering the results valid when a class is cancelled
		hasResults = false;
		//END
	}

	/**
	 * Deletes a station
	 * @param i the index of the station
	 */
	public void deleteStation(int i) {
		if (stations < 2) {
			throw new RuntimeException("System must have at least one station");
		}
		stations--;
		stationNames = ArrayUtils.delete(stationNames, i);
		stationTypes = ArrayUtils.delete(stationTypes, i);
		stationServers = ArrayUtils.delete(stationServers, i);

		visits = ArrayUtils.delete2_1(visits, i);
		serviceTimes = ArrayUtils.delete3_1(serviceTimes, i);

		resize(stations, classes);
		//DEK (Federico Granata) 3-10-2003
		//it was considering the results valid when a class is cancelled
		hasResults = false;
		//END
	}

	/**
	 * @return true if the model has been changed
	 */
	public boolean isChanged() {
		return changed;
	}

	/**
	 * resets the changed flag.
	 * <br>
	 * WARNING: this enables change checking on parameter setting, which can be quite time-consuming.
	 */
	public void resetChanged() {
		changed = false;
	}

	/**
	 * flags the model as changed.
	 * There is no need to call this, except to disable time-consuming change checking if you're not interested in it
	 */
	public void setChanged() {
		changed = true;
	}

	/**
	 * @return true if results are available
	 */
	public boolean hasResults() {
		return hasResults;
	}

	/**
	 * @return true if results are valid
	 */
	public boolean areResultsOK() {
		return resultsOK;
	}

	/**
	 * Creates a DOM representation of this object
	 * @return a DOM representation of this object
	 */
	public Document createDocument() {
		Document root;
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			root = dbf.newDocumentBuilder().newDocument();
		} catch (ParserConfigurationException pce) {
			throw new RuntimeException(pce);
		}

		/* model */
		Element modelElement = root.createElement("model");

		modelElement.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		modelElement.setAttribute("xsi:noNamespaceSchemaLocation", "JMTmodel.xsd");

		root.appendChild(modelElement);

		/* description */
		if (!description.equals("")) {
			Element descriptionElement = root.createElement("description");
			descriptionElement.appendChild(root.createCDATASection(description));
			modelElement.appendChild(descriptionElement);
		}

		/* parameters */
		Element parametersElement = root.createElement("parameters");
		modelElement.appendChild(parametersElement);

		/* classes */
		Element classes_element = root.createElement("classes");
		parametersElement.appendChild(classes_element);
		classes_element.setAttribute("number", Integer.toString(classes));
		for (int i = 0; i < classes; i++) {
			classes_element.appendChild(makeClassElement(root, i));
		}

		/* stations */
		Element stationsElement = root.createElement("stations");
		parametersElement.appendChild(stationsElement);
		stationsElement.setAttribute("number", Integer.toString(stations));
		for (int i = 0; i < stations; i++) {
			stationsElement.appendChild(makeStationElement(root, i));
		}

		/* What-if Analysis - Bertoli Marco */
		if (isWhatIf()) {
			Element whatIf = root.createElement("whatIf");
			modelElement.appendChild(whatIf);
			whatIf.setAttribute("type", whatIfType);
			whatIf.setAttribute("values", ArrayUtils.toCSV(whatIfValues));
			// Class name
			if (whatIfClass >= 0) {
				whatIf.setAttribute("className", classNames[whatIfClass]);
			}
			// Station name
			if (whatIfStation >= 0) {
				whatIf.setAttribute("stationName", stationNames[whatIfStation]);
			}
		}

		//OLD
		//if (hasResults) modelElement.appendChild(appendSolutionElement(root));

		//NEW
		//@author Stefano Omini
		if (hasResults && resultsOK) {
			appendSolutionElement(root, modelElement);
			//end NEW
		}

		return root;

	}

	private Element makeClassElement(Document root, int classNum) {
		Element classElement;
		if (classTypes[classNum] == CLASS_CLOSED) {
			classElement = root.createElement("closedclass");
			classElement.setAttribute("population", Integer.toString((int) classData[classNum]));
			classElement.setAttribute("name", classNames[classNum]);

		} else {
			classElement = root.createElement("openclass");
			classElement.setAttribute("rate", Double.toString(classData[classNum]));
			classElement.setAttribute("name", classNames[classNum]);

		}
		return classElement;
	}

	private Element makeStationElement(Document root, int stationNum) {

		Element station_element;
		Node servicetimes_element;
		Node visits_element;

		switch (this.stationTypes[stationNum]) {

			case STATION_LI:

				station_element = root.createElement("listation");
				station_element.setAttribute("name", this.stationNames[stationNum]);
				station_element.setAttribute("servers", String.valueOf(this.stationServers[stationNum]));

				/* create the section for service times */
				servicetimes_element = station_element.appendChild(root.createElement("servicetimes"));
				station_element.appendChild(servicetimes_element);

				/* create the section for visits */
				visits_element = station_element.appendChild(root.createElement("visits"));
				station_element.appendChild(visits_element);

				/* for each customer class */
				for (int j = 0; j < classes; j++) {
					String class_name = this.classNames[j];
					/* set service time */
					Element st_element = root.createElement("servicetime");
					st_element.setAttribute("customerclass", class_name);
					st_element.appendChild(root.createTextNode(Double.toString(this.serviceTimes[stationNum][j][0])));
					servicetimes_element.appendChild(st_element);
					/* set visit */
					Element visit_element = root.createElement("visit");
					visit_element.setAttribute("customerclass", class_name);
					visit_element.appendChild(root.createTextNode(Double.toString(this.visits[stationNum][j])));
					visits_element.appendChild(visit_element);
				}

				break;

			case STATION_DELAY:

				station_element = root.createElement("delaystation");
				station_element.setAttribute("name", this.stationNames[stationNum]);

				/* create the section for service times */
				servicetimes_element = station_element.appendChild(root.createElement("servicetimes"));
				station_element.appendChild(servicetimes_element);

				/* create the section for visits */
				visits_element = station_element.appendChild(root.createElement("visits"));
				station_element.appendChild(visits_element);

				/* for each customer class */
				for (int j = 0; j < classes; j++) {
					String class_name = this.classNames[j];
					/* set service time */
					Element st_element = root.createElement("servicetime");
					st_element.setAttribute("customerclass", class_name);
					st_element.appendChild(root.createTextNode(Double.toString(this.serviceTimes[stationNum][j][0])));
					servicetimes_element.appendChild(st_element);
					/* set visit */
					Element visit_element = root.createElement("visit");
					visit_element.setAttribute("customerclass", class_name);
					visit_element.appendChild(root.createTextNode(Double.toString(this.visits[stationNum][j])));
					visits_element.appendChild(visit_element);
				}

				break;

			case STATION_LD:

				station_element = root.createElement("ldstation");
				station_element.setAttribute("name", this.stationNames[stationNum]);
				station_element.setAttribute("servers", String.valueOf(this.stationServers[stationNum]));

				/* create the section for service times */
				servicetimes_element = station_element.appendChild(root.createElement("servicetimes"));
				station_element.appendChild(servicetimes_element);

				/* create the section for visits */
				visits_element = station_element.appendChild(root.createElement("visits"));
				station_element.appendChild(visits_element);

				/* for each customer class */
				for (int j = 0; j < classes; j++) {
					String class_name = this.classNames[j];
					/* set service times, one for each population (values are CSV formatted) */
					Element st_element = root.createElement("servicetimes");
					st_element.setAttribute("customerclass", class_name);

					String serv_t = ArrayUtils.toCSV(serviceTimes[stationNum][j]);

					st_element.appendChild(root.createTextNode(serv_t));

					servicetimes_element.appendChild(st_element);
					/* set visit */
					Element visit_element = root.createElement("visit");
					visit_element.setAttribute("customerclass", class_name);
					visit_element.appendChild(root.createTextNode(Double.toString(this.visits[stationNum][j])));
					visits_element.appendChild(visit_element);
				}

				break;

			default:
				station_element = null;
		}//end switch
		return station_element;
	}

	/**
	 * Appends solution elements to model element
	 * @param root root element of Document
	 * @param parentElement model element where solutions have to be appended
	 * <br>
	 * Author: Bertoli Marco
	 */
	private void appendSolutionElement(Document root, Element parentElement) {
		for (int k = 0; k < iterations; k++) {
			Element result_element = root.createElement("solutions");
			result_element.setAttribute("ok", "true");
			if (!isWhatIf()) {
				result_element.setAttribute("solutionMethod", "analytical");
			} else {
				result_element.setAttribute("solutionMethod", "analytical whatif");
				result_element.setAttribute("iteration", Integer.toString(k));
				result_element.setAttribute("iterationValue", Double.toString(whatIfValues[k]));
			}
			for (int i = 0; i < stations; i++) {
				Element stationresults_element = (Element) result_element.appendChild(root.createElement("stationresults"));
				stationresults_element.setAttribute("station", this.stationNames[i]);
				for (int j = 0; j < classes; j++) {
					Element classesresults_element = (Element) stationresults_element.appendChild(root.createElement("classresults"));
					classesresults_element.setAttribute("customerclass", classNames[j]);

					Element Q_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
					Q_element.setAttribute("measureType", "Number of Customers");
					Q_element.setAttribute("successful", "true");
					Q_element.setAttribute("meanValue", Double.toString(this.queueLen[i][j][k]));

					Element X_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
					X_element.setAttribute("measureType", "Throughput");
					X_element.setAttribute("successful", "true");
					X_element.setAttribute("meanValue", Double.toString(this.throughput[i][j][k]));

					Element R_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
					R_element.setAttribute("measureType", "Residence time");
					R_element.setAttribute("successful", "true");
					R_element.setAttribute("meanValue", Double.toString(this.resTimes[i][j][k]));

					Element U_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
					U_element.setAttribute("measureType", "Utilization");
					U_element.setAttribute("successful", "true");
					U_element.setAttribute("meanValue", Double.toString(this.util[i][j][k]));
				}
			}
			parentElement.appendChild(result_element);
		}
	}

	/* Not used
	private void appendMatrixCSV(Document root, Element base, double[][] arr, String outer, String inner) {
		// forse devo usare questo per trattare anche il caso LD
	    Element elems, elem;
		int n = arr.length;
		elems = root.createElement(outer);
		base.appendChild(elems);
		for (int i = 0; i < n; i++) {
			elem = root.createElement(inner);
			// separa i diversi elementi dell'array con ";"
	        elem.appendChild(root.createTextNode(ArrayUtils.toCSV(arr[i])));
			elems.appendChild(elem);
		}
	}  */

	/**
	 * load the state of this object from the Document.
	 * @return true if the operation was successful.
	 * WARNING: If the operation fails the object is left in an incorrect state and should be discarded.
	 */
	public boolean loadDocument(Document doc) {

		Node classNode = doc.getElementsByTagName("classes").item(0);
		Node stationNode = doc.getElementsByTagName("stations").item(0);

		NodeList descList = doc.getElementsByTagName("description");
		NodeList solList = doc.getElementsByTagName("solutions");

		//load description
		if (descList.item(0) != null) {
			if (!loadDescription((Element) descList.item(0))) {
				//description loading failed!
				return false;
			}
		} else {
			description = "";
		}

		//NEW
		//@author Stefano Omini

		//load classes
		if (classNode != null) {
			if (!loadClasses(classNode)) {
				//classes loading failed!
				return false;
			}
		}

		//load stations
		if (stationNode != null) {
			if (!loadStations(stationNode)) {
				//stations loading failed!
				return false;
			}
		}

		//end NEW

		/* What-if Analysis - Bertoli Marco */
		NodeList whatIfs = doc.getElementsByTagName("whatIf");
		if (whatIfs.getLength() > 0) {
			// What-if analysis was saved
			Element whatIf = (Element) whatIfs.item(0);
			setWhatIfType(whatIf.getAttribute("type"));
			setWhatIfValues(ArrayUtils.fromCSV(whatIf.getAttribute("values")));
			// Try to retrive what-if class informations
			setWhatIfClass(-1);
			String className = whatIf.getAttribute("className");
			if (className != null && !className.equals("")) {
				for (int i = 0; i < classes; i++) {
					if (classNames[i].equals(className)) {
						setWhatIfClass(i);
						break;
					}
				}
			}
			// Try to retrive what-if station informations
			setWhatIfStation(-1);
			String stationName = whatIf.getAttribute("stationName");
			if (stationName != null && !stationName.equals("")) {
				for (int i = 0; i < stations; i++) {
					if (stationNames[i].equals(stationName)) {
						setWhatIfStation(i);
						break;
					}
				}
			}
		} else {
			// What-if analysis was not saved
			iterations = 1;
			setWhatIfClass(-1);
			setWhatIfStation(-1);
		}

		//load solution
		if (solList.getLength() > 0) {

			if (!loadSolution(solList)) {
				return false;
			}
			hasResults = true;

		} else {
			this.resetResults();
		}

		// compute flags
		resize(stations, classes);
		changed = false;
		return true;
	}

	public boolean loadDescription(Element desc) {
		description = desc.getFirstChild().getNodeValue();
		return true;
	}

	//NEW
	//@author Stefano Omini
	public boolean loadClasses(Node classNode) {

		classes = Integer.parseInt(((Element) classNode).getAttribute("number"));

		classNames = new String[classes];
		classTypes = new int[classes];
		classData = new double[classes];

		NodeList classList = classNode.getChildNodes();

		int classNum = 0;

		maxpop = 0;
		Node n;
		Element current;
		closed = true;
		open = true;

		/* classes */
		for (int i = 0; i < classList.getLength(); i++) {
			n = classList.item(i);
			if (!(n instanceof Element)) {
				continue;
			}
			current = (Element) n;
			classNames[classNum] = current.getAttribute("name");
			if (current.getTagName().equals("closedclass")) {
				classTypes[classNum] = CLASS_CLOSED;
				classData[classNum] = Double.parseDouble(current.getAttribute("population"));
				maxpop += (int) classData[classNum];
				open = false;
			} else {
				classTypes[classNum] = CLASS_OPEN;
				classData[classNum] = Double.parseDouble(current.getAttribute("rate"));
				closed = false;
			}
			classNum++;
		}

		return true;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	public boolean loadStations(Node stationNode) {

		stations = Integer.parseInt(((Element) stationNode).getAttribute("number"));

		stationNames = new String[stations];
		stationTypes = new int[stations];
		stationServers = new int[stations];
		visits = new double[stations][];
		serviceTimes = new double[stations][][];

		NodeList stationList = stationNode.getChildNodes();

		ld = false;

		String statType;
		NodeList sTimes;
		int stationNum = 0;

		/* stations */

		Node n;
		Element current;

		for (int i = 0; i < stationList.getLength(); i++) {
			n = stationList.item(i);
			if (!(n instanceof Element)) {
				continue;
			}
			current = (Element) n;
			statType = current.getTagName();
			stationNames[stationNum] = current.getAttribute("name");
			if (current.hasAttribute("servers")) {
				stationServers[stationNum] = Integer.parseInt(current.getAttribute("servers"));
			} else {
				stationServers[stationNum] = 1;
			}

			/* make arrays */

			visits[stationNum] = new double[classes];
			serviceTimes[stationNum] = new double[classes][];

			/* station types and service times */

			if (statType.equals("ldstation")) {
				//LD
				ld = true;
				if (maxpop == 0) {
					System.err.println("LD station with zero customers");
					return false;
				}
				stationTypes[stationNum] = STATION_LD;

				/* create arrays */
				for (int k = 0; k < classes; k++) {
					//serviceTimes[stationNum] = new double[classes][maxpop + 1];
					serviceTimes[stationNum] = new double[classes][maxpop];
				}

				//Element sTimesElem = (Element) current.getElementsByTagName("servicetimes").item(0);
				Element sTimesElem = (Element) current.getElementsByTagName("servicetimes").item(0);
				sTimes = sTimesElem.getElementsByTagName("servicetimes");

				if (sTimes.getLength() != classes) {
					System.err.println("Wrong number of service times sets for LD station " + stationNames[stationNum]);
					return false;
				}

				Element visitsElem = (Element) current.getElementsByTagName("visits").item(0);
				NodeList visitsNodeList = visitsElem.getElementsByTagName("visit");

				for (int k = 0; k < classes; k++) {
					String visit = (visitsNodeList.item(k).getFirstChild()).getNodeValue();
					visits[stationNum][k] = Double.parseDouble(visit);

					//string of LD service times for class k
					Element class_st = (Element) sTimes.item(k);
					String stimes = class_st.getFirstChild().getNodeValue();

					double[] servt_arr = new double[maxpop];
					ArrayUtils.fromCSV(servt_arr, stimes);

					System.arraycopy(servt_arr, 0, serviceTimes[stationNum][k], 0, maxpop);
				}
			} else { //LI or delay
				if (statType.equals("delaystation")) {
					stationTypes[stationNum] = STATION_DELAY;
				} else {
					stationTypes[stationNum] = STATION_LI;
				}

				/* create arrays */

				sTimes = current.getElementsByTagName("servicetime");
				NodeList visitsNodeList = current.getElementsByTagName("visit");

				serviceTimes[stationNum] = new double[classes][1];
				visits[stationNum] = new double[classes];
				for (int k = 0; k < classes; k++) {

					Node node = sTimes.item(k).getFirstChild();
					String nodeValue = (node).getNodeValue();
					serviceTimes[stationNum][k][0] = Double.parseDouble(nodeValue);
					visits[stationNum][k] = Double.parseDouble((visitsNodeList.item(k).getFirstChild()).getNodeValue());
				}
			}
			stationNum++;
		}

		return true;
	}

	//end NEW

	/**
	 * Load solutions from xml file
	 * @param sol NodeList of solution elements
	 * @return true if load was succesful, false otherwise
	 */
	public boolean loadSolution(NodeList sol) {
		resultsOK = true;
		resetResults();
		for (int i = 0; i < sol.getLength(); i++) {
			Element solution = (Element) sol.item(i);
			String status = solution.getAttribute("ok");
			resultsOK = resultsOK && (status.equals("true"));
			ArrayUtils.copy2to3(loadResultsMatrix(solution, stations, classes, "Number of Customers"), queueLen, i);
			ArrayUtils.copy2to3(loadResultsMatrix(solution, stations, classes, "Throughput"), throughput, i);
			ArrayUtils.copy2to3(loadResultsMatrix(solution, stations, classes, "Residence time"), resTimes, i);
			ArrayUtils.copy2to3(loadResultsMatrix(solution, stations, classes, "Utilization"), util, i);
		}
		return true;
	}

	//NEW
	//@author Stefano Omini
	public double[][] loadResultsMatrix(Element base, int len1, int len2, String res) {

		//matrix of results
		double[][] arr = new double[len1][len2];

		if (base.getElementsByTagName("stationresults").getLength() != len1) {
			return null;
		}

		for (int i = 0; i < len1; i++) {
			Element s_res = (Element) base.getElementsByTagName("stationresults").item(i);
			for (int c = 0; c < len2; c++) {
				Element n_cls = (Element) s_res.getElementsByTagName("classresults").item(c);

				NodeList measure_list = n_cls.getElementsByTagName("measure");
				Element measure;
				String value = null;

				for (int m = 0; m < measure_list.getLength(); m++) {
					measure = (Element) measure_list.item(m);
					//Below IF clause is added for backward compatibility of the Perf Index : Number of customers
					//as previously it was known as Queue Length.
					if (res.equalsIgnoreCase("Number of Customers")) {//This is the present name of Label
						if (measure.getAttribute("measureType").equalsIgnoreCase("Queue length")) {//Previously known as "Queue length" in old JMVA files.
							res = "Queue length";
						}
					}
					if (measure.getAttribute("measureType").equalsIgnoreCase(res)) {
						//it's the measure we are searching for
						value = measure.getAttribute("meanValue");
						break;
					}
				}

				//Element r = (Element) n_cls.getElementsByTagName(res).item(0);
				//String value = r.getFirstChild().getNodeValue();

				if (value != null) {
					arr[i][c] = Double.parseDouble(value);
				} else {
					arr[i][c] = 0.0;
				}

			}
		}
		return arr;
	}

	//end NEW

	//methods for aggregate results retrieval

	/**Returns per-class aggregate for throughput*/
	public double[][] getPerClassX() {
		if (throughput == null) {
			return null;
		} else {
			double[][] retVal = new double[classes][iterations];
			// Scans for every iteration (what if analysis)
			for (int k = 0; k < iterations; k++) {
				//scan columns to get one value per column
				for (int i = 0; i < retVal.length; i++) {
					//scan cells of each column
					for (int j = 0; j < throughput.length; j++) {
						//throughput is ratio of specific throughput on specific num of visits
						if (visits[j][i] != 0) {
							retVal[i][k] = throughput[j][i][k] / visits[j][i];
							break;
						} else {
							//if all visits for a class (why is this included in model???)
							//throughput for that class is 0
							if (j == throughput.length - 1) {
								retVal[i][k] = 0;
							}
						}
					}

				}

			}
			return retVal;
		}
	}

	/**Returns per-station aggregate for throughput*/
	public double[][] getPerStationX() {
		if (throughput == null) {
			return null;
		} else {
			double[][] retVal = new double[stations][iterations];
			for (int i = 0; i < retVal.length; i++) {
				// Scans for every iteration (what if analysis)
				for (int k = 0; k < iterations; k++) {
					retVal[i][k] = 0;
					for (int j = 0; j < throughput[i].length; j++) {
						retVal[i][k] += throughput[i][j][k];
					}
				}
			}
			return retVal;
		}
	}

	/**Returns global aggregate for throughput*/
	public double[] getGlobalX() {
		double[] retVal = new double[iterations];
		double[][] aggs = getPerClassX();
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			if (throughput == null) {
				retVal[k] = Double.NaN;
			} else {
				if (aggs != null) {
					for (double[] agg : aggs) {
						retVal[k] += agg[k];
					}
				} else {
					retVal[k] = Double.NaN;
				}
			}
		}
		return retVal;
	}

	/**Returns per-class aggregate for queue lenghts*/
	public double[][] getPerClassQ() {
		if (queueLen == null) {
			return null;
		}
		double[][] retVal = new double[classes][iterations];
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			//first scan columns
			for (int i = 0; i < retVal.length; i++) {
				retVal[i][k] = 0;
				//then rows
				for (double[][] element : queueLen) {
					retVal[i][k] += element[i][k];
				}
			}
		}
		return retVal;
	}

	/**Returns per-station aggregate for queue lenghts*/
	public double[][] getPerStationQ() {
		if (queueLen == null) {
			return null;
		}
		double[][] retVal = new double[stations][iterations];
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			for (int i = 0; i < queueLen.length; i++) {
				retVal[i][k] = 0;
				for (int j = 0; j < queueLen[i].length; j++) {
					retVal[i][k] += queueLen[i][j][k];
				}
			}
		}
		return retVal;
	}

	/**Returns global aggregate for queue lenghts*/
	public double[] getGlobalQ() {
		if (queueLen == null) {
			return null;
		}
		double[] retVal = new double[iterations];
		double[][] aggs = getPerClassQ();
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			if (aggs != null) {
				for (double[] agg : aggs) {
					retVal[k] += agg[k];
				}
			} else {
				retVal[k] = Double.NaN;
			}
		}
		return retVal;
	}

	/**Returns per-class aggregate for residence times*/
	public double[][] getPerClassR() {
		if (resTimes == null) {
			return null;
		}
		double[][] retVal = new double[classes][iterations];
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			for (int i = 0; i < retVal.length; i++) {
				retVal[i][k] = 0;
				for (double[][] resTime : resTimes) {
					retVal[i][k] += resTime[i][k];
				}
			}
		}
		return retVal;
	}

	/**Returns per-station aggregate for residence times*/
	public double[][] getPerStationR() {
		if (resTimes == null) {
			return null;
		}
		double[][] retVal = new double[stations][iterations];
		double[][] xClassAggs = getPerClassX();
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			for (int i = 0; i < retVal.length; i++) {
				retVal[i][k] = 0;
				double dividend = 0;
				for (int j = 0; j < resTimes[i].length; j++) {
					if (xClassAggs != null) {
						retVal[i][k] += xClassAggs[j][k] * resTimes[i][j][k];
						dividend += xClassAggs[j][k];
					} else {
						return null;
					}
				}
				if (dividend != 0) {
					retVal[i][k] /= dividend;
				} else {
					retVal[i][k] = 0;
				}
			}
		}
		return retVal;
	}

	/**Returns system response time*/
	public double[] getGlobalR() {
		if (resTimes == null) {
			return null;
		}
		double[] retVal = new double[iterations];
		double[][] xClassAggs = getPerClassX();
		double[][] aggs = getPerClassR();
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			double dividend = 0;
			if (aggs != null) {
				for (int i = 0; i < aggs.length; i++) {
					retVal[k] += xClassAggs[i][k] * aggs[i][k];
					dividend += xClassAggs[i][k];
				}
				if (dividend > 0) {
					retVal[k] /= dividend;
				} else {
					retVal[k] = 0;
				}
			} else {
				retVal[k] = Double.NaN;
			}
		}
		return retVal;
	}

	/**Returns per-class aggregate for utilization*/
	public double[][] getPerClassU() {
		if (util == null) {
			return null;
		} else {
			/* Disabled as this measure is unsensed... Returns an array with negatives instead
			double[] retVal = new double[classes];
			for(int i=0; i<retVal.length; i++){
			    retVal[i] = 0;
			    for(int j=0; j<util.length; j++){
			        retVal[i] += util[j][i];
			    }
			}
			return retVal;
			*/
			double[][] neg = new double[classes][iterations];
			for (int k = 0; k < classes; k++) {
				Arrays.fill(neg[k], -1.0);
			}
			return neg;
		}
	}

	public double[][] getPerStationU() {
		if (util == null) {
			return null;
		}
		double[][] retVal = new double[stations][iterations];
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			for (int i = 0; i < retVal.length; i++) {
				retVal[i][k] = 0;
				for (int j = 0; j < util[i].length; j++) {
					retVal[i][k] += util[i][j][k];
				}
			}
		}
		return retVal;
	}

	public double[] getGlobalU() {
		if (util == null) {
			return null;
		} else {
			/* Disabled as this measure is unsensed... Returns an array with negatives instead
			double retVal = 0;
			double[] aggs = getPerStationU();
			if(aggs!=null){
			    for(int i=0; i<aggs.length; i++) retVal += aggs[i];
			    return retVal;
			}else return Double.NaN;
			*/
			double[] neg = new double[iterations];
			Arrays.fill(neg, -1.0);
			return neg;
		}
	}

	//Added by ASHANKA START
	//System Power
	//Unlike other performance indices we don't have station level
	//aggregate and individual 
	public double[][] getPerClassSP() {
		if (resTimes == null) {
			return null;
		}
		double[][] retVal = new double[classes][iterations];
		// Scans for every iteration (what if analysis)
		for (int k = 0; k < iterations; k++) {
			for (int i = 0; i < retVal.length; i++) {
				try {
					retVal[i][k] = getPerClassX()[i][k] / getPerClassR()[i][k];
				} catch (ArithmeticException ae) {
					retVal[i][k] = 0;
				}
			}
		}
		return retVal;
	}

	public double[] getGlobalSP() {
		if (resTimes == null) {
			return null;
		}
		double[] retVal = new double[iterations];
		for (int k = 0; k < iterations; k++) {
			try {
				retVal[k] = getGlobalX()[k] / getGlobalR()[k];
			} catch (ArithmeticException e) {
				retVal[k] = 0;
			}
		}
		return retVal;
	}

	//Added by ASHANKA STOP
	/**
	 * This method tells if visits were set or are all unitary (or zero if
	 * corresponding service time is zero). This is used to show correct panel layout
	 * upon loading
	 * @return true iff visits were not set
	 */
	public boolean areVisitsSet() {
		return unitaryVisits;
	}

	//NEW Federico Dall'Orso
	/**Randomizes model's service times and visits.*/
	public void randomizeModelData() {
		double globRate = globalArrRate();
		for (int i = 0; i < serviceTimes.length; i++) {
			for (int j = 0; j < serviceTimes[i].length; j++) {
				for (int k = 0; k < serviceTimes[i][j].length; k++) {
					if (j < classTypes.length) {
						if (classTypes[j] == CLASS_CLOSED) {
							serviceTimes[i][j][k] = MAXRAND * Math.exp(-Math.random() * MAXRANGE);
						} else {
							if (globRate != 0) {
								serviceTimes[i][j][k] = Math.random() * (0.9) / globRate;
							} else {
								serviceTimes[i][j][k] = Math.random();
							}
						}
					}
				}
			}
		}
		for (int i = 0; i < visits.length; i++) {
			for (int j = 0; j < visits[i].length; j++) {
				visits[i][j] = 1;
			}
		}
	}

	//calculates global arrival rate for open classes
	private double globalArrRate() {
		double sum = 0;
		for (int i = 0; i < classTypes.length && i < classData.length; i++) {
			if (classTypes[i] == CLASS_OPEN) {
				sum += classData[i];
			}
		}
		return sum;
	}

	//END

	//---- Methods for What-If analysis ---- Bertoli Marco ------------------------------
	/**
	 * Tells if this model includes a what-if analysis
	 * @return true if this model includes a what-if analysis
	 */
	public boolean isWhatIf() {
		return iterations > 1;
	}

	/**
	 * Removes What-if analysis from current model
	 */
	public void removeWhatIf() {
		if (iterations != 1) {
			iterations = 1;
			changed = true;
		}
	}

	/**
	 * Sets the array of values used for what-if analysis
	 * @param values vector with values to be used in iterations of what-if analysis
	 */
	public boolean setWhatIfValues(double[] values) {
		if (whatIfValues == null && values == null) {
			return false;
		}

		if (whatIfValues == null || !Arrays.equals(whatIfValues, values)) {
			whatIfValues = values;
			if (values != null) {
				iterations = values.length;
			} else {
				iterations = 1;
			}
			changed = true;
			resultsOK = false;
			return true;
		}
		return true;
	}

	/**
	 * Sets class used for what-if analysis
	 * @param classNum ordered number of selected class or -1 for every class
	 * @return true if data was changed, false otherwise
	 */
	public boolean setWhatIfClass(int classNum) {
		if (whatIfClass != classNum) {
			whatIfClass = classNum;
			changed = true;
			resultsOK = false;
			return true;
		}
		return false;
	}

	/**
	 * Sets station used for what-if analysis
	 * @param stationNum ordered number of selected station or -1 for every station
	 * @return true if data was changed, false otherwise
	 */
	public boolean setWhatIfStation(int stationNum) {
		if (whatIfStation != stationNum) {
			whatIfStation = stationNum;
			changed = true;
			resultsOK = false;
			return true;
		}
		return false;
	}

	/**
	 * Sets type of what-if analysis
	 * @param type WHAT_IF_ARRIVAL, WHAT_IF_CUSTOMERS, WHAT_IF_MIX, WHAT_IF_DEMANDS
	 * @see ExactConstants
	 * @return true if data was changed, false otherwise
	 */
	public boolean setWhatIfType(String type) {
		if (whatIfType == null && type == null) {
			return false;
		}
		if (whatIfType == null || !whatIfType.equalsIgnoreCase(type)) {
			whatIfType = type;
			changed = true;
			resultsOK = false;
			return true;
		}
		return false;
	}

	/**
	 * Returns type of what-if analysis
	 * @return WHAT_IF_ARRIVAL, WHAT_IF_CUSTOMERS, WHAT_IF_MIX, WHAT_IF_DEMANDS
	 * @see ExactConstants
	 */
	public String getWhatIfType() {
		return whatIfType;
	}

	/**
	 * Returns index of station selected for what-if analysis or -1 if every station is selected
	 * @return index of station selected for what-if analysis or -1 if every station is selected
	 */
	public int getWhatIfStation() {
		return whatIfStation;
	}

	/**
	 * Returns index of class selected for what-if analysis or -1 if every class is selected
	 * @return index of class selected for what-if analysis or -1 if every class is selected
	 */
	public int getWhatIfClass() {
		return whatIfClass;
	}

	/**
	 * Returns the array of values used for what-if analysis
	 * @return the array of values used for what-if analysis
	 */
	public double[] getWhatIfValues() {
		return whatIfValues;
	}

	/**
	 * This method is used to generate a suitable vector of values for what-if analysis.
	 * @param type type of what-if analysis to be performed. (WHAT_IF_ARRIVAL,
	 * WHAT_IF_CUSTOMERS, WHAT_IF_MIX, WHAT_IF_DEMANDS)
	 * @param from initial value
	 * @param to final value
	 * @param iterations expected number of iterations (will be adjusted to be compliant
	 * with specified from and to values in WHAT_IF_CUSTOMERS and WHAT_IF_MIX)
	 * @param ClassRef index of class to be analyzed or -1 for all classes
	 * @param stationRef index of station to be analyzed (only for WHAT_IF_DEMANDS)
	 * @return suitable array of iterations to be performed
	 */
	public double[] generateWhatIfValues(String type, double from, double to, int iterations, int ClassRef, int stationRef) {
		double[] ret, tmp;
		int n = iterations - 1;
		boolean inverted = false; // tells if 'from' and 'to' values were exchanged
		// order from and to values
		double f, t;
		if (from < to) {
			f = from;
			t = to;
		} else if (from > to) {
			f = to;
			t = from;
			inverted = true;
		} else {
			// In the case of overlapping from and to values, returns a single-number array
			return new double[] { from };
		}

		// Avoid 0 arrival rate
		if (type.equals(WHAT_IF_ARRIVAL) && f == 0.0) {
			f = 1e-5;
		}

		// Arrival rate and service Demands: this are really simple.
		if (type.equals(WHAT_IF_ARRIVAL) || type.equals(WHAT_IF_DEMANDS)) {
			ret = new double[iterations];
			for (int i = 0; i <= n; i++) {
				ret[i] = (f * (n - i) + i * t) / n;
			}
		}
		// Number of customers: this is complex because only integer values are allowed.
		else if (type.equals(WHAT_IF_CUSTOMERS)) {
			// Single class
			if (ClassRef >= 0) {
				tmp = new double[iterations];
				int c = 0; // a simple counter to remove duplicates
				for (int i = 0; i < iterations; i++) {
					tmp[c] = Math.rint((f * (n - i) + i * t) / n);
					// Increment counter only if last element was not repeated
					if (c < 1 || tmp[c] != tmp[c - 1]) {
						c++;
					}
				}
				// Pack target array
				ret = new double[c];
				System.arraycopy(tmp, 0, ret, 0, c);
			}
			// Multiclass
			else {
				// An array that will hold number of customers for each closed class.
				int[] customers = new int[classes];
				int c = 0; // counter of closed classes
				for (int i = 0; i < classes; i++) {
					if (classTypes[c] == CLASS_CLOSED) {
						customers[c++] = (int) classData[i];
					}
				}
				// Inverse of Highest Common Factor is the minimum percentage allowed.
				int hcf = hcf(customers, c);
				if (hcf == 0) {
					hcf = 1;
				}
				double step = 1.0 / hcf;
				if (f < step) {
					f = step;
				}
				if (t < f) {
					return new double[] { f };
				}

				c = 0; // counter of created steps
				tmp = new double[iterations];
				for (int i = 0; i < iterations; i++) {
					tmp[c] = Math.rint((f / step * (n - i) + i * t / step) / n);
					// Increment counter only if last element was not repeated
					if (c < 1 || tmp[c] != tmp[c - 1]) {
						c++;
					}
				}

				// Now creates results array
				ret = new double[c];
				for (int i = 0; i < c; i++) {
					ret[i] = tmp[i] * step;
				}
			}
		}
		// Population mix: this is complex because only integer values are allowed.
		else if (type.equals(WHAT_IF_MIX)) {
			int cl2 = -1;
			// Finds second class
			for (int i = 0; i < classes; i++) {
				if (classTypes[i] == CLASS_CLOSED && i != ClassRef) {
					cl2 = i;
					break;
				}
			}

			int N = (int) (classData[ClassRef] + classData[cl2]);

			double step = 1.0 / N;
			if (f < step) {
				f = step;
			}
			if (t > (N - 1) * step) {
				t = (N - 1) * step;
			}
			if (t < f) {
				return new double[] { f };
			}

			int c = 0; // counter of created steps
			tmp = new double[iterations];
			for (int i = 0; i < iterations; i++) {
				tmp[c] = Math.rint((f / step * (n - i) + i * t / step) / n);
				// Increment counter only if last element was not repeated
				if (c < 1 || tmp[c] != tmp[c - 1]) {
					c++;
				}
			}

			// Now creates results array
			ret = new double[c];
			for (int i = 0; i < c; i++) {
				ret[i] = tmp[i] * step;
			}
		} else {
			ret = null;
		}

		// Inverts results if from and to values were exchanged
		if (inverted && ret != null) {
			double[] inv = new double[ret.length];
			for (int i = 0; i < ret.length; i++) {
				inv[ret.length - 1 - i] = ret[i];
			}
			ret = inv;
		}
		return ret;
	}

	/**
	 * This method will recalculate whatif analysis values after the initial values were changed. At first
	 * detects changes, than applies modifications. If what-if analysis is no longer appliable, resets it.
	 */
	public void recalculateWhatifValues() {
		if (whatIfType == null) {
			return;
		}

		HashSet<Integer> closedClasses = new HashSet<Integer>();
		HashSet<Integer> openClasses = new HashSet<Integer>();

		for (int i = 0; i < classTypes.length; i++) {
			if (classTypes[i] == CLASS_OPEN) {
				openClasses.add(new Integer(i));
			} else if (classTypes[i] == CLASS_CLOSED) {
				closedClasses.add(new Integer(i));
			}
		}

		// Checks validity first
		if (classTypes.length <= whatIfClass || stationTypes.length <= whatIfStation) {
			removeWhatIf();
		} else if (WHAT_IF_ARRIVAL.equals(whatIfType)) {
			if (openClasses.size() == 0 || (whatIfClass >= 0 && classTypes[whatIfClass] != CLASS_OPEN)) {
				removeWhatIf();
			}
		} else if (WHAT_IF_CUSTOMERS.equals(whatIfType)) {
			if (closedClasses.size() == 0 || (whatIfClass >= 0 && classTypes[whatIfClass] != CLASS_CLOSED)) {
				removeWhatIf();
			}
		} else if (WHAT_IF_MIX.equals(whatIfType)) {
			if (closedClasses.size() != 2 || (whatIfClass >= 0 && classTypes[whatIfClass] != CLASS_CLOSED)) {
				removeWhatIf();
			}
		} else if (WHAT_IF_DEMANDS.equals(whatIfType)) {
			if (whatIfStation >= 0 && stationTypes[whatIfStation] == STATION_LD) {
				removeWhatIf();
			}
		}

		// If what-if is still valid, updates initial values
		if (whatIfType != null) {
			// Check if class data was changed
			if (whatIfClass >= 0) {
				if ((WHAT_IF_ARRIVAL.equals(whatIfType) || WHAT_IF_CUSTOMERS.equals(whatIfType)) && classData[whatIfClass] != whatIfValues[0]) {
					setWhatIfValues(generateWhatIfValues(whatIfType, classData[whatIfClass], whatIfValues[iterations - 1], iterations, whatIfClass,
							whatIfStation));
				} else if (WHAT_IF_DEMANDS.equals(whatIfType)
						&& serviceTimes[whatIfStation][whatIfClass][0] * visits[whatIfStation][whatIfClass] != whatIfValues[0]) {
					setWhatIfValues(generateWhatIfValues(whatIfType,
							serviceTimes[whatIfStation][whatIfClass][0] * visits[whatIfStation][whatIfClass], whatIfValues[iterations - 1],
							iterations, whatIfClass, whatIfStation));
				} else if (WHAT_IF_MIX.equals(whatIfType)) {
					// Check that no fractionary values are used
					int class2 = -1;
					for (Integer integer : closedClasses) {
						int idx = integer.intValue();
						if (idx != whatIfClass) {
							class2 = idx;
							break;
						}
					}
					for (int i = 0; i < iterations; i++) {
						double fClassVal = whatIfValues[i] * classData[whatIfClass];
						double sClassVal = (1 - whatIfValues[i]) * classData[class2];
						if (Math.abs(fClassVal - Math.rint(fClassVal)) > 1e-8 || Math.abs(sClassVal - Math.rint(sClassVal)) > 1e-8) {
							setWhatIfValues(generateWhatIfValues(whatIfType, 0, 1, iterations, whatIfClass, whatIfStation));
							break;
						}

					}
				}
			}
		}
	}

	/**
	 * Helper method that finds Highest Common Factor in a given array of integer values.
	 * @param values array of integer values. MUST have at least 2 elements
	 * @param len number of elements to be considered in input array (must be at least 2)
	 * @return found hcf
	 */
	private static int hcf(int[] values, int len) {
		int min, max, tmp;
		min = values[0];

		for (int i = 1; i < len; i++) {
			// Finds minimum value between min (previous hcf) and values[i]
			if (values[i] > min) {
				max = values[i];
			} else {
				max = min;
				min = values[i];
			}
			tmp = max % min;
			while (tmp > 0) {
				max = min;
				min = tmp;
				tmp = max % min;
			}
			// At this point 'min' holds the hcf value
		}
		return min;
	}

	/**
	 * This function will check if one or more resources are in saturation. This will
	 * consider each iteration of what-if analysis if present.
	 * @return NO_SATURATION if everything is okay, SATURATION if a class saturation is
	 * detected with specified parameters and SATURATION_WHATIF if a saturation will be caused
	 * by whatif values.
	 */
	public int checkSaturation() {
		// Checks saturation without what-if analysis
		if (checkForSaturation(classData, visits, serviceTimes, stationServers)) {
			return SATURATION_WHATIF;
		}
		if (isWhatIf()) {
			double maxValue = whatIfValues[iterations - 1];
			// Checks if values are inverted
			if (whatIfValues[0] > maxValue) {
				maxValue = whatIfValues[0];
			}

			// What if arrival rates
			if (whatIfType.equals(WHAT_IF_ARRIVAL)) {
				double[] newClassData = (double[]) classData.clone();
				// Change arrival rate of a single class only
				if (whatIfClass >= 0) {
					newClassData[whatIfClass] = maxValue;
				}
				// Change arrival rate of all open classes
				else {
					for (int i = 0; i < classes; i++) {
						if (classTypes[i] == ExactConstants.CLASS_OPEN) {
							newClassData[i] *= maxValue;
						}
					}
				}
				if (checkForSaturation(newClassData, visits, serviceTimes, stationServers)) {
					return SATURATION_WHATIF;
				}
			}
			// What if service demands
			else if (whatIfType.equals(WHAT_IF_DEMANDS)) {
				double[][][] newServiceTimes = (double[][][]) serviceTimes.clone();
				double[][] newVisits = (double[][]) visits.clone();

				// Change service demands of a LI station for a single (open) class only
				if (whatIfClass >= 0 && classTypes[whatIfClass] == CLASS_OPEN) {
					newServiceTimes[whatIfStation][whatIfClass][0] = maxValue;
					newVisits[whatIfStation][whatIfClass] = 1;
				}
				// Change service demands of a LI station for all (open) classes
				else {
					for (int i = 0; i < classes; i++) {
						if (classTypes[i] == ExactConstants.CLASS_OPEN) {
							newServiceTimes[whatIfStation][i][0] *= maxValue;
						}
					}
				}
				if (checkForSaturation(classData, newVisits, newServiceTimes, stationServers)) {
					return SATURATION_WHATIF;
				}
			}
		}
		return NO_SATURATION;
	}

	public static final int NO_SATURATION = 0;
	public static final int SATURATION = 1;
	public static final int SATURATION_WHATIF = 2;

	/**
	 * Checks current model for saturation, given arrival rates for customer classes
	 * @param classData arrival rates for customer classes
	 * @param visits number of visits for station
	 * @param serviceTimes service times for station
	 * @param stationServers number of servers for each station
	 * @return true if model will saturate, false otherwise
	 */
	private boolean checkForSaturation(double[] classData, double[][] visits, double[][][] serviceTimes, int[] stationServers) {
		for (int i = 0; i < stations; i++) {
			if (stationTypes[i] == STATION_DELAY) {
				//delay station: don't check saturation
				continue;
			}

			//utiliz is the aggregate utilization for station j
			double utiliz = 0;
			for (int j = 0; j < classes; j++) {
				//consider only open classes
				if (classTypes[j] == CLASS_OPEN) {
					utiliz += classData[j] * visits[i][j] * serviceTimes[i][j][0];
				}
			}
			if (utiliz >= stationServers[i]) {
				return true;
			}
		}
		//there are no stations in saturation
		return false;
	}
	//-----------------------------------------------------------------------------------
}
