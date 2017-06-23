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

package jmt.gui.jaba;

import java.util.Arrays;
import java.util.Vector;

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
 */
public class JabaModel implements JabaConstants {

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
	 * dim: queueLen[stations][classes+1]
	 */
	private double[][] queueLen;

	/**
	 * throughput
	 * dim: throughput[stations+1][classes+1]
	 */
	private double[][] throughput;

	/**
	 * residence times
	 * dim: resTime[stations+1][classes+1]
	 */
	private double[][] resTimes;

	/**
	 * utilization
	 * dim: util[stations][classes+1]
	 */
	private double[][] util;

	private Vector<Object> res;

	/*****************************************************************/
	//parameters for randomization
	private static final double MAXRAND = 100;
	//todo controllare che MAXRANGE=1 sia ok
	private static final double MAXRANGE = 1;

	/*****************************************************************/

	/**
	 * make an object with default values
	 */
	public JabaModel() {
		setDefaults();
	}

	/**
	 * copy constructor
	 */
	public JabaModel(JabaModel e) {
		closed = e.closed;
		open = e.open;
		hasResults = e.hasResults;
		resultsOK = e.resultsOK;
		changed = e.changed;

		stations = e.stations;
		classes = e.classes;
		maxpop = e.maxpop;

		description = e.description;
		unitaryVisits = e.unitaryVisits;

		stationNames = ArrayUtils.copy(e.stationNames);
		stationTypes = ArrayUtils.copy(e.stationTypes);

		classNames = ArrayUtils.copy(e.classNames);
		classTypes = ArrayUtils.copy(e.classTypes);
		classData = ArrayUtils.copy(e.classData);

		visits = ArrayUtils.copy2(e.visits);

		serviceTimes = ArrayUtils.copy3(e.serviceTimes);

		if (hasResults) {
			// Zanzottera
			res = e.res;
		}
	}

	/**
	 * Clears all the results
	 */
	public void discardResults() {
		hasResults = false;
		resultsOK = false;
		queueLen = null;
		throughput = null;
		resTimes = null;
		util = null;
		changed = true;
		// Zanzottera
		res = null;
	}

	/**
	 * sets all the result data for this model.
	 */
	public void setResults(Vector<Object> res) {
		this.res = res;
		hasResults = true;
		resultsOK = true;
		changed = true;
	}

	public Vector<Object> getResults() {
		return res;
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
		// NEW minimum classes = 2
		classes = 2;

		//TODO: perchè è 1 se non ci sono customers?? Lasciare così
		maxpop = 1;
		changed = true;

		classData = new double[2];
		//NEW
		//@author Stefano Omini
		//@author Andrea Zanzottera (added second class)
		//todo mettere String
		classData[0] = 0;
		classData[1] = 0;
		//end NEW

		stationNames = new String[1];
		stationNames[0] = "Station1";

		stationTypes = new int[1];
		stationTypes[0] = STATION_LI;

		classNames = new String[2];
		classNames[0] = "Class1";
		classNames[1] = "Class2";

		classTypes = new int[2];
		classTypes[0] = CLASS_CLOSED;
		classTypes[1] = CLASS_CLOSED;

		visits = new double[1][2];
		//Settato a 1 i Service Demands di default
		visits[0][0] = 1.0;
		visits[0][1] = 1.0;

		serviceTimes = new double[1][2][1];
		//NEW
		//@author Andrea Zanzottera
		//Settato a 1 i Service Demands di default
		serviceTimes[0][0][0] = 1.0;
		serviceTimes[0][1][0] = 1.0;
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
	 */
	public void setDescription(String description) {
		if (!changed) {
			if (description.equals(this.description)) {
				return;
			}
		}
		this.description = description;
		changed = true;
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
	 * sets the names of the service centers.
	 * @param stationNames the names of the service centers
	 * @throws IllegalArgumentException if the array is not of the correct size
	 */
	public void setStationNames(String[] stationNames) {
		if (stationNames.length != stations) {
			throw new IllegalArgumentException("stationNames.length!=stations");
		}
		if (!changed) {
			if (Arrays.equals(this.stationNames, stationNames)) {
				return;
			}
		}
		this.stationNames = stationNames;
		changed = true;
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
	 */
	public void setClassNames(String[] classNames) {
		if (classNames.length != classes) {
			throw new IllegalArgumentException("classNames.length!=classes");
		}
		if (!changed) {
			if (Arrays.equals(this.classNames, classNames)) {
				return;
			}
		}
		this.classNames = classNames;
		changed = true;
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
	 */
	public void setClassData(double[] classData) {
		if (classData.length != classes) {
			throw new IllegalArgumentException("classData.length!=classes");
		}
		if (!changed || resultsOK) {
			if (Arrays.equals(this.classData, classData)) {
				return;
			}
		}
		this.classData = classData;
		changed = true;
		resultsOK = false;

		// make sure 3rd dimension of serviceTimes is ok
		resize(stations, classes);
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
	 */
	public void setClassTypes(int[] classTypes) {
		if (classTypes.length != classes) {
			throw new IllegalArgumentException("classTypes.length!=classes");
		}
		if (!changed || resultsOK) {
			if (Arrays.equals(this.classTypes, classTypes)) {
				return;
			}
		}
		this.classTypes = classTypes;
		closed = calcClosed();
		open = calcOpen();
		changed = true;
		resultsOK = false;
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
	 */
	public void setStationTypes(int[] stationTypes) {
		if (stationTypes.length != stations) {
			throw new IllegalArgumentException("stationTypes.length!=stations");
		}
		if (!changed || resultsOK) {
			if (Arrays.equals(this.stationTypes, stationTypes)) {
				return;
			}
		}
		this.stationTypes = stationTypes;
		// adjusts serviceTimes size and recalculates flags
		resize(stations, classes);
		changed = true;
		resultsOK = false;
	}

	/**
	 * @return the matrix of visits
	 */
	public double[][] getVisits() {
		return visits;
	}

	/**
	 * This method tells if visits were set or are all unitary (or zero if
	 * corresponding service time is zero). This is used to show correct panel layout
	 * upon loading
	 * @return true iff visits were not set
	 */
	public boolean areVisitsSet() {
		return unitaryVisits;
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
	 * sets the matrix of visits
	 * @param visits the matrix of visits
	 * @throws IllegalArgumentException if the matrix is not of the correct size
	 */
	public void setVisits(double[][] visits) {
		if (visits.length != stations || visits[0].length != classes) {
			throw new IllegalArgumentException("incorrect array dimension");
		}
		if (!changed || resultsOK) {
			if (ArrayUtils.equals2(this.visits, visits)) {
				return;
			}
		}
		this.visits = visits;
		changed = true;
		resultsOK = false;
		// Checks if visits are all one
		calcUnitaryVisits();
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
	 */
	public void setServiceTimes(double[][][] serviceTimes) {
		if (serviceTimes.length != stations || serviceTimes[0].length != classes) {
			throw new IllegalArgumentException("incorrect array dimension");
		}
		if (!changed || resultsOK) {
			if (ArrayUtils.equals3(this.serviceTimes, serviceTimes)) {
				return;
			}
		}
		int currSize;
		double[][] subST;

		//validate sizes
		for (int s = 0; s < stations; s++) {
			currSize = (stationTypes[s] == STATION_LD ? maxpop : 1);
			//TODO: se stazione è LD ma non ci sono customers, max pop = 0
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
	}

	/**
	 * Resizes the data structures according to specified parameters. Data is preserved as far as possible
	 */
	public void resize(int stations, int classes) {
		if (stations <= 0 || classes <= 0) {
			throw new IllegalArgumentException("stations and classes must be >0");
		}
		if (this.stations != stations || this.classes != classes) {
			//other cases already handled in setXXX methods
			discardResults();
		}
		this.stations = stations;
		this.classes = classes;

		stationNames = ArrayUtils.resize(stationNames, stations, null);
		stationTypes = ArrayUtils.resize(stationTypes, stations, STATION_LI);
		ld = calcLD();

		visits = ArrayUtils.resize2(visits, stations, classes, 1.0);

		classNames = ArrayUtils.resize(classNames, classes, null);
		classTypes = ArrayUtils.resize(classTypes, classes, CLASS_CLOSED);
		closed = calcClosed();

		classData = ArrayUtils.resize(classData, classes, 0.0);

		maxpop = calcMaxpop();

		serviceTimes = ArrayUtils.resize3var(serviceTimes, stations, classes, calcSizes(), 1.0);
		// Checks if visits are all one
		calcUnitaryVisits();
	}

	/**
	 * @return queue lengths
	 */
	public double[][] getQueueLen() {
		return queueLen;
	}

	/**
	 * @return response times
	 */
	public double[][] getResTimes() {
		return resTimes;
	}

	/**
	 * @return throughputs
	 */
	public double[][] getThroughput() {
		return throughput;
	}

	/**
	 * @return utilizations
	 */
	public double[][] getUtilization() {
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
	 * @return true if the model contains only closed stations
	 */
	private boolean calcClosed() {
		for (int i = 0; i < classes; i++) {
			if (classTypes[i] != CLASS_CLOSED) {
				//make sure we stay in a consistent state
				removeLD();
				//TODO: rimuove le LD perchè il caso LD multiclasse non è gestibile
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
		/*
		if (hasResults) {
		    s.append("queue lengths=").append(ArrayUtils.toString2(queueLen)).append("\n")
		            .append("throughput=").append(ArrayUtils.toString2(throughput)).append("\n")
		            .append("resTimes=").append(ArrayUtils.toString2(resTimes)).append("\n")
		            .append("utilization=").append(ArrayUtils.toString2(util)).append("\n");

		}
		*/

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
		modelElement.setAttribute("jaba", "true");

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

			//NEW
			//@author Stefano Omini
			//TODO: controllare se va
			classes_element.appendChild(makeClassElement(root, i));
			//end NEW
		}

		/* stations */
		Element stationsElement = root.createElement("stations");
		parametersElement.appendChild(stationsElement);
		stationsElement.setAttribute("number", Integer.toString(stations));
		for (int i = 0; i < stations; i++) {
			stationsElement.appendChild(makeStationElement(root, i));
		}

		//OLD
		//if (hasResults) modelElement.appendChild(makeSolutionElement(root));

		//NEW
		//@author Stefano Omini
		//todo eliminato il salvataggio dei risultati
		//if (hasResults && resultsOK) modelElement.appendChild(makeSolutionElement(root));
		//end NEW

		return root;

	}

	private Element makeClassElement(Document root, int classNum) {
		Element classElement = null;
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

		Element station_element = null;
		Node servicetimes_element;
		Node visits_element;

		switch (this.stationTypes[stationNum]) {

			case STATION_LI:

				station_element = root.createElement("listation");
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
					st_element.setAttribute("customerclass", class_name);;
					st_element.appendChild(root.createTextNode(Double.toString(this.serviceTimes[stationNum][j][0])));
					servicetimes_element.appendChild(st_element);
					/* set visit */
					Element visit_element = root.createElement("visit");
					visit_element.setAttribute("customerclass", class_name);
					visit_element.appendChild(root.createTextNode(Double.toString(this.visits[stationNum][j])));
					visits_element.appendChild(visit_element);
				}

				break;

			case STATION_DELAY: //TODO: è uguale al caso Li ad eccezione del nome (forse si può semplificare)

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
					st_element.setAttribute("customerclass", class_name);;
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
					st_element.setAttribute("customerclass", class_name);;
					//TODO: questa parte va cambiata, devo avere una stringa csv con tutti i serv times (tranne l'elem 0 che è nullo)

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

	private Element makeSolutionElement(Document root) {
		//TODO: non sono compresi i risultati aggregati (quelli che prendo sono giusti?? gli aggregati sono gli ultimi??)
		Element result_element = root.createElement("solutions");
		result_element.setAttribute("ok", "true");
		result_element.setAttribute("solutionMethod", "analytical");
		for (int i = 0; i < stations; i++) {
			Element stationresults_element = (Element) result_element.appendChild(root.createElement("stationresults"));
			stationresults_element.setAttribute("station", this.stationNames[i]);
			for (int j = 0; j < classes; j++) {
				Element classesresults_element = (Element) stationresults_element.appendChild(root.createElement("classresults"));
				classesresults_element.setAttribute("customerclass", classNames[j]);

				Element Q_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
				Q_element.setAttribute("measureType", "Queue length");
				Q_element.setAttribute("successful", "true");
				Q_element.setAttribute("meanValue", Double.toString(this.queueLen[i][j]));

				Element X_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
				X_element.setAttribute("measureType", "Throughput");
				X_element.setAttribute("successful", "true");
				X_element.setAttribute("meanValue", Double.toString(this.throughput[i][j]));

				Element R_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
				R_element.setAttribute("measureType", "Response time");
				R_element.setAttribute("successful", "true");
				R_element.setAttribute("meanValue", Double.toString(this.resTimes[i][j]));

				Element U_element = (Element) classesresults_element.appendChild(root.createElement("measure"));
				U_element.setAttribute("measureType", "Utilization");
				U_element.setAttribute("successful", "true");
				U_element.setAttribute("meanValue", Double.toString(this.util[i][j]));
			}
		}
		return result_element;
	}

	private void appendMatrixCSV(Document root, Element base, double[][] arr, String outer, String inner) {
		//TODO: forse devo usare questo per trattare anche il caso LD
		Element elems, elem;
		int n = arr.length;
		elems = root.createElement(outer);
		base.appendChild(elems);
		for (int i = 0; i < n; i++) {
			elem = root.createElement(inner);
			//TODO: separa i diversi elementi dell'array con ";"
			elem.appendChild(root.createTextNode(ArrayUtils.toCSV(arr[i])));
			elems.appendChild(elem);
		}
	}

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

		//load solution
		if (solList.item(0) != null) {

			if (!loadSolution((Element) solList.item(0))) {
				return false;
			}
			hasResults = true;

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
					//TODO: maxpop o maxpop+1 ??????
					//serviceTimes[stationNum] = new double[classes][maxpop + 1];
					serviceTimes[stationNum] = new double[classes][maxpop];
				}

				//Element sTimesElem = (Element) current.getElementsByTagName("servicetimes").item(0);
				//TODO: non funziona
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

					for (int p = 0; p < maxpop; p++) {
						serviceTimes[stationNum][k][p] = servt_arr[p];
					}
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

	public boolean loadSolution(Element sol) {

		String status = sol.getAttribute("ok");
		resultsOK = (status.equals("true") ? true : false);

		queueLen = loadResultsMatrix(sol, stations, classes, "Queue length");
		if (queueLen == null) {
			return false;
		}
		throughput = loadResultsMatrix(sol, stations, classes, "Throughput");
		if (throughput == null) {
			return false;
		}
		resTimes = loadResultsMatrix(sol, stations, classes, "Response time");
		if (resTimes == null) {
			return false;
		}
		util = loadResultsMatrix(sol, stations, classes, "Utilization");
		if (util == null) {
			return false;
		}

		return true;
	}

	//NEW
	//@author Stefano Omini
	//TODO: nuovo schema JMTmodel.xsd
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
	public double[] getPerClassX() {
		if (throughput == null) {
			return null;
		} else {
			double[] retVal = new double[classes];
			//scan columns to get one value per column
			for (int i = 0; i < retVal.length; i++) {
				//scan cells of each column
				for (int j = 0; j < throughput.length; j++) {
					//throughput is ratio of specific throughput on specific num of visits
					if (visits[j][i] != 0) {
						retVal[i] = throughput[j][i] / visits[j][i];
						break;
					} else {
						//if all visits for a class (why is this included in model???)
						//throughput for that class is 0
						if (j == throughput.length - 1) {
							retVal[i] = 0;
						}
					}
				}
			}
			return retVal;
		}
	}

	/**Returns per-station aggregate for throughput*/
	public double[] getPerStationX() {
		if (throughput == null) {
			return null;
		} else {
			double[] retVal = new double[stations];
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				for (int j = 0; j < throughput[i].length; j++) {
					retVal[i] += throughput[i][j];
				}
			}
			return retVal;
		}
	}

	/**Returns global aggregate for throughput*/
	public double getGlobalX() {
		if (throughput == null) {
			return Double.NaN;
		} else {
			double retVal = 0;
			double[] aggs = getPerClassX();
			if (aggs != null) {
				for (double agg : aggs) {
					retVal += agg;
				}
				return retVal;
			} else {
				return Double.NaN;
			}
		}
	}

	/**Returns per-class aggregate for queue lenghts*/
	public double[] getPerClassQ() {
		if (queueLen == null) {
			return null;
		} else {
			double[] retVal = new double[classes];
			//first scan columns
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				//then rows
				for (double[] element : queueLen) {
					retVal[i] += element[i];
				}
			}
			return retVal;
		}
	}

	/**Returns per-station aggregate for queue lenghts*/
	public double[] getPerStationQ() {
		if (queueLen == null) {
			return null;
		} else {
			double[] retVal = new double[stations];
			for (int i = 0; i < queueLen.length; i++) {
				retVal[i] = 0;
				for (int j = 0; j < queueLen[i].length; j++) {
					retVal[i] += queueLen[i][j];
				}
			}
			return retVal;
		}
	}

	/**Returns global aggregate for queue lenghts*/
	public double getGlobalQ() {
		if (queueLen == null) {
			return Double.NaN;
		} else {
			double retVal = 0;
			double[] aggs = getPerClassQ();
			if (aggs != null) {
				for (double agg : aggs) {
					retVal += agg;
				}
				return retVal;
			} else {
				return Double.NaN;
			}
		}
	}

	/**Returns per-class aggregate for residence times*/
	public double[] getPerClassR() {
		if (resTimes == null) {
			return null;
		} else {
			double[] retVal = new double[classes];
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				for (double[] resTime : resTimes) {
					retVal[i] += resTime[i];
				}
			}
			return retVal;
		}
	}

	/**Returns per-station aggregate for residence times*/
	public double[] getPerStationR() {
		if (resTimes == null) {
			return null;
		} else {
			double[] retVal = new double[stations];
			double[] xClassAggs = getPerClassX();
			double xGlobal = getGlobalX();
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				for (int j = 0; j < resTimes[i].length; j++) {
					if (xClassAggs != null) {
						retVal[i] += xClassAggs[j] * resTimes[i][j];
					} else {
						return null;
					}
				}
				if (xGlobal != 0) {
					retVal[i] /= xGlobal;
				} else {
					retVal[i] = 0;
				}
			}
			return retVal;
		}
	}

	/**Returns system response time*/
	public double getGlobalR() {
		if (resTimes == null) {
			return Double.NaN;
		} else {
			double retVal = 0;
			double[] aggs = getPerStationR();
			if (aggs != null) {
				for (double agg : aggs) {
					retVal += agg;
				}
				return retVal;
			} else {
				return Double.NaN;
			}
		}
	}

	/**Returns per-class aggregate for utilization*/
	public double[] getPerClassU() {
		if (util == null) {
			return null;
		} else {
			double[] retVal = new double[classes];
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				for (double[] element : util) {
					retVal[i] += element[i];
				}
			}
			return retVal;
		}
	}

	public double[] getPerStationU() {
		if (util == null) {
			return null;
		} else {
			double[] retVal = new double[stations];
			for (int i = 0; i < retVal.length; i++) {
				retVal[i] = 0;
				for (int j = 0; j < util[i].length; j++) {
					retVal[i] += util[i][j];
				}
			}
			return retVal;
		}
	}

	public double getGlobalU() {
		if (util == null) {
			return Double.NaN;
		} else {
			double retVal = 0;
			double[] aggs = getPerStationU();
			if (aggs != null) {
				for (double agg : aggs) {
					retVal += agg;
				}
				return retVal;
			} else {
				return Double.NaN;
			}
		}
	}

	/**
	 * Randomizes model's service times and visits.
	 * <br>Modified by Bertoli Marco to limit service times to two decimal
	 *
	 */
	public void randomizeModelData() {
		double globRate = globalArrRate();
		for (int i = 0; i < serviceTimes.length; i++) {
			for (int j = 0; j < serviceTimes[i].length; j++) {
				for (int k = 0; k < serviceTimes[i][j].length; k++) {
					if (j < classTypes.length) {
						if (classTypes[j] == CLASS_CLOSED) {
							serviceTimes[i][j][k] = Math.round(MAXRAND * Math.exp(-Math.random() * MAXRANGE) * 100) / 100.0;
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

}
