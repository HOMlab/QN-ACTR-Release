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

package jmt.engine.testSystem;

import java.io.File;
import java.util.Random;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.framework.data.ArrayUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * This class creates models with random parameters.
 * Models can be saved into xml file, according to JMTmodel.xsd schema
 *
 * @author Stefano Omini
 */
public class RandomMVAModelGenerator {

	private static int counter = 0;

	public static final int CLOSED_MODEL = 0;
	public static final int OPEN_MODEL = 1;
	public static final int MIXED_MODEL = 2;

	public static final int STATION_DELAY = 0;
	public static final int STATION_LI = 1; //load independent
	public static final int STATION_LD = 2; //load dependent
	public static final String[] STATION_TYPENAMES_CLOSED = { "delay", "LI", "LD" };
	public static final String[] STATION_TYPENAMES = { "delay", "LI" };

	public static final int CLASS_CLOSED = 0;
	public static final int CLASS_OPEN = 1;
	public static final String[] CLASS_TYPENAMES = { "closed", "open" };

	//see constants above: CLOSED_MODEL, OPEN_MODEL, MIXED_MODEL
	private int modelType;

	//true if the model is closed
	private boolean closed;
	//true if the model is open
	private boolean open;
	//true if the model is mixed
	private boolean mixed;

	//true if the model contains load dependent stations
	private boolean ld;

	//description of the model
	private String description;

	private Document modelDOM = null;

	//generator of random numbers
	private Random randomGenerator = null;
	//seed of random generator
	private long seed;

	//if only FCFS service strategy is available, to have a BCMP model
	//jSIM must have, for each station, the same exp distribution

	//private static final boolean onlyFCFS = true;
	private static final boolean onlyFCFS = true;

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

	/***********************SOME CONSTRAINTS****************************/

	//max number of class
	public static final int maxNumber_Classes = 5;
	//max number of stations
	public static final int maxNumber_Station = 15;

	//FOR OPEN MODELS, we want to set the Umax, that is the bottleneck resource utilization,
	//so that it's contained in a fixed interval [min, max]
	private double U_min_bottleneck = 0.3;
	private double U_max_bottleneck = 0.85;

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

	/*******************************************************************/

	/**
	 * Creates a random model.
	 * @param classNumber number of classes (max is <tt>maxNumber_classes</tt>)
	 * @param stationNumber number of stations (max is <tt>maxNumber_stations</tt>)
	 * @param type model type (see constants)
	 * @param seed the seed of random generator (if -1, a random seed will be used)
	 */
	public RandomMVAModelGenerator(int classNumber, int stationNumber, int type, long seed) {

		counter++;

		if (seed == -1) {
			//automatic seed
			//uses current time as seed
			seed = System.currentTimeMillis();
			randomGenerator = new Random(seed);
		} else {
			randomGenerator = new Random(seed);
		}

		this.seed = seed;

		description = "created with RandomMVAModelGenerator: model n. " + counter + " (seed = " + Long.toString(seed) + ")";

		classes = classNumber;
		if (classes > maxNumber_Classes) {
			classes = maxNumber_Classes;
		}

		stations = stationNumber;
		if (stations > maxNumber_Station) {
			stations = maxNumber_Station;
		}

		modelType = type;

		createNames();

		randomVisits();
		randomServiceTimes();

		randomStationTypes();
		randomClassTypes();

		createClassData();
		calcModelType();

		createDocument();

		saveToFile();

	}

	/**
	 * Generates random numbers to fill the matrix of service times
	 * (LD case not supported)
	 *
	 */
	private void randomServiceTimes() {
		serviceTimes = new double[stations][classes][1];

		if (onlyFCFS) {

			//for each station, all classes must have the same service time
			//loop over classes
			for (int s = 0; s < stations; s++) {

				double temp = Math.abs(randomGenerator.nextDouble());
				//loop over classes and set the same service time
				for (int c = 0; c < classes; c++) {
					serviceTimes[s][c][0] = temp;
				}

			}

			//do not normalize service times

		} else {

			//loop over classes
			for (int c = 0; c < classes; c++) {
				double max = 0.0;
				double temp;

				//loop over stations and saves the maximum service time
				for (int i = 0; i < stations; i++) {
					temp = Math.abs(randomGenerator.nextDouble());
					serviceTimes[i][c][0] = temp;
					//determines whether it's the maximum value for this class
					if (temp > max) {
						max = temp;
					}
				}
				//Normalize values so that the max service time for this class becomes 1
				for (int i = 0; i < stations; i++) {
					serviceTimes[i][c][0] /= max;
				}
			}

		}

	}

	/**
	 * Generates random numbers to fill the matrix of visits
	 */
	private void randomVisits() {
		visits = new double[stations][classes];
		double temp;

		//loop over classes
		for (int c = 0; c < classes; c++) {
			//loop over stations
			for (int i = 0; i < stations; i++) {
				do {
					temp = Math.abs(randomGenerator.nextDouble());
					if (temp <= 0.5) {
						visits[i][c] = temp * 2;
					}
				} while (temp > 0.5 || temp == 0.0);
			}
		}

	}

	/**
	 * Generates random station types to fill the vector of station types (LI, Delay).
	 * LD service centers not supported yet.
	 *
	 * According to some articles in QN literature, the proportion is LI 95% and Delay 5%
	 * (see "Lineariser")
	 *
	 */
	private void randomStationTypes() {

		stationTypes = new int[stations];
		double temp;

		//at least one LI station
		stationTypes[0] = STATION_LI;

		//loop over stations
		for (int i = 1; i < stations; i++) {
			temp = Math.abs(randomGenerator.nextDouble());
			if (temp < 0.95) {
				stationTypes[i] = STATION_LI;
			} else {
				stationTypes[i] = STATION_DELAY;
			}
		}
	}

	/**
	 * Creates class and station names (Class0, Class1, ..., Station0, Station1, ...)
	 */
	private void createNames() {

		classNames = new String[classes];

		for (int c = 0; c < classes; c++) {
			classNames[c] = "Class" + c;
		}

		stationNames = new String[stations];

		for (int s = 0; s < stations; s++) {
			stationNames[s] = "Station" + s;
		}

		return;

	}

	private void randomClassTypes() {

		classTypes = new int[classes];

		switch (modelType) {
			case CLOSED_MODEL:
				for (int c = 0; c < classes; c++) {
					classTypes[c] = CLASS_CLOSED;
				}
				break;
			case OPEN_MODEL:
				for (int c = 0; c < classes; c++) {
					classTypes[c] = CLASS_OPEN;
				}
				break;
			case MIXED_MODEL:
				for (int c = 0; c < classes; c++) {
					if (randomGenerator.nextDouble() < 0.5) {
						classTypes[c] = CLASS_CLOSED;
					} else {
						classTypes[c] = CLASS_OPEN;
					}
				}
				break;
		}
	}

	private void calcModelType() {

		closed = open = false;

		for (int c = 0; c < classes; c++) {
			if (classTypes[c] == CLASS_CLOSED) {
				closed = true;
			} else if (classTypes[c] == CLASS_OPEN) {
				open = true;
			}
		}

		if (closed && open) {
			//both open and closed classes -> mixed = true
			mixed = true;
			closed = open = false;
		}

	}

	private void createClassData() {

		classData = new double[classes];

		switch (modelType) {
			case CLOSED_MODEL:
				//only closed classes

				for (int c = 0; c < classes; c++) {
					//computes total think time (i.e. the sum of all delays for this class)
					double z = 0.0;
					double maxD = 0.0;
					double temp;

					for (int s = 0; s < stations; s++) {
						temp = serviceTimes[s][c][0] * visits[s][c];

						//computes Dmax
						if (temp > maxD) {
							maxD = temp;
						}

						if (stationTypes[s] == STATION_DELAY) {
							z += temp;
						}

					}

					//the chosen population is influenced by think time and Dmax
					classData[c] = Math.floor(50 * randomGenerator.nextDouble() + z / maxD + 1);

				}
				break;

			case OPEN_MODEL:

				//only open classes

				for (int c = 0; c < classes; c++) {
					//sets lambda from [0,1)
					classData[c] = randomGenerator.nextDouble();
				}

				//computes the actual Umax
				double Umax = 0.0;

				for (int s = 0; s < stations; s++) {

					double Ustation = 0.0;

					//do not consider delay stations
					if (stationTypes[s] == STATION_LI) {
						for (int c = 0; c < classes; c++) {
							Ustation += classData[c] * visits[s][c] * serviceTimes[s][c][0];
						}

						//is Umax?
						if (Ustation > Umax) {
							Umax = Ustation;
						}
					}
				}

				//chooses the wanted Umax*, included in [U_min_bottleneck , U_max_bottleneck] (example: [0.1 , 0.7])
				double Umax_desired = U_min_bottleneck + randomGenerator.nextDouble() * (U_max_bottleneck - U_min_bottleneck);

				//then scales all the service times of LI stations in order to obtain that U*
				double scaleFactor = Umax_desired / Umax;

				for (int s = 0; s < stations; s++) {
					if (stationTypes[s] == STATION_LI) {
						for (int c = 0; c < classes; c++) {
							serviceTimes[s][c][0] *= scaleFactor;
						}
					}
				}
				break;

			case MIXED_MODEL:

				//both open and closed classes

				//first of all, sets the lambda
				for (int c = 0; c < classes; c++) {

					if (classTypes[c] == CLASS_OPEN) {
						//sets lambda from [0,1)
						double rand;
						do {
							rand = randomGenerator.nextDouble();
							classData[c] = rand;
						} while (rand == 0);
					}
				}

				//computes the actual Umax
				double Umax2 = 0.0;

				for (int s = 0; s < stations; s++) {

					double Ustation = 0.0;
					//do not consider delay stations
					if (stationTypes[s] == STATION_LI) {
						for (int c = 0; c < classes; c++) {
							if (classTypes[c] == CLASS_OPEN) {
								Ustation += classData[c] * visits[s][c] * serviceTimes[s][c][0];
							}
						}
						//is Umax?
						if (Ustation > Umax2) {
							Umax2 = Ustation;
						}
					}
				}

				//if only closed class are present, Umax2 will be 0
				//therefore there is no need to rescale service times

				//if Umax2 > 0 rescale service times 
				if (Umax2 > 0) {

					//chooses the wanted Umax*, included in [U_min_bottleneck , U_max_bottleneck] (example: [0.1 , 0.7])
					double Umax_desired2 = U_min_bottleneck + randomGenerator.nextDouble() * (U_max_bottleneck - U_min_bottleneck);

					//then scales all the service times in order to obtain that U*
					double scaleFactor2 = Umax_desired2 / Umax2;

					for (int s = 0; s < stations; s++) {
						if (stationTypes[s] == STATION_LI) {
							for (int c = 0; c < classes; c++) {
								serviceTimes[s][c][0] *= scaleFactor2;
							}
						}
					}

				}

				//now consider closed classes
				for (int c = 0; c < classes; c++) {
					if (classTypes[c] == CLASS_CLOSED) {

						double z = 0.0;
						double maxD = 0.0;
						double temp;

						for (int s = 0; s < stations; s++) {
							temp = serviceTimes[s][c][0] * visits[s][c];

							if (temp > maxD) {
								maxD = temp;
							}

							z += temp;
						}

						classData[c] = Math.floor(50 * randomGenerator.nextDouble() + z / maxD + 1);

					}
				}
				break;

		}

	}

	/**
	 * Creates a DOM representation of this object
	 *
	 */
	public void createDocument() {
		Document root;
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			root = dbf.newDocumentBuilder().newDocument();
		} catch (ParserConfigurationException pce) {
			throw new RuntimeException(pce);
		}

		/* model */
		Element modelElement = root.createElement("model");
		//TODO: xmlns:xsi ??
		modelElement.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");

		//TODO: xsi:noNamespace... ??
		//modelElement.setAttribute("xsi:noNamespaceSchemaLocation", "JMTmodel.xsd");
		modelElement.setAttribute("xsi:noNamespaceSchemaLocation", XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION));

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
			//TODO: bisogna modificare per tenere conto di LD e DELAY
			//OLD
			//stationsElement.appendChild(makeStationElement(root, i));
			stationsElement.appendChild(makeStationElement(root, i));
		}

		modelDOM = root;

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

	//TODO: nuova per gestire anche LD e DELAY
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

	/**
	 * Saves the random model into an xml file
	 * @return the File containing the random model
	 */
	public File saveToFile() {

		/////////////////
		//Output the XML
		try {
			//set up a transformer
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer trans = transfac.newTransformer();
			trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
			trans.setOutputProperty(OutputKeys.INDENT, "yes");

			String modelT = "";

			switch (modelType) {
				case CLOSED_MODEL:
					modelT = "closed";
					break;
				case OPEN_MODEL:
					modelT = "open";
					break;
				case MIXED_MODEL:
					modelT = "mixed";
					break;

			}

			File modelFile = new File("randomModel_" + modelT + "_" + counter + ".xml");

			DOMSource source = new DOMSource(modelDOM);

			// Prepare the output file
			File temp = File.createTempFile("~jmva_randomModel", ".xml", modelFile.getParentFile());
			StreamResult result = new StreamResult(temp);

			// Write the DOM document to the file
			trans.transform(source, result);

			// commit
			if (modelFile.exists()) {
				modelFile.delete();
			}

			temp.renameTo(modelFile);

			return modelFile;

		} catch (javax.xml.transform.TransformerConfigurationException exc) {
			exc.printStackTrace();
		} catch (javax.xml.transform.TransformerException exc) {
			exc.printStackTrace();
		} catch (java.io.IOException exc) {
			exc.printStackTrace();
		}

		return null;

	}

	/**
	 * Generates the specified number of models of the specified type and save them to files.
	 * @param classN number of classes of the model
	 * @param statN number of stations of the model
	 * @param modelN number of models
	 * @param modelType model type
	 */
	public static void testGenerator(int classN, int statN, int modelN, int modelType) {

		RandomMVAModelGenerator randomGen;
		File model;

		for (int m = 0; m < modelN; m++) {
			//Creates a random model of the specified type
			//uses automatic seed for model generation
			randomGen = new RandomMVAModelGenerator(classN, statN, modelType, -1);
			model = randomGen.saveToFile();

		}

	}

	/**
	 * Generates a models of the specified type and save it to file.
	 * @param classN number of classes of the model
	 * @param statN number of stations of the model
	 * @param fixedSeed seed to be used to generate job
	 * @param modelType model type
	 */
	public static void testGenerator_fixed(int classN, int statN, int modelType, long fixedSeed) {

		//Creates a random model of the specified type
		//uses the fixed seed for model generation
		RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, fixedSeed);
		File model = randomGen.saveToFile();

	}

}
