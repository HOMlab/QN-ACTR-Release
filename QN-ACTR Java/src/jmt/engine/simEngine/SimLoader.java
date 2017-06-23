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

package jmt.engine.simEngine;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import javax.xml.parsers.DocumentBuilderFactory;

import jmt.common.exception.LoadException;
import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.engine.NodeSections.InputSection;
import jmt.engine.NodeSections.OutputSection;
import jmt.engine.NodeSections.ServiceSection;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.QueueNet.SimConstants;
import jmt.engine.dataAnalysis.InverseMeasure;
import jmt.engine.dataAnalysis.Measure;
import jmt.engine.dataAnalysis.SimParameters;
import jmt.engine.random.Distribution;
import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

/**
 * This class contains the methods to load the DOM description of a
 * queueing netwok model from a XML file and then to create a Simulation object
 * from that description.
 *
 * For each node, all parameters and sub-parameters are loaded using the suitable constructors.
 *
 * @author Federico Granata, Stefano Omini, Bertoli Marco
 * @version 26-ago-2003 14.23.27
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
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 */
public class SimLoader {

	//used for debug pourposes
	private static final boolean DEBUG = false;

	//represents the entire XML document. it is the root
	//of the document tree, and provides the primary access to the document's data
	Document document;

	//customer class array
	private JobClass[] jobClasses;
	// simulation object created by this loader
	private Simulation sim;

	//path of the xml file containing the sim model
	private String simModelPath;

	/**
	 * Creates a Simulation object, loading all the model definition from the
	 * passed xml file
	 *
	 * @param xmlPath the <em>absolute</em> path of the xml model definition
	 */
	public SimLoader(String xmlPath) throws IOException, LoadException {

		//NEW
		//@author Stefano Omini

		simModelPath = xmlPath;
		InputStream is = new BufferedInputStream(new FileInputStream(xmlPath));
		//loads the model and creates a Simulation object
		load(is);

		//end NEW

	}

	private void load(InputStream is) throws LoadException {

		if (is == null) {
			throw new LoadException("File not Found");
		}
		InputSource inputSource = new InputSource(is);
		//create a parser
		DOMParser parser = new DOMParser();

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(true);
		factory.setNamespaceAware(true);

		try {
			// turn on schema validation ( note need to set both sax and dom validation )
			parser.setFeature("http://xml.org/sax/features/validation", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);

			//NEW
			//TODO: setto lo schema xsd con cui fare il parsing
			String externalSchemaLocation = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_DEFINITION);
			parser.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation", externalSchemaLocation);
			//end NEW

			try {
				//document parsing
				parser.parse(inputSource);
			} catch (FileNotFoundException e) {
				throw new LoadException("Problems while parsing", e);

			}

			//get the w3c document
			document = parser.getDocument();
			if (DEBUG) {
				System.out.println(" created document");
			}
			//gets root
			Element root = document.getDocumentElement();
			if (DEBUG) {
				System.out.println("root = " + root.getAttribute("name"));
			}
			//recovers the name of the simulation & creates a getLog with the same
			//name
			if (root.getNodeName() == null) {
				throw new LoadException("Problems loading");
			} else if (!root.getNodeName().equalsIgnoreCase("sim")) {
				throw new LoadException("Problems loading");
			}

			//OLD
			//sim = new Simulation(root.getAttribute("name"), root.getAttribute("debug").equals("true"));

			//NEW
			//@author Stefano Omini

			//default values

			long seed = -1;
			String simName = "";

			if (root.hasAttribute("name")) {
				simName = root.getAttribute("name");
			}

			//vraiable debug is no longer USED

			if (root.getAttribute("seed") != "") {
				seed = Long.parseLong(root.getAttribute("seed"));
			}

			if (simName.equalsIgnoreCase("")) {

				//NEW
				//@author Stefano Omini

				//no name specified: uses current time as name
				String datePattern = "yyyyMMdd_HHmmss";
				SimpleDateFormat formatter = new SimpleDateFormat(datePattern);

				Date today = new Date();
				String todayString = formatter.format(today);

				simName = "jSIM_" + todayString;

				//OLD
				//sim = new Simulation(seed, null, debug);
				sim = new Simulation(seed, simName);

				//end NEW

			} else {

				//OLD
				//sim = new Simulation(seed, simName, debug);
				sim = new Simulation(seed, simName);
			}

			sim.setXmlSimModelDefPath(simModelPath);

			//end NEW

			//-------------- SIM PARAMETERS -------------------//

			//TODO: codice per fissare i sim parameters

			// Create a class SimParameter, whose parameters will be shared by all
			// dynamic data analyzer in order to compute confidence intervals.
			// For example, number of batches, batch size, ecc..

			//this constructor will use default values
			SimParameters simParam = new SimParameters();

			//TODO: qui dovrei mettere blocchi tipo if (has attribute("batch")) then set(..) ecc
			//una volta aggiunti nello schema dell'xml, vanno letti e
			//inseriti coi rispettivi metodi set

			// {......}

			//TODO: finita la parte con parsing e set degli attributi, si mette questo metodo
			//(che per il momento si limita a settare i valori di default)

			//sets the reference in sim object
			sim.setSimParameters(simParam);

			//gets the default value of maxsamples
			//(max number of samples for each measure)
			int maxSamples = simParam.getMaxSamples();

			// Gets the timestamp value
			simParam.setTimestampValue(Long.toString(System.currentTimeMillis()));

			//-------------- end SIM PARAMETERS -------------------//

			// NEW Bertoli Marco -- Read MAX Samples if specified
			if (root.getAttribute("maxSamples") != "") {
				maxSamples = Integer.parseInt(root.getAttribute("maxSamples"));
				simParam.setMaxSamples(maxSamples);
			}
			// END

			// NEW Bertoli Marco -- Disables condidence interval as stopping criteria
			if (root.hasAttribute("disableStatisticStop")) {
				simParam.setDisableStatisticStop(root.getAttribute("disableStatisticStop").equalsIgnoreCase(Boolean.TRUE.toString()));
			}
			//END

			// MF08 0.7.4  Michael Fercu (Bertoli Marco) -- re-defines global logger attributes
			// for the purpose of passing them to the Logger constructor
			if (root.hasAttribute("logPath")) {
				String temp_lp = root.getAttribute("logPath");
				simParam.setLogPath(temp_lp);
			}
			if (root.hasAttribute("logDelimiter")) {
				String temp_ld = root.getAttribute("logDelimiter");
				simParam.setLogDelimiter(temp_ld);

			}
			if (root.hasAttribute("logDecimalSeparator")) {
				String temp_ld = root.getAttribute("logDecimalSeparator");
				simParam.setLogDecimalSeparator(temp_ld);

			}
			if (root.hasAttribute("logReplaceMode")) {
				String temp_lr = root.getAttribute("logReplaceMode");
				simParam.setLogReplaceMode(temp_lr);
			}
			if (root.hasAttribute("lastRunTime")) {
				String temp_ltv = root.getAttribute("lastRunTime");
				simParam.setTimestampValue(temp_ltv);
			}
			//END MF08

			//Returns a NodeList of all the Elements with a given tag name in the order in which they
			//are encountered in a preorder traversal of the Document tree.
			NodeList nodeList = root.getElementsByTagName("node");
			NodeList classList = root.getElementsByTagName("userClass");
			NodeList measureList = root.getElementsByTagName("measure");
			NodeList connectionList = root.getElementsByTagName("connection");

			//class array creation
			jobClasses = new JobClass[classList.getLength()];
			for (int i = 0; i < classList.getLength(); i++) {
				//OLD
				//jobClasses[i] = new JobClass(((Element) classList.item(i)).getAttribute("name"));

				//NEW
				//@author Stefano Omini

				Element currentJobClass = (Element) classList.item(i);

				//parse class attributes: name, type and priority

				String currentClassName = currentJobClass.getAttribute("name");
				String currentClassType = currentJobClass.getAttribute("type");
				String currentClassPriority = currentJobClass.getAttribute("priority");
				String referenceNode = currentJobClass.getAttribute("referenceSource");

				int type, priority;

				if (currentClassType.equalsIgnoreCase("closed")) {
					type = JobClass.CLOSED_CLASS;

					//TODO: al momento non viene letto l'attributo opzionale "customers"
					//(che comunque non � necessario: i job vengono creati dal terminal o precaricati
					//nelle code)
				} else {
					type = JobClass.OPEN_CLASS;
				}

				priority = Integer.parseInt(currentClassPriority);
				if (priority < 0) {
					//negative priorities not allowed
					priority = 0;
				}

				//add job class
				jobClasses[i] = new JobClass(currentClassName, priority, type, referenceNode);

				//end NEW

				if (DEBUG) {
					System.out.println("Class " + jobClasses[i].getName() + " created");
				}
			}
			//inserts all JobClasses in the Simulation object
			sim.addClasses(jobClasses);
			if (DEBUG) {
				System.out.println("classes added\n");
			}

			//creates the nodes from xml & adds them to the simulation object
			for (int i = 0; i < nodeList.getLength(); i++) {
				Element node = (Element) nodeList.item(i);
				if (DEBUG) {
					System.out.println("start creation of node = " + node.getAttribute("name"));
				}
				//gets list of sections
				NodeList sectionList = node.getElementsByTagName("section");
				NodeSection[] sections = new NodeSection[3];
				//creates all sections (max is 3)
				for (int j = 0; j < sectionList.getLength(); j++) {
					if (DEBUG) {
						System.out.println("    start creation of section = " + ((Element) sectionList.item(j)).getAttribute("className"));
					}
					NodeSection ns = createSection((Element) sectionList.item(j));
					if (DEBUG) {
						System.out.println("    finished creation of " + ((Element) sectionList.item(j)).getAttribute("className") + "\n");
					}
					if (ns instanceof InputSection) {
						sections[0] = ns;
					} else if (ns instanceof ServiceSection) {
						sections[1] = ns;
					} else if (ns instanceof OutputSection) {
						sections[2] = ns;
					} else {
						throw new LoadException("trying to cast the wrong Class type");
					}
				}
				//adds node.
				sim.addNode(node.getAttribute("name"), (InputSection) sections[0], (ServiceSection) sections[1], (OutputSection) sections[2]);
				if (DEBUG) {
					System.out.println("node added\n");
				}
			}
			if (DEBUG) {
				System.out.println("");
			}

			//adds all connections
			for (int i = 0; i < connectionList.getLength(); i++) {
				Element e = (Element) connectionList.item(i);
				sim.addConnection(e.getAttribute("source"), e.getAttribute("target"));
				if (DEBUG) {
					System.out.println("added connection = " + e.getAttribute("source") + " to " + e.getAttribute("target"));
				}
			}
			if (DEBUG) {
				System.out.println("");
			}

			//adds all measures
			for (int i = 0; i < measureList.getLength(); i++) {
				Element e = (Element) measureList.item(i);

				String type = e.getAttribute("type");
				//Added variable referenceNode as a step to control backward compatibility in obtainMeasureType to differentiate between
				//new Number of Customers (orig. Queue Length) and old Number of Customers (presently System Number of Customers)
				//because we know that Queue Length is a node level perf index where other one is system and hence has blank node.
				String referenceNode = e.getAttribute("referenceNode");
				int measureType = obtainMeasureType(type, referenceNode);
				String nodeType = e.getAttribute("nodeType"); // Node is a station or a region
				//throughput measure requires an InverseMeasure object!!
				if (measureType == SimConstants.THROUGHPUT || measureType == SimConstants.SYSTEM_THROUGHPUT || measureType == SimConstants.DROP_RATE
						|| measureType == SimConstants.SYSTEM_DROP_RATE
						//Added by ASHANKA START
						//Added System power as an Inverse Measure.
						|| measureType == SimConstants.SYSTEM_POWER
				//Added by ASHANKA STOP
						|| measureType == SimConstants.THROUGHPUT_PER_SINK
				) {
					//throughput measure
					InverseMeasure invMeasure = new InverseMeasure(e.getAttribute("name"), Double.parseDouble(e.getAttribute("alpha")), Double
							.parseDouble(e.getAttribute("precision")), maxSamples, e.getAttribute("verbose").equalsIgnoreCase("true"));

					sim.addMeasure(measureType, e.getAttribute("referenceNode"), invMeasure, e.getAttribute("referenceUserClass"), nodeType);
				} else {
					//other measures
					Measure measure = new Measure(e.getAttribute("name"), Double.parseDouble(e.getAttribute("alpha")), Double.parseDouble(e
							.getAttribute("precision")), maxSamples, e.getAttribute("verbose").equalsIgnoreCase("true"), null);

					sim.addMeasure(measureType, e.getAttribute("referenceNode"), measure, e.getAttribute("referenceUserClass"), nodeType);

				}

				if (DEBUG) {
					System.out.println("added measure = " + e.getAttribute("name"));
				}
			}
			if (DEBUG) {
				System.out.println("");
			}

			//NEW
			//@author Stefano Omini

			NodeList regionList = root.getElementsByTagName("blockingRegion");

			// Use external method
			loadBlockingRegions(root, regionList);
			if (DEBUG) {
				System.out.println("");
			}

			//end NEW

			//Preloading

			NodeList preloadList = root.getElementsByTagName("preload");
			Element preload = (Element) preloadList.item(0);

			//station names
			String[] stationNames;
			//initial populations [station, class]
			int[][] initialP;

			if (preload != null) {
				//preload has been defined by user

				NodeList stations;
				NodeList initialPops;
				Element station;

				//gets all station elements
				stations = preload.getElementsByTagName("stationPopulations");
				//number of stations to be preloaded
				int stationsNumber = stations.getLength();
				int classNumber = jobClasses.length;

				//create the array of station names to be preloaded
				// (not ALL the stations!!)
				stationNames = new String[stationsNumber];
				//create the population matrix
				initialP = new int[stationsNumber][classNumber];

				//initializes matrix
				for (int s = 0; s < stationsNumber; s++) {
					for (int c = 0; c < classNumber; c++) {
						initialP[s][c] = 0;
					}
				}

				//loop over stations
				for (int s = 0; s < stationsNumber; s++) {
					//current station
					station = (Element) stations.item(s);

					//set station name
					stationNames[s] = station.getAttribute("stationName");

					//retrieves the class initial populations
					initialPops = station.getElementsByTagName("classPopulation");
					int entries = initialPops.getLength();

					for (int c = 0; c < entries; c++) {
						String pop = ((Element) initialPops.item(c)).getAttribute("population");
						String className = ((Element) initialPops.item(c)).getAttribute("refClass");

						int jobClassPosition = getJobClassPosition(className);
						if (jobClassPosition == -1) {
							//the specified name does not correspond to any existing job class
							//ignore this preload entry
							continue;
						} else {
							//sets the value in the correct position (using the class ID)
							initialP[s][jobClassPosition] = Integer.parseInt(pop);
						}
					}
				}

				//copies the preload info into simulation object

				sim.setPreloadEnabled(true);
				sim.setPreload_stationNames(stationNames);
				sim.setPreload_initialPopulations(initialP);

			}

			//end NEW

		} catch (SAXNotRecognizedException e) {
			e.printStackTrace();
		} catch (SAXNotSupportedException e) {
			e.printStackTrace();
		} catch (SAXException sxe) {
			// Error generated during parsing)
			Exception x = sxe;
			if (sxe.getException() != null) {
				x = sxe.getException();
			}
			x.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (jmt.common.exception.NetException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Returns the Simulation object which has been created by this SimLoader while loading the
	 * queueing network model.
	 * @return The simulation created
	 */
	public Simulation getSim() {
		return sim;
	}

	/**
	 * Creates a nodeName section from a dom description. Each parameter (or array of parameters)
	 * is created using the suitable constructor.
	 *
	 * @param section  dom description of the nodeName section.
	 * @return NodeSection created.
	 */
	private NodeSection createSection(Element section) throws LoadException {
		//gets all the parameters
		NodeList parameterList = section.getElementsByTagName("parameter");
		try {
			//gets appropriate Class Object
			Class<?> c = Class.forName("jmt.engine.NodeSections." + section.getAttribute("className"));
			if (DEBUG) {
				System.out.println("    class found");
			}
			//creates with Default Constructor
			if (parameterList.getLength() == 0) {
				if (DEBUG) {
					System.out.println("    using default constructor");
				}
				return (NodeSection) c.newInstance();

			} else {
				//creates the array with all parameters & the array with all their classes
				Object[] initargs = new Object[parameterList.getLength()];
				Class<?>[] parameterTypes = new Class[parameterList.getLength()];
				for (int i = 0; i < parameterList.getLength(); i++) {
					//creates the parameter
					initargs[i] = createParameter((Element) parameterList.item(i));
					if (initargs[i] != null) {
						//gets the class of the parameter
						parameterTypes[i] = initargs[i].getClass();
					} else {
						if (!((Element) parameterList.item(i)).getAttribute("array").equals("true")) {
							//the parameter is not an array
							parameterTypes[i] = Class.forName(((Element) parameterList.item(i)).getAttribute("classPath"));
						} else {
							// array parameter
							parameterTypes[i] = Array.newInstance(Class.forName(((Element) parameterList.item(i)).getAttribute("classPath")), 0)
									.getClass();
						}
						if (DEBUG) {
							System.out.println("        parameter of class = " + parameterTypes[i].getName());
						}
					}
				}
				//gets the right constructor
				Constructor<?> constr = getConstructor(c, parameterTypes);
				if (DEBUG) {
					System.out.println("    constructor found");
				}
				//creates the Section with the constructor
				return (NodeSection) constr.newInstance(initargs);
			}
		} catch (ClassNotFoundException e) {
			throw new LoadException("Class of Section Not found", e);
		} catch (InstantiationException e) {
			throw new LoadException("Class of Section cannot be istantiated", e);
		} catch (IllegalAccessException e) {
			throw new LoadException("Class of Section illegal access", e);
		} catch (NoSuchMethodException e) {
			throw new LoadException("Constructor of Section not found", e);
		} catch (InvocationTargetException e) {
			throw new LoadException("problems with Section constructor", e);
		}
	}

	/**
	 * creates a parameter object given the corresponding description of the tag.
	 * @param param
	 * @throws jmt.common.exception.LoadException
	 */
	private Object createParameter(Element param) throws LoadException {
		if (!param.getTagName().equals("parameter")) {
			throw new LoadException("trying to use createParameter on a : " + param.getTagName());
		}
		try {
			if (DEBUG) {
				System.out.println("        start creation of parameter = " + param.getAttribute("name"));
			}
			//gets Class Object for the parameter
			String classPath = param.getAttribute("classPath");
			NodeList valueList = XMLParser.getElementsByTagName(param, "value");
			//if value == null the object is a null pointer
			if (valueList.getLength() > 0 && valueList.item(0).getChildNodes().item(0).getNodeValue().equals("null")) {
				if (DEBUG) {
					System.out.println("        parameter null");
				}
				return null;
			}
			Class<?> c = Class.forName(classPath);
			if (DEBUG) {
				System.out.println("        parameter  class found = " + classPath);
			}
			//parameter is an array;
			if (param.getAttribute("array").equals("true")) {
				if (DEBUG) {
					System.out.println("        parameter is an array");
				}
				//check if there are child nodes in the array
				if (!param.hasChildNodes()) {
					if (DEBUG) {
						System.out.println("        creates 0 size array");
					}
					return Array.newInstance(c, 0);//return a 0 element array
				}
				//there are 2 situations: the parameter is an array or a group of subparameters defined per class

				//first situation: just an array without classes
				Object[] arrayElements;
				if (XMLParser.getElementsByTagName(param, "refClass").getLength() == 0) {
					//gets list of first level subParameters
					NodeList childList = XMLParser.getElementsByTagName(param, "subParameter");
					//creates an array of the appropriate length
					arrayElements = new Object[childList.getLength()];
					if (DEBUG) {
						System.out.println("        instance a simple array of size " + arrayElements.length);
					}
					//creates all the elements of the array
					for (int i = 0; i < arrayElements.length; i++) {
						if (DEBUG) {
							System.out.println("creating subparemter = " + ((Element) childList.item(i)).getAttribute("name"));
						}
						arrayElements[i] = createSubParameter((Element) childList.item(i));
					}
					//creates a fake array object
					Object parameter = Array.newInstance(c, childList.getLength());
					//copy inside all the elements
					System.arraycopy(arrayElements, 0, parameter, 0, arrayElements.length);
					if (DEBUG) {
						System.out.println("        created parameter");
					}
					return parameter;

				} else {

					//it's a group parameter! more complicated(the other was easy... :P )
					if (DEBUG) {
						System.out.println("        it's group class parameter");
					}
					//creates an array of Object that has enough element to store
					//subParametes for each class.
					arrayElements = new Object[jobClasses.length];
					NodeList chiList = param.getChildNodes();
					ArrayList<String> classVect = new ArrayList<String>();
					//iterates over the childList, it's like this, it has a list (optional)of classRef
					//followed by a subParameter
					for (int i = 0; i < chiList.getLength(); i++) {
						Node n = chiList.item(i);
						if (n.getNodeType() == 1) {
							//if gets a class it add to list of classes
							if (n.getNodeName().equals("refClass")) {
								classVect.add(n.getFirstChild().getNodeValue());
								if (DEBUG) {
									System.out.println("        found class " + n.getFirstChild().getNodeValue());
								}
							} else {
								//gets the position of classes
								int[] positions = new int[classVect.size()];
								for (int j = 0; j < positions.length; j++) {
									positions[j] = findClassPosition(classVect.get(j));
								}
								//gets the subparameter
								for (int j = 0; j < positions.length; j++) {
									if (DEBUG) {
										System.out.println("        creating subParameter " + ((Element) n).getAttribute("name") + " for class "
												+ classVect.get(j));
									}
									arrayElements[positions[j]] = createSubParameter((Element) n);
								}
								//clears the classes vector
								classVect.clear();
							}
						}
					}
					//creates a fake array object
					Object parameter = Array.newInstance(c, jobClasses.length);
					//copy inside all the elements
					System.arraycopy(arrayElements, 0, parameter, 0, arrayElements.length);
					if (DEBUG) {
						System.out.println("        created parameter");
					}
					return parameter;
				}
			}
			//check for default constructor
			if (!param.hasChildNodes()) {
				if (DEBUG) {
					System.out.println("       created with default constructor");
				}
				return c.newInstance();
			}

			//check if it's a leaf node (it has a value & not subparameters)
			if (valueList != null) {

				String value = valueList.item(0).getFirstChild().getNodeValue();
				if (DEBUG) {
					System.out.println("        parameter is a leaf node, value = " + value);
				}
				//needs to get the String constructor
				Object[] initargs = { value };
				Class<?>[] paramterTypes = { initargs[0].getClass() };
				Constructor<?> constr = getConstructor(c, paramterTypes);
				if (DEBUG) {
					System.out.println("        created parameter");
				}
				return constr.newInstance(initargs);
			} else {
				//leaf node but has subparameters
				NodeList childList = XMLParser.getElementsByTagName(param, "subParameter");
				Object[] initargs = new Object[childList.getLength()];
				if (DEBUG) {
					System.out.println("        parameter is a leaf node with subparameter ");
				}
				Class<?>[] paramClasses = new Class[childList.getLength()];
				//creates iteratevely all the subparamters
				for (int i = 0; i < childList.getLength(); i++) {
					Element e = (Element) childList.item(i);
					if (DEBUG) {
						System.out.println("            creates subparameter = " + e.getAttribute("name"));
					}
					initargs[i] = createSubParameter(e);
					paramClasses[i] = initargs[i].getClass();
				}
				Constructor<?> constr = getConstructor(c, paramClasses);
				return constr.newInstance(initargs);
			}

		} catch (ClassNotFoundException e) {
			throw new LoadException("Class of Parameter not found", e);
		} catch (NoSuchMethodException e) {
			throw new LoadException("there is not a String Constructor", e);
		} catch (InstantiationException e) {
			throw new LoadException("Class of parameter cannot be instantiated", e);
		} catch (IllegalAccessException e) {
			throw new LoadException("Class of parameter cannot be instantiated", e);
		} catch (InvocationTargetException e) {
			throw new LoadException("Class of parameter cannot be instantiated", e);
		}
	}

	/**
	 * Creates a subParameter given an appropriate Element of a DOM
	 * @param subp
	 * @return
	 * @throws jmt.common.exception.LoadException
	 */
	private Object createSubParameter(Element subp) throws LoadException {
		//gets Class object
		try {
			NodeList valueList = XMLParser.getElementsByTagName(subp, "value");
			//if value == null the object is a null pointer
			if (valueList.getLength() > 0 && valueList.item(0).getChildNodes().item(0).getNodeValue().equals("null")) {
				if (DEBUG) {
					System.out.println("         subParameter null");
				}
				return null;
			}
			Class<?> c = Class.forName(subp.getAttribute("classPath"));
			if (DEBUG) {
				System.out.println("            found subparameter class = " + c.getName());
			}
			//check is subp is an array
			if (subp.getAttribute("array").equals("true")) {
				//array subparameter
				if (DEBUG) {
					System.out.println("            the subParameter is an array");
				}
				//check if there r childs in the array
				if (!subp.hasChildNodes()) {
					if (DEBUG) {
						System.out.println("            creates 0 size array");
					}
					return Array.newInstance(c, 0);//return a 0 element array
				}
				Object[] arrayElements;
				//gets list of first level subParameters
				NodeList childList = XMLParser.getElementsByTagName(subp, "subParameter");
				//creates an array of the appropriate length
				arrayElements = new Object[childList.getLength()];
				if (DEBUG) {
					System.out.println("            instance a simple array of size " + arrayElements.length);
				}
				//creates all the elements of the array
				for (int i = 0; i < arrayElements.length; i++) {
					if (DEBUG) {
						System.out.println("            creating subparameter = " + ((Element) childList.item(i)).getAttribute("name"));
					}
					arrayElements[i] = createSubParameter((Element) childList.item(i));
				}
				//reates a fake array object
				Object parameter = Array.newInstance(c, childList.getLength());
				//copy inside all the elements
				System.arraycopy(arrayElements, 0, parameter, 0, arrayElements.length);
				if (DEBUG) {
					System.out.println("            created parameter");
				}
				return parameter;
			}
			//check for defaul cosntructor
			if (!subp.hasChildNodes()) {
				if (DEBUG) {
					System.out.println("            created with default cosntructor");
				}
				return c.newInstance();
			}
			//check if it's a leaf node (it has a value & not subparameters)
			if (valueList.getLength() > 0) {
				String value = valueList.item(0).getFirstChild().getNodeValue();
				if (DEBUG) {
					System.out.println("            subParameter is leaf node, value = " + value);
				}
				//needs to get the String constructor
				Object[] initargs = { value };
				Class<?>[] paramterTypes = { initargs[0].getClass() };
				Constructor<?> constr = getConstructor(c, paramterTypes);
				if (DEBUG) {
					System.out.println("            created subParameter");
				}

				return constr.newInstance(initargs);
			} else {
				//leaf node but has subparameters
				NodeList childList = XMLParser.getElementsByTagName(subp, "subParameter");
				Object[] initargs = new Object[childList.getLength()];
				if (DEBUG) {
					System.out.println("            subParameter is a leaf node with subparamter ");
				}
				Class<?>[] paramClasses = new Class[childList.getLength()];
				//creates iteratevely all the subparamters
				for (int i = 0; i < childList.getLength(); i++) {
					Element e = (Element) childList.item(i);
					if (DEBUG) {
						System.out.println("            creates subparameter = " + e.getAttribute("name"));
					}
					initargs[i] = createSubParameter(e);
					paramClasses[i] = initargs[i].getClass();
				}
				//gets the right constructor
				Constructor<?> constr = getConstructor(c, paramClasses);
				Object o = constr.newInstance(initargs);
				if (o instanceof NetNode && o instanceof Distribution) {
					sim.addDistrNetNode((NetNode) o);
				}

				return o;
			}
		} catch (ClassNotFoundException e) {
			throw new LoadException("class of subparameter not found", e);
		} catch (InstantiationException e) {
			throw new LoadException("class of subparameter not found", e);
		} catch (IllegalAccessException e) {
			throw new LoadException("class of subparameter not found", e);
		} catch (NoSuchMethodException e) {
			throw new LoadException("class of subparameter not found", e);
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		if (DEBUG) {
			System.out.println("            creation fake of subparameter");
		}
		return null;
	}

	//finds the position of the classes from the giving names
	private int findClassPosition(String name) {
		for (int j = 0; j < jobClasses.length; j++) {
			JobClass aClass = jobClasses[j];
			if (aClass.getName().equals(name)) {
				return j;
			}
		}
		//class not found
		return -1;
	}

	// An array of names for DOM node-types
	// (Array indexes = nodeType() values.)
	static final String[] typeName = { "none", "Element", "Attr", "Text", "CDATA", "EntityRef", "Entity", "ProcInstr", "Comment", "Document",
			"DocType", "DocFragment", "Notation", };

	/**
	 * gets an appropriate constructor for c given the paramClasses
	 * @param c
	 * @param paramClasses
	 * @return found constructor
	 */
	public Constructor<?> getConstructor(Class<?> c, Class<?>[] paramClasses) throws NoSuchMethodException {
		try {
			return c.getConstructor(paramClasses);
		} catch (NoSuchMethodException e) {
			//the right constructor isn't contained.. let search for another
			//one that fits
		} catch (SecurityException e) {
			e.printStackTrace();
		}
		//gets all constructors
		Constructor<?>[] constrs = c.getConstructors();
		for (Constructor<?> constr : constrs) {
			Class<?>[] params = constr.getParameterTypes();
			if (params.length == paramClasses.length) {
				//right number of parameters
				boolean ok = true;
				for (int j = 0; j < params.length && ok; j++) {
					Class<?> param = params[j];
					if (!param.isAssignableFrom(paramClasses[j])) {
						ok = false;
					}
				}
				if (ok) {
					return constr;
				}
			}
		}
		String errorMessage = c.getName() + "." + "<init>(";
		for (Class<?> paramClass : paramClasses) {
			errorMessage += paramClass.getName() + ", ";
		}
		errorMessage += ")";
		throw new NoSuchMethodException(errorMessage);
	}

	/**
	 * Finds the <tt>int</tt> type of measure corresponding to the <tt>String</tt>
	 * type of measure
	 *
	 * @param measure the String type of measure
	 * @return the int type of measure
	 */
	private int obtainMeasureType(String measure, String referenceNode) {
		if (measure.equalsIgnoreCase("Queue length")//This is for backward compatibility as previously this index was known as Queue Length
				|| measure.equalsIgnoreCase("Customer Number")//This is also for backward compatibility as Queue Length it was known as Customer Number
				|| (measure.equalsIgnoreCase("Number of Customers") && !"".equalsIgnoreCase(referenceNode))) {
			// The above is the present name of the performance index, the AND is used to differentiate it from the old label of System Number of Customers
			// This Perf Index is not system level so it will always refer to some Node. eg Fork, Queueing Station etc.
			return SimConstants.QUEUE_LENGTH;
		} else if (measure.equalsIgnoreCase("Utilization")) {
			return SimConstants.UTILIZATION;
		} else if (measure.equalsIgnoreCase("Throughput")) {
			return SimConstants.THROUGHPUT;
		} else if (measure.equalsIgnoreCase("Response time")) {
			return SimConstants.RESPONSE_TIME;
		} else if (measure.equalsIgnoreCase("Residence time")) {
			return SimConstants.RESIDENCE_TIME;
		} else if (measure.equalsIgnoreCase("Queue time")) {
			return SimConstants.QUEUE_TIME;
		} else if (measure.equalsIgnoreCase("Drop Rate")) {
			return SimConstants.DROP_RATE;
		} else if (measure.equalsIgnoreCase("System response time")) {
			return SimConstants.SYSTEM_RESPONSE_TIME;
		} else if (measure.equalsIgnoreCase("System Throughput")) {
			return SimConstants.SYSTEM_THROUGHPUT;
		} else if (measure.equalsIgnoreCase("System Number of Customers")//Present name of this perf Index
				|| measure.equalsIgnoreCase("System Customer Number")//Previous Name of the Perf Index
				|| (measure.equalsIgnoreCase("Number of Customers") && "".equalsIgnoreCase(referenceNode))) {//Initial Name of the perf index
			//Above OR conditions are for the backward compatibility
			return SimConstants.SYSTEM_JOB_NUMBER;
		} else if (measure.equalsIgnoreCase("System Drop Rate")) {
			return SimConstants.SYSTEM_DROP_RATE;
		}
		//Added by ASHANKA START
		//This loads the System Power into the JSIM
		else if (measure.equalsIgnoreCase("System Power")) {
			return SimConstants.SYSTEM_POWER;
		}
		//Added by ASHANKA STOP
		else if (measure.equalsIgnoreCase("Response Time per Sink")) {
			return SimConstants.RESPONSE_TIME_PER_SINK;
		}
		else if (measure.equalsIgnoreCase("Throughput per Sink")) {
			return SimConstants.THROUGHPUT_PER_SINK;
		}
		//default value
		return SimConstants.UTILIZATION;
	}

	/**
	 * Find the position of the specified class
	 * @param className the name of the class
	 * @return the position if such class exists, -1 otherwise
	 */
	private int getJobClassPosition(String className) {
		if (jobClasses == null) {
			//classes not loaded yet
			return -1;
		}

		//else search in the array
		JobClass jobClass;
		for (int c = 0; c < jobClasses.length; c++) {
			jobClass = jobClasses[c];
			if (jobClass.getName().equalsIgnoreCase(className)) {
				//it's the class we are searching for
				return c;
			}
		}

		//no class with the specified name
		return -1;

	}

	/**
	 * Load blocking region from input XML file
	 * @param root root element of XML file
	 * @param regionList a NodeList data structure with all blocking regions
	 * @throws LoadException if some problems occurrs during loading
	 */
	private void loadBlockingRegions(Element root, NodeList regionList) throws LoadException {

		int classNumber = root.getElementsByTagName("userClass").getLength();

		//adds all regions
		for (int i = 0; i < regionList.getLength(); i++) {

			//get i-th region
			Element region = (Element) regionList.item(i);

			//name
			String regionName = region.getAttribute("name");

			if (DEBUG) {
				System.out.println("start adding region = " + regionName);
			}

			//NOT USED
			//String regionType = region.getAttribute("type");

			//--------------REGION NODES----------------//

			//gets the names of the stations contained in the region
			NodeList regionNodesList = region.getElementsByTagName("regionNode");

			String[] stationNames = new String[regionNodesList.getLength()];

			for (int j = 0; j < regionNodesList.getLength(); j++) {
				Element rn = (Element) regionNodesList.item(j);
				stationNames[j] = rn.getAttribute("nodeName");
				if (DEBUG) {
					System.out.println("   region contains node = " + stationNames[j]);
				}
			}

			//---------GLOBAL CONSTRAINT-----------//

			double maxCapacity = -1;

			NodeList globalConstraints = region.getElementsByTagName("globalConstraint");

			if (globalConstraints.item(0) != null) {
				Element globalConst = (Element) globalConstraints.item(0);
				maxCapacity = Double.parseDouble(globalConst.getAttribute("maxJobs"));
				if (DEBUG) {
					System.out.println("   global constraint = " + Double.toString(maxCapacity));
				}
			} else {
				throw new LoadException("Element \"globalConstraint\" missing...");
			}

			//--------------DROP RULES---------------//

			NodeList dropRules = region.getElementsByTagName("dropRules");

			//drop rules (one for each class)
			boolean[] dropThisClass = new boolean[classNumber];
			//init
			if (dropRules.getLength() == 0) {
				//no drop rules specified: use default drop values (drop open and keep closed)
				for (int c = 0; c < classNumber; c++) {
					int classType = jobClasses[c].getType();
					if (classType == JobClass.OPEN_CLASS) {
						//drop open class jobs
						dropThisClass[c] = true;
					} else {
						//block closed class jobs
						dropThisClass[c] = false;
					}
				}
				if (DEBUG) {
					System.out.println("Loading default drop rules...");
				}
			} else {
				//drop rules specified by user

				if (DEBUG) {
					System.out.println("Loading specified drop rules...");
				}

				//the number of drop rules must be exactly equal
				//to the number of classes
				/*
				if (dropRules.getLength() != classNumber) {
				throw new LoadException("One drop rule is required for each class.");
				}
				*/

				Element dropRule = null;
				int classPosition;
				String className;

				for (int dr = 0; dr < dropRules.getLength(); dr++) {
					dropRule = (Element) dropRules.item(dr);

					className = dropRule.getAttribute("jobClass");

					//entries may be in a wrong order: find the right position
					classPosition = getJobClassPosition(className);

					if (classPosition == -1) {
						//class not exists
						throw new LoadException("The job class associated to this drop rule does not exist in model");
					} else {
						dropThisClass[classPosition] = dropRule.getAttribute("dropThisClass").equalsIgnoreCase("true");
					}

					if (DEBUG) {
						System.out.println("   drop class " + className + " = " + dropThisClass[classPosition]);
					}
				}

			}

			//-----------------CLASS CONSTRAINTS-----------------//
			NodeList classConstraints = region.getElementsByTagName("classConstraint");

			//max capacity for each class (-1 means no constraint)
			double[] maxCapacityPerClass = new double[classNumber];
			//init
			Arrays.fill(maxCapacityPerClass, -1.0);

			if (classConstraints.getLength() > 0) {
				//the number of class constraints must be exactly equal
				//to the number of classes
				/*
				if (classConstraints.getLength() != classNumber) {
				    throw new LoadException("One class constraint is required for each class.");
				}
				*/

				Element clsConst = null;
				int classPosition;
				String className;

				for (int cc = 0; cc < classConstraints.getLength(); cc++) {
					clsConst = (Element) classConstraints.item(cc);

					className = clsConst.getAttribute("jobClass");

					//entries may be in a wrong order: find the right position
					classPosition = getJobClassPosition(className);

					if (classPosition == -1) {
						//class not exists
						throw new LoadException("The job class associated to this class constraint does not exist in model");
					} else {
						//set max capacity for this class
						maxCapacityPerClass[classPosition] = Double.parseDouble(clsConst.getAttribute("maxJobsPerClass"));
					}

					if (DEBUG) {
						System.out.println("   constraint for class " + className + " = " + Double.toString(maxCapacityPerClass[classPosition]));
					}
				}

			}

			//----------------CLASS WEIGHTS----------------//

			NodeList classWeights = root.getElementsByTagName("classWeight");

			double[] regionClassWeights = new double[classNumber];
			//init
			Arrays.fill(regionClassWeights, 1.0);

			if (classWeights.getLength() > 0) {

				//the number of class weights must be exactly equal
				//to the number of classes
				/*
				if (classWeights.getLength() != classNumber) {
				    throw new LoadException("One class weight is required for each class.");
				}
				*/

				Element clsWeight = null;
				int classPosition;
				String className;

				for (int c = 0; c < classWeights.getLength(); c++) {
					clsWeight = (Element) classWeights.item(c);

					className = clsWeight.getAttribute("jobClass");

					//entries may be in a wrong order: find the right position
					classPosition = getJobClassPosition(className);

					if (classPosition == -1) {
						//class not exists
						throw new LoadException("The job class associated to this weight does not exist in model");
					} else {
						regionClassWeights[classPosition] = Double.parseDouble(clsWeight.getAttribute("weight"));
					}

					if (DEBUG) {
						System.out.println("   weight for class " + className + " = " + Double.toString(regionClassWeights[classPosition]));
					}
				}

			}

			//------------------ADD BLOCKING REGION TO SIM ---------//
			sim.addRegion(regionName, maxCapacity, maxCapacityPerClass, regionClassWeights, dropThisClass, stationNames);

			if (DEBUG) {
				System.out.println("added region " + regionName);
			}
		}

	}

	// Cut and paste of old GUI code used by SimLoader (to clear up things) - Bertoli Marco
	public static class XMLParser {
		/**
		 *  Returns a NodeList of all descendant Elements with a given tag name, in
		 * the order in which they are encountered in a preorder traversal of this
		 * Element (ony at first level).
		 * @param e  The lement that is being analyzed.
		 * @param name  The name of the tag to match on.
		 * @return A list of matching Element nodes.
		 */
		public static NodeList getElementsByTagName(Element e, String name) {
			JmtNodeList reqChild = new JmtNodeList();

			NodeList childList = e.getChildNodes();
			if (childList != null && childList.getLength() != 0) {
				for (int i = 0; i < childList.getLength(); i++) {
					Node n = childList.item(i);
					if (n.getNodeType() == 1 && n.getNodeName() != null && n.getNodeName().equals(name)) {
						reqChild.add(n);
					}

				}
			}
			return reqChild;

		}

		/** Implements a NodeList that lets also add Noes, feel free to
		 * add methods if u need.

		 * @author Federico Granata
		 * Date: 16-ott-2003
		 * Time: 17.17.43

		 */
		public static class JmtNodeList implements NodeList {
			ArrayList<Node> data;

			public JmtNodeList() {
				data = new ArrayList<Node>();
			}

			public void add(Node child) {
				data.add(child);
			}

			public Node item(int index) {
				return data.get(index);
			}

			public int getLength() {
				return data.size();
			}
		}

	}
	//END - Bertoli Marco

}
