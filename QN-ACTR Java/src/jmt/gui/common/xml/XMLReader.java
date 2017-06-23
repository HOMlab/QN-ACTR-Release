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

package jmt.gui.common.xml;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.engine.log.JSimLogger;
import jmt.engine.log.LoggerParameters;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.common.serviceStrategies.LDStrategy;
import jmt.gui.common.serviceStrategies.ZeroStrategy;

import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * <p>Title: XML Reader</p>
 * <p>Description: Reads model information from an XML file. This
 * class provide methods for model load. It's designed to be used by both JModel and JSim.</p>
 *
 * @author Bertoli Marco
 *         Date: 27-lug-2005
 *         Time: 13.59.48
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
 */
public class XMLReader implements XMLConstantNames, CommonConstants {
	protected static TreeMap<String, Object> classes; // Data structure used to map between class name and its key
	protected static TreeMap<String, Object> stations; // Data structure used to map between station name and its key
	protected static TreeMap<String, Object> regions; // Data structure used to map between region name and its key
	protected static HashMap<Object, String> refStations; // Data structure used to hold classes' reference stations
	protected static HashMap<Object[], Map<String, Double>> empiricalRouting; // Data structure to save malformed empirical routing tuples

	/*defines the default logger (used to report errors and information for debugging purposes)*/
	private static final jmt.engine.log.JSimLogger debugLog = jmt.engine.log.JSimLogger.getLogger(JSimLogger.STD_LOGGER);

	/*defines matching between engine representation and gui names for drop
	rules.*/
	protected static final HashMap dropRulesNamesMatchings = new HashMap() {
		/**
		 *
		 */
		private static final long serialVersionUID = 1L;

		{
			put("drop", FINITE_DROP);
			put("waiting queue", FINITE_WAITING);
			put("BAS blocking", FINITE_BLOCK);
		}
	};

	// Variables used with caching purpose to improve reading speed
	protected static Map<String, Distribution> engineToGuiDistr = null;
	protected static Map<String, RoutingStrategy> engineToGuiRouting = null;

	protected static final String queueGetFCFS = "jmt.engine.NetStrategies.QueueGetStrategies.FCFSstrategy";
	protected static final String queueGetLCFS = "jmt.engine.NetStrategies.QueueGetStrategies.LCFSstrategy";
	protected static final String queueGetPS = "jmt.engine.NetStrategies.QueueGetStrategies.PSSstrategy";
	protected static final String queuePut = "jmt.engine.NetStrategies.QueuePutStrategy";
	protected static final String serviceStrategy = "jmt.engine.NetStrategies.ServiceStrategy";
	protected static final String distributionContainer = "jmt.engine.random.DistributionContainer";

	/**
	 * Restore a model saved in an XML file, given the name of the file. If specified file
	 * is a jmodel archive, extracts model informations from it and uses them to reconstruct
	 * the model. This method is provided to be used with JSIM
	 * @param fileName name of the file to be opened
	 * @param model data structure where model should be created (a new data structure
	 * is the best choice)
	 * @return true iff model was recognized and loaded, false otherwise
	 */
	public static boolean loadModel(String fileName, CommonModel model) {
		Document doc = loadXML(fileName, XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_DEFINITION));
		if (doc.getElementsByTagName(XML_DOCUMENT_ROOT).getLength() != 0) {
			// Document is a simulation model
			parseXML(doc, model);
			return true;
		} else if (doc.getElementsByTagName(GuiXMLConstants.XML_ARCHIVE_DOCUMENT_ROOT).getLength() != 0) {
			// Document is an archive
			parseXML(XMLArchiver.getSimFromArchiveDocument(doc), model);
			return true;
		}
		return false;
	}

	/**
	 * Restore a model saved in an XML file, given the handler to the file. If specified file
	 * is a jmodel archive, extracts model informations from it and uses them to reconstruct
	 * the model. This method is provided to be used with JSIM
	 * @param xmlFile handler to the file to be opened
	 * @param model data structure where model should be created (a new data structure
	 * is the best choice)
	 * @return true iff model was recognized and loaded, false otherwise
	 */
	public static boolean loadModel(File xmlFile, CommonModel model) {
		return loadModel(xmlFile.getAbsolutePath(), model);
	}

	/**
	 * Parses given Gui XML Document to reconstruct simulation model.
	 * @param root root of document to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Element root, CommonModel model) {
		// Gets optional parameter simulation seed
		String seed = root.getAttribute(XML_A_ROOT_SEED);
		if (seed != null && seed != "") {
			model.setUseRandomSeed(false);
			model.setSimulationSeed(new Long(seed));
		} else {
			model.setUseRandomSeed(true);
		}
		// Gets optional parameter maximum time
		String maxTime = root.getAttribute(XML_A_ROOT_DURATION);
		if (maxTime != null && maxTime != "") {
			model.setMaximumDuration(new Double(maxTime));
		} else {
			model.setMaximumDuration(new Double(-1));
		}

		// Gets optional parameter polling interval
		String polling = root.getAttribute(XML_A_ROOT_POLLING);
		if (polling != null && polling != "") {
			model.setPollingInterval(Double.parseDouble(polling));
		}

		// Gets optional parameter maximum samples
		String maxSamples = root.getAttribute(XML_A_ROOT_MAXSAMPLES);
		if (maxSamples != null && maxSamples != "") {
			model.setMaxSimulationSamples(Integer.decode(maxSamples));
		}

		// Gets optional parameter disable statistic
		String disableStatistic = root.getAttribute(XML_A_ROOT_DISABLESTATISTIC);
		if (disableStatistic != null && disableStatistic != "") {
			model.setDisableStatistic(Boolean.valueOf(disableStatistic));
		}

		/* Gets optional parameters log path, replace policy, and delimiter
		 * Values here should correspond to SimLoader values (Ctrl+F for them) */
		String logPath = root.getAttribute(XML_A_ROOT_LOGPATH);
		if (logPath != null && logPath != "") {
			model.setLoggingGlbParameter("path", logPath);
		} else {
			model.setLoggingGlbParameter("path", "");
		}
		String logReplaceMode = root.getAttribute(XML_A_ROOT_LOGREPLACE);
		if (logReplaceMode != null && logReplaceMode != "") {
			model.setLoggingGlbParameter("autoAppend", logReplaceMode);
		} else {
			model.setLoggingGlbParameter("autoAppend", Defaults.get("loggerAutoAppend"));
		}
		String logDelimiter = root.getAttribute(XML_A_ROOT_LOGDELIM);
		if (logDelimiter != null && logDelimiter != "") {
			model.setLoggingGlbParameter("delim", logDelimiter);
		} else {
			model.setLoggingGlbParameter("delim", Defaults.get("loggerDelimiter"));
		}
		String logDecimalSeparator = root.getAttribute(XML_A_ROOT_LOGDECIMALSEPARATOR);
		if (logDecimalSeparator != null && logDecimalSeparator != "") {
			model.setLoggingGlbParameter("decimalSeparator", logDecimalSeparator);
		} else {
			model.setLoggingGlbParameter("decimalSeparator", ".");
		}

		parseClasses(root, model);
		empiricalRouting = new HashMap<Object[], Map<String, Double>>();
		parseStations(root, model);
		parseConnections(root, model);
		parseBlockingRegions(root, model);
		parseMeasures(root, model);
		parsePreloading(root, model);
		// Set reference station for each class
		Object[] keys = refStations.keySet().toArray();
		for (Object key : keys) {
			model.setClassRefStation(key, stations.get(refStations.get(key)));
		}
		// Sets correct station key into every empiricalRouting element
		// Now each key is an Object[] where (0) is station key and (1) class key
		keys = empiricalRouting.keySet().toArray();
		for (Object key : keys) {
			Object[] dualkey = (Object[]) key;
			RoutingStrategy rs = (RoutingStrategy) model.getRoutingStrategy(dualkey[0], dualkey[1]);
			Map routing = rs.getValues();
			Map values = empiricalRouting.get(key);
			Object[] names = values.keySet().toArray();
			// Creates correct hashmap with station key --> probability mapping
			for (Object name : names) {
				routing.put(stations.get(name), values.get(name));
			}
		}
	}

	/**
	 * Parses given Gui XML Document to reconstruct simulation model.
	 * @param xml Document to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Document xml, CommonModel model) {
		parseXML(xml.getDocumentElement(), model);
	}

	// --- Helper methods ----------------------------------------------------------------------------
	/**
	 * Helper method that searches for first text node, between all children of current node
	 * and returns its value. (This is needed to garbage out all comments)
	 * @param elem root node to begin search
	 * @return parsed text if found, otherwise null
	 */
	protected static String findText(Node elem) {
		NodeList tmp = elem.getChildNodes();
		for (int j = 0; j < tmp.getLength(); j++) {
			if (tmp.item(j).getNodeType() == Node.TEXT_NODE) {
				return tmp.item(j).getNodeValue();
			}
		}
		return null;
	}

	// -----------------------------------------------------------------------------------------------

	// --- Class section -----------------------------------------------------------------------------
	/**
	 * Parses userclasses information. Note that distributions for open class will be set lately
	 * and reference station information is stored into refStations data structure as will
	 * be used later
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parseClasses(Element root, CommonModel model) {
		// Initialize classes and refStations data structure
		classes = new TreeMap<String, Object>();
		refStations = new HashMap<Object, String>();
		NodeList nodeclasses = root.getElementsByTagName(XML_E_CLASS);
		// Now scans all elements
		Element currclass;
		int type, priority;
		Integer customers;
		String name;
		Distribution defaultDistr = (Distribution) Defaults.getAsNewInstance("classDistribution");
		Object key;
		for (int i = 0; i < nodeclasses.getLength(); i++) {
			currclass = (Element) nodeclasses.item(i);
			name = currclass.getAttribute(XML_A_CLASS_NAME);
			type = currclass.getAttribute(XML_A_CLASS_TYPE).equals("closed") ? CLASS_TYPE_CLOSED : CLASS_TYPE_OPEN;
			customers = new Integer(0);
			priority = 0;
			// As these elements are not mandatory, sets them to 0, then tries to parses them
			String tmp = currclass.getAttribute(XML_A_CLASS_CUSTOMERS);
			if (tmp != null && tmp != "") {
				customers = Integer.valueOf(tmp);
			}

			tmp = currclass.getAttribute(XML_A_CLASS_PRIORITY);
			if (tmp != null && tmp != "") {
				priority = Integer.parseInt(tmp);
			}

			// Now adds user class. Note that distribution will be set lately.
			key = model.addClass(name, type, priority, customers, defaultDistr);
			// Stores reference station as will be set lately (when we will have stations key)
			refStations.put(key, currclass.getAttribute(XML_A_CLASS_REFSOURCE));
			// Creates mapping class-name -> key into stations data structure
			classes.put(name, key);
		}

	}

	// -----------------------------------------------------------------------------------------------

	// --- Station section ---------------------------------------------------------------------------
	/**
	 * Parses all station related informations and puts them into data structure
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parseStations(Element root, CommonModel model) {
		// Initialize stations data structure
		stations = new TreeMap<String, Object>();
		NodeList nodestations = root.getElementsByTagName(XML_E_STATION);
		Object key;
		Element station;
		String type, name;
		NodeList sections;
		// For every station, identifies its type and parses its parameters
		for (int i = 0; i < nodestations.getLength(); i++) {
			station = (Element) nodestations.item(i);
			sections = station.getElementsByTagName(XML_E_STATION_SECTION);
			type = getStationType(station);
			name = station.getAttribute(XML_A_STATION_NAME);
			// Puts station into data structure
			key = model.addStation(name, type);
			// Creates mapping station-name -> key into stations data structure
			stations.put(name, key);
			// Handles source (set distribution)
			if (type.equals(STATION_TYPE_SOURCE)) {
				parseSource((Element) sections.item(0), model, key, name);
				parseRouter((Element) sections.item(2), model, key);
			} else if (type.equals(STATION_TYPE_TERMINAL) || type.equals(STATION_TYPE_ROUTER) || type.equals(STATION_TYPE_JOIN)) {
				parseRouter((Element) sections.item(2), model, key);
			} else if (type.equals(STATION_TYPE_DELAY)) {
				parseDelay((Element) sections.item(1), model, key);
				parseRouter((Element) sections.item(2), model, key);
			} else if (type.equals(STATION_TYPE_SERVER)) {
				parseQueue((Element) sections.item(0), model, key);
				parseServer((Element) sections.item(1), model, key);
				parseRouter((Element) sections.item(2), model, key);
			} else if (type.equals(STATION_TYPE_FORK)) {
				parseQueue((Element) sections.item(0), model, key);
				parseFork((Element) sections.item(2), model, key);
			} else if (type.equals(STATION_TYPE_LOGGER)) {
				parseLogger((Element) sections.item(1), model, key);
				parseRouter((Element) sections.item(2), model, key);
			}

		}
	}

	/**
	 * Extract all informations regarding Source section. If this source is reference class
	 * for any kind of open class, uses service time informations stored here to set distribution
	 * for this class.
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 * @param stationName Name of current station. This is used to correctly set reference station
	 * distribution. That cannot be derived from model.getStationName(key) as JSim can change
	 * source name upon opening a model stored with JModel.
	 */
	protected static void parseSource(Element section, CommonModel model, Object key, String stationName) {
		Element parameter = (Element) section.getElementsByTagName(XML_E_PARAMETER).item(0);
		// Now parses Service Distribution
		Map<String, Node> distributions = parseParameterRefclassArray(parameter);
		// Assign distribution for a class only if current source is its reference station
		Object[] classNames = distributions.keySet().toArray();
		Object classkey;
		for (Object className : classNames) {
			// If current class has this station as reference source and is open...
			if (refStations.get(classes.get(className)) != null && refStations.get(classes.get(className)).equals(stationName)
					&& model.getClassType(classes.get(className)) == CLASS_TYPE_OPEN) {
				classkey = classes.get(className);
				model.setClassDistribution(parseServiceStrategy((Element) distributions.get(className)), classkey);
				model.setClassRefStation(classkey, key);
				// Removes this class from refStations as it was already handled
				refStations.remove(classkey);
			}
		}
	}

	/**
	 * Extract all informations regarding Queue section.
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 */
	protected static void parseQueue(Element section, CommonModel model, Object key) {
		NodeList parameters = section.getElementsByTagName(XML_E_PARAMETER);
		Element curr;
		String name, classpath;
		boolean fcfs = true;
		boolean ps = false;
		Map<String, Node> putStrategy = null;
		Map<String, Node> dropRules = null;
		for (int i = 0; i < parameters.getLength(); i++) {
			curr = (Element) parameters.item(i);
			name = curr.getAttribute(XML_A_PARAMETER_NAME);
			classpath = curr.getAttribute(XML_A_PARAMETER_CLASSPATH);
			if (classpath.equals(queueGetFCFS)) {
				fcfs = true;
			} else if (classpath.equals(queueGetLCFS)) {
				fcfs = false;
			} else if (classpath.equals(queueGetPS)) {
				fcfs = false;
				ps = true;
			} else if (classpath.equals(queuePut)) {
				putStrategy = parseParameterRefclassArray(curr);
			} else if (name.equals("size")) {
				Integer size = Integer.valueOf(findText(curr.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				model.setStationQueueCapacity(size, key);
			} else if (name.equals("dropStrategies")) {
				dropRules = parseParameterRefclassArray(curr);
			}
		}
		if (putStrategy != null) {
			Object[] classNames = putStrategy.keySet().toArray();
			String strategy;
			for (Object className : classNames) {
				strategy = ((Element) putStrategy.get(className)).getAttribute(XML_A_SUBPARAMETER_CLASSPATH);
				// Takes away classpath from put strategy name
				strategy = strategy.substring(strategy.lastIndexOf(".") + 1, strategy.length());
				// Now sets correct queue strategy, given combination of queueget and queueput policies
				if (ps) {
					model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_PS);
					model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_FCFS);
				} else if (strategy.equals("HeadStrategy")) {
					model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_QUEUE);
					if (fcfs) {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_LCFS);
					} else {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_FCFS);
					}
				} else if (strategy.equals("HeadStrategyPriority")) {
					model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_QUEUE_PRIORITY);
					if (fcfs) {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_LCFS);
					} else {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_FCFS);
					}
				} else if (strategy.equals("TailStrategy")) {
					model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_QUEUE);
					if (fcfs) {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_FCFS);
					} else {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_LCFS);
					}
				} else if (strategy.equals("TailStrategyPriority")) {
					model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_QUEUE_PRIORITY);
					if (fcfs) {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_FCFS);
					} else {
						model.setQueueStrategy(key, classes.get(className), QUEUE_STRATEGY_LCFS);
					}
				}
			}
		}
		// Decodes drop rules
		if (dropRules != null) {
			Object[] classNames = dropRules.keySet().toArray();
			String strategy;
			for (Object className : classNames) {
				strategy = findText(((Element) dropRules.get(className)).getElementsByTagName(XML_E_PARAMETER_VALUE).item(0));
				model.setDropRule(key, classes.get(className), (String) dropRulesNamesMatchings.get(strategy));
			}
		}
	}

	/**
	 * Extract all informations regarding Delay section.
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 */
	protected static void parseDelay(Element section, CommonModel model, Object key) {
		Element parameter = (Element) section.getElementsByTagName(XML_E_PARAMETER).item(0);
		// Retrives all distributions subParameters
		Map<String, Node> distributions = parseParameterRefclassArray(parameter);
		Object[] classNames = distributions.keySet().toArray();
		// Sets service time distributions
		for (Object className : classNames) {
			model.setServiceTimeDistribution(key, classes.get(className), parseServiceStrategy((Element) distributions.get(className)));
		}
	}

	/**
	 * Extract all informations regarding Delay section.
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 */
	protected static void parseServer(Element section, CommonModel model, Object key) {
		String className = section.getAttribute(XML_A_STATION_SECTION_CLASSNAME);
		if (CLASSNAME_PSSERVER.equals(className)) {
			model.setStationQueueStrategy(key, QUEUE_STRATEGY_STATION_PS);
		}
		NodeList parameters = section.getElementsByTagName(XML_E_PARAMETER);
		Element curr;
		String name, classpath;
		for (int i = 0; i < parameters.getLength(); i++) {
			curr = (Element) parameters.item(i);
			name = curr.getAttribute(XML_A_PARAMETER_NAME);
			classpath = curr.getAttribute(XML_A_PARAMETER_CLASSPATH);
			if (classpath.equals(serviceStrategy)) {
				// Retrives all distributions subParameters
				Map<String, Node> distributions = parseParameterRefclassArray((Element) parameters.item(i));
				Object[] classNames = distributions.keySet().toArray();
				// Sets service time distributions
				for (Object className2 : classNames) {
					model.setServiceTimeDistribution(key, classes.get(className2), parseServiceStrategy((Element) distributions.get(className2)));
				}
			} else if (name.equals("maxJobs")) {
				// Sets number of servers
				Integer jobs = Integer.valueOf(findText(curr.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				model.setStationNumberOfServers(jobs, key);
			}
		}
	}

	/**
	 * Parses A Parameter Array node, returning a Map of ClassName -> subParameter
	 * @param parameterNode
	 * @return a Map of ClassName -> subParameter
	 */
	protected static Map<String, Node> parseParameterRefclassArray(Element parameterNode) {
		// For some reasons getElementsByTagName returns only first service time strategy.
		// So we need to look every children of parameterNode node.
		TreeMap<String, Node> res = new TreeMap<String, Node>();
		Node child = parameterNode.getFirstChild();
		String refClass;
		// This manual parsing is a bit unclean but works well and it's really fast.
		// I was forced to do in this way for the problem said before.
		while (child != null) {
			while (child != null && (child.getNodeType() != Node.ELEMENT_NODE || !child.getNodeName().equals(XML_E_PARAMETER_REFCLASS))) {
				child = child.getNextSibling();
			}

			if (child == null) {
				break;
			}
			refClass = findText(child);
			// Now finds first subParameter element
			while (child != null && (child.getNodeType() != Node.ELEMENT_NODE || !child.getNodeName().equals(XML_E_SUBPARAMETER))) {
				child = child.getNextSibling();
			}

			if (child == null) {
				break;
			}

			// Puts className and subParameter into destination Map
			res.put(refClass, child);
			child = child.getNextSibling();
		}

		return res;
	}

	/**
	 * Parses a parameter array and returns Vector of found subParameters
	 * @param parameterNode
	 * @return Vector with found subParameters
	 */
	protected static Vector<Node> parseParameterArray(Element parameterNode) {
		Vector<Node> ret = new Vector<Node>();
		Node child = parameterNode.getFirstChild();

		while (child != null) {
			while (child != null && (child.getNodeType() != Node.ELEMENT_NODE || !child.getNodeName().equals(XML_E_SUBPARAMETER))) {
				child = child.getNextSibling();
			}

			if (child == null) {
				break;
			}
			// Puts found subParameter into destination Vector
			ret.add(child);
			child = child.getNextSibling();
		}
		return ret;
	}

	/**
	 * Parse router section
	 * @param section router section
	 * @param model data structure
	 * @param key station's key
	 */
	protected static void parseRouter(Element section, CommonModel model, Object key) {
		Element parameter = (Element) section.getElementsByTagName(XML_E_PARAMETER).item(0);
		Map<String, Node> routing = parseParameterRefclassArray(parameter);
		Object[] classNames = routing.keySet().toArray();
		String className;

		// Creates a Map of Name --> Routing Strategy if needed
		if (engineToGuiRouting == null) {
			engineToGuiRouting = new TreeMap<String, RoutingStrategy>();
			RoutingStrategy[] allRS = RoutingStrategy.findAll();
			for (RoutingStrategy element : allRS) {
				engineToGuiRouting.put(element.getClass().getName(), element);
			}
		}

		Object[] routStratKeys = engineToGuiRouting.keySet().toArray();
		for (Object className2 : classNames) {
			className = ((Element) routing.get(className2)).getAttribute(XML_A_SUBPARAMETER_CLASSPATH);
			// Searches all available routing strategy to find the one saved
			for (Object routStratKey : routStratKeys) {
				if (className.equals(engineToGuiRouting.get(routStratKey).getClassPath())) {
					model.setRoutingStrategy(key, classes.get(className2), engineToGuiRouting.get(routStratKey).clone());
				}
			}

			// Treat particular case of Empirical (Probabilities) Routing
			RoutingStrategy rs = (RoutingStrategy) model.getRoutingStrategy(key, classes.get(className2));
			if (rs instanceof ProbabilityRouting) {
				// Creates a Vector of all empirical entris. Could not be done automaticly
				// for the above problem with array (see parseParameterRefclassArray)
				Vector<Node> entries = new Vector<Node>();
				// Finds EntryArray node
				Node entryArray = routing.get(className2).getFirstChild();
				while (entryArray.getNodeType() != Node.ELEMENT_NODE || !entryArray.getNodeName().equals(XML_E_SUBPARAMETER)) {
					entryArray = entryArray.getNextSibling();
				}
				// Now finds every empirical entry
				Node child = entryArray.getFirstChild();
				while (child != null) {
					// Find first subParameter element
					while (child != null && (child.getNodeType() != Node.ELEMENT_NODE || !child.getNodeName().equals(XML_E_SUBPARAMETER))) {
						child = child.getNextSibling();
					}
					if (child != null) {
						entries.add(child);
						child = child.getNextSibling();
					}
				}
				// For each empirical entry get station name and probability
				for (int j = 0; j < entries.size(); j++) {
					NodeList values = ((Element) entries.get(j)).getElementsByTagName(XML_E_SUBPARAMETER);
					String stationName = findText(((Element) values.item(0)).getElementsByTagName(XML_E_PARAMETER_VALUE).item(0));
					Double probability = Double.valueOf(findText(((Element) values.item(1)).getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
					// Now puts the tuple stationName -> probability into a Map, then adds it
					// to empiricalRouting Map. This is needed as at this
					// point we don't have all station's key so will be adjusted latelly
					Map<String, Double> tmp = new TreeMap<String, Double>();
					tmp.put(stationName, probability);
					// Put into empiricalRouting a pair of station key and class key and map with
					// station names instead of station key
					empiricalRouting.put(new Object[] { key, classes.get(className2) }, tmp);
				}
			}
		}
	}

	/**
	 * Extract all parameters for a Logger section from the XML document.
	 * The information from parseLogger is passed to LogTunnel.
	 * 
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 * @author Michael Fercu (Bertoli Marco)
	 *		   Date: 08-aug-2008
	 * @see jmt.engine.log.LoggerParameters LoggerParameters
	 * @see jmt.gui.common.XMLWriter#writeLoggerSection XMLWriter.writeLoggerSection()
	 * @see jmt.gui.common.definitions.CommonModel#getLoggingParameters CommonModel.getLoggingParameters()
	 * @see jmt.gui.common.definitions.CommonModel#setLoggingParameters CommonModel.setLoggingParameters()
	 * @see jmt.engine.NodeSections.LogTunnel LogTunnel
	 */
	protected static void parseLogger(Element section, CommonModel model, Object key) {
		NodeList parameters = section.getElementsByTagName(XML_E_PARAMETER);
		LoggerParameters logParams = new LoggerParameters();

		for (int i = 0; i < parameters.getLength(); i++) {
			Element parameter = (Element) parameters.item(i);
			String parameterName = parameter.getAttribute(XML_A_PARAMETER_NAME);
			try {
				// Get the parameters from the XML file
				if (parameterName.equals(XML_LOG_FILENAME)) {
					logParams.name = new String(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_FILEPATH)) {
					try {
						logParams.path = new String(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
					} catch (Exception e) {
						logParams.path = "./";
					} // this should never happen
				} else if (parameterName.equals(XML_LOG_B_EXECTIMESTAMP)) {
					logParams.boolExecTimestamp = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_LOGGERNAME)) {
					logParams.boolLoggername = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_TIMESTAMP)) {
					logParams.boolTimeStamp = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_JOBID)) {
					logParams.boolJobID = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_JOBCLASS)) {
					logParams.boolJobClass = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_TIMESAMECLS)) {
					logParams.boolTimeSameClass = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals(XML_LOG_B_TIMEANYCLS)) {
					logParams.boolTimeAnyClass = new Boolean(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0)));
				} else if (parameterName.equals("numClasses")) {
					/* No parsing needed for these parameters:
					 * Only useful to (and has already been passed to) the simulator. */;
				} else {
					debugLog.error("XMLReader.parseLogger() - Unknown parameter \"" + parameterName + "\".");
				}
			} catch (Exception e) {
				debugLog.error("XMLreader.parseLogger: " + e.toString());
			}
			model.setLoggingParameters(key, logParams);

		}
	}

	/**
	 * Extract all informations regarding Fork section.
	 * @param section input section of source station
	 * @param model link to data structure
	 * @param key key of search for this source station into data structure
	 */
	protected static void parseFork(Element section, CommonModel model, Object key) {
		NodeList parameters = section.getElementsByTagName(XML_E_PARAMETER);
		for (int i = 0; i < parameters.getLength(); i++) {
			Element parameter = (Element) parameters.item(i);
			String parameterName = parameter.getAttribute(XML_A_PARAMETER_NAME);
			// Fork number of server is used as number of jobs per link
			if (parameterName.equals("jobsPerLink")) {
				model.setStationNumberOfServers(Integer.decode(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0))), key);
			} else if (parameterName.equals("block")) {
				model.setForkBlock(key, Integer.valueOf(findText(parameter.getElementsByTagName(XML_E_PARAMETER_VALUE).item(0))));
			}
		}
	}

	/**
	 * Parses service section informations contained in serviceTimeStrategy element to create a
	 * correct Distribution or LDStrategy object
	 * @param serviceTimeStrategy Element that holds all distribution informations
	 * @return created Distribution or LDStrategy or null if this field is set to null
	 */
	protected static Object parseServiceStrategy(Element serviceTimeStrategy) {
		// Ccreates a map with distribution classpath --> Distribution if needed
		if (engineToGuiDistr == null) {
			Distribution[] allDistr = Distribution.findAll();
			engineToGuiDistr = new TreeMap<String, Distribution>();
			for (Distribution element : allDistr) {
				engineToGuiDistr.put(element.getClassPath(), element);
			}
		}

		String serviceClassPath = serviceTimeStrategy.getAttribute(XML_A_SUBPARAMETER_CLASSPATH);
		if (serviceClassPath.equals(ZeroStrategy.getEngineClassPath())) {
			// Zero service time strategy
			return new ZeroStrategy();
		} else if (serviceClassPath.equals(LDStrategy.getEngineClassPath())) {
			// Load dependent Service Strategy
			Element LDParameterArray = (Element) serviceTimeStrategy.getElementsByTagName(XML_E_SUBPARAMETER).item(0);
			LDStrategy strategy = new LDStrategy();
			// Now parses LDStrategy ranges
			Vector<Node> ranges = parseParameterArray(LDParameterArray);
			for (int i = 0; i < ranges.size(); i++) {
				Vector<Node> parameters = parseParameterArray((Element) ranges.get(i));
				int from = Integer.parseInt(findText(((Element) parameters.get(0)).getElementsByTagName(XML_E_SUBPARAMETER_VALUE).item(0)));
				Distribution distr = parseDistribution((Element) parameters.get(1), (Element) parameters.get(2));
				String mean = findText(((Element) parameters.get(3)).getElementsByTagName(XML_E_SUBPARAMETER_VALUE).item(0));
				Object key;
				if (from == 1) {
					// If this is first range
					key = strategy.getAllRanges()[0];
				} else {
					// next ranges
					key = strategy.addRange();
					strategy.setRangeFrom(key, from);
					// This is needed as key will change
					key = strategy.getAllRanges()[strategy.getRangeNumber() - 1];
				}
				strategy.setRangeDistributionNoCheck(key, distr);
				strategy.setRangeDistributionMeanNoCheck(key, mean);
			}
			return strategy;
		} else {

			//use the parseParameterArray function to return only DIRECT subparameters
			Vector<Node> distribution = parseParameterArray(serviceTimeStrategy);
			if (distribution.size() == 0) {
				return null;
			}
			return parseDistribution((Element) distribution.get(0), (Element) distribution.get(1));
		}
	}

	/**
	 * Parses a distribution, given its distribution and distributionPar nodes
	 * @param distr distribution node
	 * @param distrPar distribution's parameter node
	 * @return parsed distribution
	 */
	protected static Distribution parseDistribution(Element distr, Element distrPar) {
		String classname = distr.getAttribute(XML_A_SUBPARAMETER_CLASSPATH);

		//get the subparameter which are directly passed to the distribution
		Vector<Node> distributionParameters = parseParameterArray(distr);
		//add the subparameters which are passed to the distribution parameter
		distributionParameters.addAll(parseParameterArray(distrPar));

		// Gets correct instance of distribution
		Distribution dist = engineToGuiDistr.get(classname).clone();
		Element currpar;
		String param_name;
		for (int i = 0; i < distributionParameters.size(); i++) {

			currpar = (Element) distributionParameters.get(i);

			param_name = currpar.getAttribute(XML_A_SUBPARAMETER_NAME);
			//if current parameter is a nested Distribution
			if (currpar.getAttribute(XML_A_SUBPARAMETER_CLASSPATH).equals(distributionContainer)) {

				//parse the currentparameter to get DIRECT subparameters
				Vector<Node> nestedDistr = parseParameterArray(currpar);
				// If distribution is not set, returns null
				Object param_value = null;
				if (nestedDistr.size() == 0) {
					param_value = null;
				} else {
					//parse the nested distribution
					param_value = parseDistribution((Element) nestedDistr.get(0), (Element) nestedDistr.get(1));
					dist.getParameter(param_name).setValue(param_value);
				}

			} else {
				String param_value = findText(currpar.getElementsByTagName(XML_E_SUBPARAMETER_VALUE).item(0));
				dist.getParameter(param_name).setValue(param_value);
			}

			dist.updateCM(); // Updates values of c and mean
		}
		return dist;
	}

	/**
	 * Returns the type of a station, reconstructing it from section names. This method must be
	 * modified if a new station type is inserted.
	 * @param station element containing sections
	 * @return station type as expected by CommonModel / JMODELModel
	 */
	protected static String getStationType(Element station) {
		NodeList sections = station.getElementsByTagName(XML_E_STATION_SECTION);
		String[] sectionNames = new String[sections.getLength()];
		// Gets all section classnames
		for (int i = 0; i < sectionNames.length; i++) {
			sectionNames[i] = ((Element) sections.item(i)).getAttribute(XML_A_STATION_SECTION_CLASSNAME);
		}
		// Finds station type, basing on section names
		if (sectionNames[0].equals(CLASSNAME_SINK)) {
			return STATION_TYPE_SINK;
		} else if (sectionNames[0].equals(CLASSNAME_SOURCE)) {
			return STATION_TYPE_SOURCE;
		} else if (sectionNames[0].equals(CLASSNAME_TERMINAL)) {
			return STATION_TYPE_TERMINAL;
		} else if (sectionNames[1].equals(CLASSNAME_DELAY)) {
			return STATION_TYPE_DELAY;
		} else if (sectionNames[1].equals(CLASSNAME_SERVER) || sectionNames[1].equals(CLASSNAME_PSSERVER)) {
			return STATION_TYPE_SERVER;
		} else if (sectionNames[2].equals(CLASSNAME_FORK)) {
			return STATION_TYPE_FORK;
		} else if (sectionNames[0].equals(CLASSNAME_JOIN)) {
			return STATION_TYPE_JOIN;
		} else if (sectionNames[1].equals(CLASSNAME_TUNNEL)) {
			return STATION_TYPE_ROUTER;
		} else if (sectionNames[1].equals(CLASSNAME_LOGGER)) {
			return STATION_TYPE_LOGGER;
		}
		return null;
	}

	// -----------------------------------------------------------------------------------------------

	// --- Measure section ---------------------------------------------------------------------------
	/**
	 * Parses all informations on measures to be taken during simulation
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parseMeasures(Element root, CommonModel model) {
		NodeList measures = root.getElementsByTagName(XML_E_MEASURE);
		Object stationKey, classKey;
		String type;
		Double alpha, precision;
		for (int i = 0; i < measures.getLength(); i++) {
			String stationName = ((Element) measures.item(i)).getAttribute(XML_A_MEASURE_STATION);
			String nodeType = ((Element) measures.item(i)).getAttribute(XML_A_MEASURE_NODETYPE);
			if (stationName != null && !stationName.equals("")) {
				if (nodeType.equalsIgnoreCase(NODETYPE_REGION)) {
					stationKey = regions.get(stationName);
				} else {
					stationKey = stations.get(stationName);
				}
			} else {
				stationKey = null;
			}
			String className = ((Element) measures.item(i)).getAttribute(XML_A_MEASURE_CLASS);
			if (className != null && !className.equals("")) {
				classKey = classes.get(className);
			} else {
				classKey = null;
			}
			type = ((Element) measures.item(i)).getAttribute(XML_A_MEASURE_TYPE);

			//Begins all backward compatibility conditions for Changes of Labels of Perf Index.
			// Supports old names
			if ("Customer Number".equalsIgnoreCase(type) && "".equalsIgnoreCase(stationName)) {
				type = SimulationDefinition.MEASURE_S_CN;
			}
			if ("System Customer Number".equalsIgnoreCase(type)) {
				type = SimulationDefinition.MEASURE_S_CN;
			}
			if ("Number of Customers".equalsIgnoreCase(type) && "".equalsIgnoreCase(stationName)) {
				type = SimulationDefinition.MEASURE_S_CN;
			}
			if ("Queue Length".equalsIgnoreCase(type)) {
				type = SimulationDefinition.MEASURE_QL;
			}
			if ("Customer Number".equalsIgnoreCase(type) && !"".equalsIgnoreCase(stationName)) {
				type = SimulationDefinition.MEASURE_QL;
			}
			//Ends the backward compatibility conditions
			// Inverts alpha
			alpha = new Double(1 - Double.parseDouble(((Element) measures.item(i)).getAttribute(XML_A_MEASURE_ALPHA)));
			precision = Double.valueOf(((Element) measures.item(i)).getAttribute(XML_A_MEASURE_PRECISION));
			// Adds measure to the model
			model.addMeasure(type, stationKey, classKey, alpha, precision);
		}
	}

	// -----------------------------------------------------------------------------------------------

	// --- Connection section ------------------------------------------------------------------------
	/**
	 * Parses all informations on connections to be made into model
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parseConnections(Element root, CommonModel model) {
		NodeList connections = root.getElementsByTagName(XML_E_CONNECTION);
		Object sourceKey, targetKey;
		for (int i = 0; i < connections.getLength(); i++) {
			sourceKey = stations.get(((Element) connections.item(i)).getAttribute(XML_A_CONNECTION_SOURCE));
			targetKey = stations.get(((Element) connections.item(i)).getAttribute(XML_A_CONNECTION_TARGET));
			// Adds connection to data structure
			model.setConnected(sourceKey, targetKey, true);
		}
	}

	// -----------------------------------------------------------------------------------------------

	// --- Preloading section ------------------------------------------------------------------------
	/**
	 * Parses all informations on preloading to be added to the model
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parsePreloading(Element root, CommonModel model) {
		NodeList preload = root.getElementsByTagName(XML_E_PRELOAD);
		if (preload.getLength() > 0) {
			// For every station, search for classes and initial jobs in queue
			NodeList station_pop = ((Element) preload.item(0)).getElementsByTagName(XML_E_STATIONPOPULATIONS);
			for (int i = 0; i < station_pop.getLength(); i++) {
				Object stationKey = stations.get(((Element) station_pop.item(i)).getAttribute(XML_A_PRELOADSTATION_NAME));
				NodeList class_pop = ((Element) station_pop.item(i)).getElementsByTagName(XML_E_CLASSPOPULATION);
				for (int j = 0; j < class_pop.getLength(); j++) {
					Object classKey = classes.get(((Element) class_pop.item(j)).getAttribute(XML_A_CLASSPOPULATION_NAME));
					Integer jobs = new Integer(((Element) class_pop.item(j)).getAttribute(XML_A_CLASSPOPULATION_POPULATION));
					// Sets preloading informations
					model.setPreloadedJobs(jobs, stationKey, classKey);
				}
			}
		}

	}

	// -----------------------------------------------------------------------------------------------

	// --- Blocking regions section ------------------------------------------------------------------
	/**
	 * Parses all informations on blocking regions to be added to the model
	 * @param root root element of XML Document
	 * @param model data structure where all properties have to be set
	 */
	protected static void parseBlockingRegions(Element root, CommonModel model) {
		regions = new TreeMap<String, Object>();
		NodeList regionNodes = root.getElementsByTagName(XML_E_REGION);
		// Creates each region into data structure
		for (int i = 0; i < regionNodes.getLength(); i++) {
			Element region = (Element) regionNodes.item(i);
			String name = region.getAttribute(XML_A_REGION_NAME);
			String type = region.getAttribute(XML_A_REGION_TYPE);
			if (type == null || type.equals("")) {
				type = Defaults.get("blockingRegionType");
			}
			// Adds blocking region to data structure
			Object key = model.addBlockingRegion(name, type);
			regions.put(name, key);
			// Now parses all included stations
			NodeList nodes = region.getElementsByTagName(XML_E_REGIONNODE);
			for (int j = 0; j < nodes.getLength(); j++) {
				String stationName = ((Element) nodes.item(j)).getAttribute(XML_A_REGIONNODE_NAME);
				model.addRegionStation(key, stations.get(stationName));
			}
			// Now parses class constraints
			NodeList classConstraints = region.getElementsByTagName(XML_E_CLASSCONSTRAINT);
			for (int j = 0; j < classConstraints.getLength(); j++) {
				Element constraint = (Element) classConstraints.item(j);
				//TODO Add support for Double class constraints
				int num = new Double(constraint.getAttribute(XML_A_CLASSCONSTRAINT_MAXJOBS)).intValue();
				model.setRegionClassCustomerConstraint(key, classes.get(constraint.getAttribute(XML_A_CLASSCONSTRAINT_CLASS)), new Integer(num));
			}
			//TODO parse weights...

			// Now parses global costraint
			Element globalConstraint = (Element) region.getElementsByTagName(XML_E_GLOBALCONSTRAINT).item(0);
			//TODO Add support for Double global constraints
			int num = new Double(globalConstraint.getAttribute(XML_A_GLOBALCONSTRAINT_MAXJOBS)).intValue();
			model.setRegionCustomerConstraint(key, new Integer(num));
			// Now parses drop rules
			NodeList drop = region.getElementsByTagName(XML_E_DROPRULES);
			for (int j = 0; j < drop.getLength(); j++) {
				Element rule = (Element) drop.item(j);
				model.setRegionClassDropRule(key, classes.get(rule.getAttribute(XML_A_DROPRULES_CLASS)), Boolean.valueOf(rule
						.getAttribute(XML_A_DROPRULES_DROP)));
			}
		}
	}

	// -----------------------------------------------------------------------------------------------

	// --- Generic XML Loader ------------------------------------------------------------------------
	/**
	 * Loads an XML file, returning the Document rappresentation of it. This method is generic
	 * and can be used to load every xml file. Actually it is used by <code>XMLReader</code>
	 * and by <code>GuiXMLReader</code>. This method will validate input file.
	 * @param filename name of the file to be loaded
	 * @param schemaSource url of schema to be used to validate the model
	 * @return Document rappresentation of input xml file
	 */
	public static Document loadXML(String filename, String schemaSource) {
		DOMParser parser = new DOMParser();
		try {
			// Sets validation only if needed
			if (schemaSource != null) {
				parser.setFeature(NAMESPACES_FEATURE_ID, true);
				parser.setFeature(VALIDATION_FEATURE_ID, true);
				parser.setFeature(SCHEMA_VALIDATION_FEATURE_ID, true);
				parser.setFeature(VALIDATION_DYNAMIC_FEATURE_ID, true);

				parser.setProperty(EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID, schemaSource);
			}

			// Creates a DOM document from the xml file
			parser.parse(filename);
			return parser.getDocument();
		} catch (SAXException e) {
			System.err.println("XMLLoader Error - An error occurs while attempting to parse the document \"" + e.getMessage() + "\".");
			/*
			JOptionPane.showMessageDialog(null,
			        "An error occurs while attempting to parse the document \"" + e.getMessage() + "\".",
			        "JMT - File error",
			        JOptionPane.ERROR_MESSAGE); */
			return null;
		} catch (IOException e) {
			System.err.println("XMLLoader Error - An error occurs while attempting to parse the document.");
			return null;
		}
	}

	/**
	 * Loads an XML file, returning the Document rappresentation of it. This method is generic
	 * and can be used to load every xml file. Actually it is used by <code>XMLReader</code>
	 * and by <code>GuiXMLReader</code>. This method will <b>not</b> validate input file.
	 * @param filename name of the file to be loaded
	 * @return Document rappresentation of input xml file
	 */
	public static Document loadXML(String filename) {
		return loadXML(filename, null);
	}

	// -----------------------------------------------------------------------------------------------

	// --- Debug -------------------------------------------------------------------------------------
	/**
	 * This method is used for debug purpose to write a portion of xml on standard output.
	 * This can be removed freely!
	 * @param node node to be written on standard output
	 */
	protected static void write(Node node) {
		try {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty("indent", "yes");
			transformer.setOutputProperty("encoding", ENCODING);
			transformer.transform(new DOMSource(node), new StreamResult(System.out));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerFactoryConfigurationError transformerFactoryConfigurationError) {
			transformerFactoryConfigurationError.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		}

	}
	// -----------------------------------------------------------------------------------------------
}
