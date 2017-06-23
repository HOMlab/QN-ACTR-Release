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

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-07-13] disable normalizeProbabilities
 * 
 */

package jmt.gui.common.xml;

import java.io.File;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.engine.log.LoggerParameters;
import jmt.engine.random.EmpiricalEntry;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.common.serviceStrategies.LDStrategy;
import jmt.gui.common.serviceStrategies.ZeroStrategy;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 15-lug-2005
 * Time: 10.56.01
 * Modified by Bertoli Marco
 */
public class XMLWriter implements CommonConstants, XMLConstantNames {
	/*defines matching between gui representation and engine names for queue
	strategies, e.g. FCFS = TailStrategy.*/
	protected static final HashMap queuePutStrategyNamesMatchings = new HashMap() {
		/**
		 *
		 */
		private static final long serialVersionUID = 1L;

		{
			put(QUEUE_STRATEGY_FCFS, "TailStrategy");
			put(QUEUE_STRATEGY_LCFS, "HeadStrategy");
		}
	};

	protected static final HashMap priorityQueuePutStrategyNamesMatchings = new HashMap() {
		/**
		 *
		 */
		private static final long serialVersionUID = 1L;

		{
			put(QUEUE_STRATEGY_FCFS, "TailStrategyPriority");
			put(QUEUE_STRATEGY_LCFS, "HeadStrategyPriority");
		}
	};

	/*defines matching between gui representation and engine names for drop
	rules.*/
	protected static final HashMap dropRulesNamesMatchings = new HashMap() {
		/**
		 *
		 */
		private static final long serialVersionUID = 1L;

		{
			put(FINITE_DROP, "drop");
			put(FINITE_WAITING, "waiting queue");
			put(FINITE_BLOCK, "BAS blocking");
		}
	};

	public static final String strategiesClasspathBase = "jmt.engine.NetStrategies.";
	public static final String queuegetStrategiesSuffix = "QueueGetStrategies.";
	public static final String queueputStrategiesSuffix = "QueuePutStrategies.";
	public static final String routingStrategiesSuffix = "RoutingStrategies.";
	public static final String serviceStrategiesSuffix = "ServiceStrategies.";
	public static final String distributionContainerClasspath = "jmt.engine.random.DistributionContainer";

	public static void writeXML(String fileName, CommonModel model) {
		writeToResult(new StreamResult(new File(fileName)), model, fileName);
	}

	public static void writeXML(File xmlFile, CommonModel model) {
		writeToResult(new StreamResult(xmlFile), model, xmlFile.getName());
	}

	public static void writeXML(OutputStream out, CommonModel model) {
		writeToResult(new StreamResult(out), model, "model");
	}

	public static void writeXML(String fileName, Document doc) {
		if (doc == null) {
			return;
		}
		try {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty("indent", "yes");
			transformer.setOutputProperty("encoding", ENCODING);
			transformer.transform(new DOMSource(doc), new StreamResult(new File(fileName)));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerFactoryConfigurationError transformerFactoryConfigurationError) {
			transformerFactoryConfigurationError.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		}
	}

	public static Document getDocument(CommonModel model, String modelName) {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder;
		try {
			docBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			return null;
		}
		Document modelDoc = docBuilder.newDocument();
		writeModel(modelDoc, model, modelName);
		return modelDoc;
	}

	private static void writeToResult(Result res, CommonModel model, String modelName) {
		Document modelDoc = getDocument(model, modelName);
		if (modelDoc == null) {
			return;
		}
		try {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty("indent", "yes");
			transformer.setOutputProperty("encoding", ENCODING);
			transformer.transform(new DOMSource(modelDoc), res);
		} catch (TransformerConfigurationException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerFactoryConfigurationError transformerFactoryConfigurationError) {
			transformerFactoryConfigurationError.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		}
	}

	static protected void writeModel(Document modelDoc, CommonModel model, String modelName) {
		Element elem = modelDoc.createElement(XML_DOCUMENT_ROOT);
		modelDoc.appendChild(elem);
		elem.setAttribute(XML_A_ROOT_NAME, modelName);
		elem.setAttribute("xsi:noNamespaceSchemaLocation", XML_DOCUMENT_XSD);
		elem.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		// Simulation seed - Bertoli Marco
		if (!model.getUseRandomSeed()) {
			elem.setAttribute(XML_A_ROOT_SEED, model.getSimulationSeed().toString());
		}
		// Max simulation time - Bertoli Marco
		if (model.getMaximumDuration().longValue() > 0) {
			elem.setAttribute(XML_A_ROOT_DURATION, model.getMaximumDuration().toString());
		}
		// Polling interval - Bertoli Marco
		elem.setAttribute(XML_A_ROOT_POLLING, Double.toString(model.getPollingInterval()));
		// Max Samples - Bertoli Marco
		elem.setAttribute(XML_A_ROOT_MAXSAMPLES, model.getMaxSimulationSamples().toString());
		// Disable Statistic
		elem.setAttribute(XML_A_ROOT_DISABLESTATISTIC, model.getDisableStatistic().toString());
		// Write attributes used by the logs - Michael Fercu
		elem.setAttribute(XML_A_ROOT_LOGPATH, model.getLoggingGlbParameter("path"));
		elem.setAttribute(XML_A_ROOT_LOGREPLACE, model.getLoggingGlbParameter("autoAppend"));
		elem.setAttribute(XML_A_ROOT_LOGDELIM, model.getLoggingGlbParameter("delim"));
		elem.setAttribute(XML_A_ROOT_LOGDECIMALSEPARATOR, model.getLoggingGlbParameter("decimalSeparator"));
		// Write all elements
		writeClasses(modelDoc, elem, model);
		writeStations(modelDoc, elem, model);
		writeMeasures(modelDoc, elem, model);
		writeConnections(modelDoc, elem, model);
		writeBlockingRegions(modelDoc, elem, model);
		writePreload(modelDoc, elem, model);
	}

	/*-----------------------------------------------------------------------------------
	*--------------------- Methods for construction of user classes ---------------------
	*-----------------------------------------------------------------------------------*/

	static protected void writeClasses(Document doc, Node simNode, CommonModel model) {
		Vector v = model.getClassKeys();
		for (int i = 0; i < v.size(); i++) {
			Object classKey = v.get(i);
			String classType = model.getClassType(classKey) == CLASS_TYPE_CLOSED ? "closed" : "open";
			Element userClass = doc.createElement(XML_E_CLASS);
			String[] attrsNames = new String[] { XML_A_CLASS_NAME, XML_A_CLASS_TYPE, XML_A_CLASS_PRIORITY, XML_A_CLASS_CUSTOMERS,
					XML_A_CLASS_REFSOURCE };
			String[] attrsValues = new String[] { model.getClassName(classKey), classType, String.valueOf(model.getClassPriority(classKey)),
					String.valueOf(model.getClassPopulation(classKey)), getSourceNameForClass(classKey, doc, model), };
			for (int j = 0; j < attrsNames.length; j++) {
				if (attrsValues[j] != null && !"null".equals(attrsValues[j])) {
					userClass.setAttribute(attrsNames[j], attrsValues[j]);
				}
			}
			simNode.appendChild(userClass);
		}
	}

	/**This method returns the name of the reference source for a class to be inserted into
	 * userclass elemnt's attribute.*/
	static protected String getSourceNameForClass(Object classKey, Document doc, CommonModel model) {
		if (model.getClassRefStation(classKey) != null) {
			return model.getStationName(model.getClassRefStation(classKey));
		} else {
			return null;
		}
	}

	/*-----------------------------------------------------------------------------------
	*----------------------- Methods for construction of stations -----------------------
	*-----------------------------------------------------------------------------------*/

	static protected void writeStations(Document doc, Node simNode, CommonModel model) {
		Vector stations = model.getStationKeys();
		Element elem;
		for (int i = 0; i < stations.size(); i++) {
			elem = doc.createElement(XML_E_STATION);
			elem.setAttribute(XML_A_STATION_NAME, model.getStationName(stations.get(i)));
			Object stationKey = stations.get(i);
			String stationType = model.getStationType(stationKey);
			if (STATION_TYPE_DELAY.equals(stationType)) {
				writeQueueSection(doc, elem, model, stationKey);
				writeDelaySection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_SERVER.equals(stationType)) {
				writeQueueSection(doc, elem, model, stationKey);
				writeServerSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_SOURCE.equals(stationType)) {
				writeSourceSection(doc, elem, model, stationKey);
				writeTunnelSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_TERMINAL.equals(stationType)) {
				writeTerminalSection(doc, elem, model, stationKey);
				writeTunnelSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_ROUTER.equals(stationType)) {
				writeQueueSection(doc, elem, model, stationKey);
				writeTunnelSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_LOGGER.equals(stationType)) {
				writeQueueSection(doc, elem, model, stationKey);
				writeLoggerSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_FORK.equals(stationType)) {
				writeQueueSection(doc, elem, model, stationKey);
				writeTunnelSection(doc, elem, model, stationKey);
				writeForkSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_JOIN.equals(stationType)) {
				writeJoinSection(doc, elem, model, stationKey);
				writeTunnelSection(doc, elem, model, stationKey);
				writeRouterSection(doc, elem, model, stationKey);
			} else if (STATION_TYPE_SINK.equals(stationType)) {
				writeSinkSection(doc, elem, model, stationKey);
			}
			simNode.appendChild(elem);
		}
	}

	/**
	 * Writes a Join input section
	 * <br>Author: Bertoli Marco
	 * @param doc document root
	 * @param node node where created section should be appended
	 * @param model data structure
	 * @param stationKey search's key for join
	 */
	private static void writeJoinSection(Document doc, Node node, CommonModel model, Object stationKey) {
		Element join = doc.createElement(XML_E_STATION_SECTION);
		join.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_JOIN);
		node.appendChild(join);
	}

	/**
	 * Writes a Fork output section
	 * <br>Author: Bertoli Marco
	 * @param doc document root
	 * @param node node where created section should be appended
	 * @param model data structure
	 * @param stationKey search's key for fork
	 */
	private static void writeForkSection(Document doc, Node node, CommonModel model, Object stationKey) {
		Element fork = doc.createElement(XML_E_STATION_SECTION);
		fork.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_FORK);
		node.appendChild(fork);
		// Creates jobsPerLink parameter
		XMLParameter jobsPerLink = new XMLParameter("jobsPerLink", model.getStationNumberOfServers(stationKey).getClass().getName(), null, model
				.getStationNumberOfServers(stationKey).toString(), false);
		// Creates block parameter
		XMLParameter block = new XMLParameter("block", model.getForkBlock(stationKey).getClass().getName(), null, model.getForkBlock(stationKey)
				.toString(), false);
		//...and adds them as fork's children
		jobsPerLink.appendParameterElement(doc, fork);
		block.appendParameterElement(doc, fork);

	}

	static protected void writeSourceSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_SOURCE);
		nodeNode.appendChild(elem);
		Vector classes = model.getClassKeys();
		//obtain classes that must be generated by this source
		Vector refClasses = getClassesForSource(model, stationKey);
		XMLParameter[] distrParams = new XMLParameter[classes.size()];
		for (int i = 0; i < distrParams.length; i++) {
			//if current class must be generated by this source
			Object currentClass = classes.get(i);
			if (refClasses.contains(currentClass)) {
				distrParams[i] = DistributionWriter.getDistributionParameter((Distribution) model.getClassDistribution(currentClass), model,
						currentClass);
			} else {
				//otherwise write a null parameter
				String name = "ServiceTimeStrategy";
				distrParams[i] = new XMLParameter(name, strategiesClasspathBase + serviceStrategiesSuffix + name, model.getClassName(currentClass),
						"null", true);
			}
		}
		//creating global service strategy parameter
		String gspName = "ServiceStrategy";
		XMLParameter globalStrategyParameter = new XMLParameter(gspName, strategiesClasspathBase + gspName, null, distrParams, false);
		//finally, create node from pareameters and append it to the section element
		globalStrategyParameter.appendParameterElement(doc, elem);
	}

	static protected void writeSinkSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_SINK);
		nodeNode.appendChild(elem);
	}

	static protected void writeQueueSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		//creating element representing queue section
		Element queue = doc.createElement(XML_E_STATION_SECTION);
		queue.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_QUEUE);
		nodeNode.appendChild(queue);
		//creating queue inner parameters
		//first create queue size node element
		String queueSize = model.getStationQueueCapacity(stationKey) == null ? "-1" : model.getStationQueueCapacity(stationKey).toString();
		XMLParameter queueLength = new XMLParameter("size", "java.lang.Integer", null, queueSize, false);
		//...and add it to queue element's children
		queueLength.appendParameterElement(doc, queue);

		// Drop policies. They are different for each customer class
		XMLParameter[] queueDropStrategies = new XMLParameter[model.getClassKeys().size()];
		XMLParameter queueDropStrategy = new XMLParameter("dropStrategies", String.class.getName(), null, queueDropStrategies, false);
		Vector classes = model.getClassKeys();
		for (int i = 0; i < queueDropStrategies.length; i++) {
			String dropStrategy = model.getDropRule(stationKey, classes.get(i));
			queueDropStrategies[i] = new XMLParameter("dropStrategy", String.class.getName(), model.getClassName(classes.get(i)),
					(String) dropRulesNamesMatchings.get(dropStrategy), true);
		}
		queueDropStrategy.appendParameterElement(doc, queue);

		/*queue get strategy, which is fixed to FCFS, as difference between strategies can
		be resolved by queueput strategies*/
		String strategy = "FCFSstrategy";
		boolean priority = false;
		if (QUEUE_STRATEGY_STATION_PS.equals(model.getStationQueueStrategy(stationKey))) {
			strategy = "PSStrategy";
		} else if (QUEUE_STRATEGY_STATION_QUEUE_PRIORITY.equals(model.getStationQueueStrategy(stationKey))) {
			priority = true;
		}
		XMLParameter queueGetStrategy = new XMLParameter(strategy, strategiesClasspathBase + queuegetStrategiesSuffix + "FCFSstrategy", null,
				(String) null, false);
		queueGetStrategy.appendParameterElement(doc, queue);
		/*At last, queue put parameter, which can be defined differently for each
		customerclass, so a more complex parameter structure is required*/
		XMLParameter[] queuePutStrategies = new XMLParameter[model.getClassKeys().size()];
		XMLParameter queuePutStrategy = new XMLParameter("NetStrategy", strategiesClasspathBase + "QueuePutStrategy", null, queuePutStrategies, false);
		for (int i = 0; i < queuePutStrategies.length; i++) {
			String queueStrategy = model.getQueueStrategy(stationKey, classes.get(i));
			HashMap map = priority ? priorityQueuePutStrategyNamesMatchings : queuePutStrategyNamesMatchings;
			String queueputStrategyName = (String) map.get(queueStrategy);
			queuePutStrategies[i] = new XMLParameter(queueputStrategyName, strategiesClasspathBase + queueputStrategiesSuffix + queueputStrategyName,
					model.getClassName(classes.get(i)), (String) null, true);
		}
		queuePutStrategy.appendParameterElement(doc, queue);
	}

	static protected void writeTerminalSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_TERMINAL);
		nodeNode.appendChild(elem);
		Vector classes = model.getClassKeys();
		//obtain classes that must be generated by this terminal
		Vector refClasses = getClassesForTerminal(model, stationKey);
		XMLParameter[] distrParams = new XMLParameter[classes.size()];
		for (int i = 0; i < distrParams.length; i++) {
			//if current class must be generated by this terminal
			Object currentClass = classes.get(i);
			if (refClasses.contains(currentClass)) {
				distrParams[i] = new XMLParameter("numberOfJobs", "java.lang.Integer", model.getClassName(currentClass), model.getClassPopulation(
						currentClass).toString(), true);
			} else {
				//otherwise write a null parameter
				distrParams[i] = new XMLParameter("numberOfJobs", "java.lang.Integer", model.getClassName(currentClass), "-1", true);
			}
		}
		//creating global population parameter
		XMLParameter globalStrategyParameter = new XMLParameter("NumberOfJobs", "java.lang.Integer", null, distrParams, false);
		//finally, create node from pareameters and append it to the section element
		globalStrategyParameter.appendParameterElement(doc, elem);
	}

	static protected void writeServerSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		if (QUEUE_STRATEGY_STATION_PS.equals(model.getStationQueueStrategy(stationKey))) {
			elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_PSSERVER);
		} else {
			elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_SERVER);
		}

		nodeNode.appendChild(elem);
		Vector classes = model.getClassKeys();
		//creating number of servers node element
		Integer maxJobs = model.getStationNumberOfServers(stationKey);
		XMLParameter numOfServers = new XMLParameter("maxJobs", "java.lang.Integer", null, maxJobs != null ? maxJobs.toString() : "-1", false);
		numOfServers.appendParameterElement(doc, elem);
		//creating visits node element
		XMLParameter[] visits = new XMLParameter[classes.size()];
		for (int i = 0; i < visits.length; i++) {
			visits[i] = new XMLParameter("numberOfVisits", "java.lang.Integer", model.getClassName(classes.get(i)), "1", true);
		}
		XMLParameter visitsParam = new XMLParameter("numberOfVisits", "java.lang.Integer", null, visits, false);
		visitsParam.appendParameterElement(doc, elem);

		getServiceSection(model, stationKey).appendParameterElement(doc, elem);
	}

	/**
	 * Returns a service section rapresentation in XMLParameter format
	 * @param model model data structure
	 * @param stationKey search's key for current station
	 * @return service section rapresentation in XMLParameter format
	 * Author: Bertoli Marco
	 */
	protected static XMLParameter getServiceSection(CommonModel model, Object stationKey) {
		Vector classes = model.getClassKeys();
		//creating set of service time distributions
		XMLParameter[] distrParams = new XMLParameter[classes.size()];
		Object currentClass;
		for (int i = 0; i < distrParams.length; i++) {
			currentClass = classes.get(i);
			Object serviceDistribution = model.getServiceTimeDistribution(stationKey, currentClass);
			if (serviceDistribution instanceof Distribution) {
				// Service time is a Distribution and not a LDService
				distrParams[i] = DistributionWriter.getDistributionParameter((Distribution) serviceDistribution, model, currentClass);
			} else if (serviceDistribution instanceof ZeroStrategy) {
				// Zero Service Time Strategy --- Bertoli Marco
				distrParams[i] = new XMLParameter("ZeroTimeServiceStrategy", ZeroStrategy.getEngineClassPath(), model.getClassName(currentClass),
						(String) null, true);
			} else {
				// Load dependent Service Time Strategy --- Bertoli Marco
				LDStrategy strategy = (LDStrategy) serviceDistribution;
				XMLParameter[] ranges = new XMLParameter[strategy.getRangeNumber()];
				Object[] rangeKeys = strategy.getAllRanges();

				for (int j = 0; j < ranges.length; j++) {
					// Creates "from" parameter
					XMLParameter from = new XMLParameter("from", Integer.class.getName(), null,
							Integer.toString(strategy.getRangeFrom(rangeKeys[j])), true);
					// Creates "distribution" parameter
					XMLParameter[] distribution = DistributionWriter.getDistributionParameter(strategy.getRangeDistribution(rangeKeys[j]));
					// Creates "function" parameter (mean value of the distribution)
					XMLParameter function = new XMLParameter("function", String.class.getName(), null, strategy
							.getRangeDistributionMean(rangeKeys[j]), true);
					ranges[j] = new XMLParameter("LDParameter", strategiesClasspathBase + serviceStrategiesSuffix + "LDParameter", null,
							new XMLParameter[] { from, distribution[0], distribution[1], function }, true);
					// Sets array = false as it's not an array of equal elements
					ranges[j].parameterArray = "false";
				}
				// Creates LDParameter array
				XMLParameter LDParameter = new XMLParameter("LDParameter", strategiesClasspathBase + serviceStrategiesSuffix + "LDParameter", null,
						ranges, true);
				// Creates Service strategy
				distrParams[i] = new XMLParameter("LoadDependentStrategy", strategiesClasspathBase + serviceStrategiesSuffix
						+ "LoadDependentStrategy", model.getClassName(currentClass), new XMLParameter[] { LDParameter }, true);
				distrParams[i].parameterArray = "false";
			}
		}
		XMLParameter globalDistr = new XMLParameter("ServiceStrategy", strategiesClasspathBase + "ServiceStrategy", null, distrParams, false);
		return globalDistr;
	}

	static protected void writeTunnelSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_TUNNEL);
		nodeNode.appendChild(elem);
	}

	/**
	 * Write all parameters for a Logger section.
	 * @param doc XML document
	 * @param node XML hierarchy node
	 * @param model link to data structure
	 * @param stationKey key of search for this source station into data structure
	 * @author Michael Fercu (Bertoli Marco)
	 *		   Date: 08-aug-2008
	 * @see jmt.engine.log.LoggerParameters LoggerParameters
	 * @see jmt.gui.common.definitions.CommonModel#getLoggingParameters CommonModel.getLoggingParameters()
	 * @see jmt.gui.common.XMLReader#parseLogger XMLReader.parseLogger()
	 * @see jmt.engine.NodeSections.LogTunnel LogTunnel
	 */
	static protected void writeLoggerSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_LOGGER);
		nodeNode.appendChild(elem);

		// Get this station's logger parameters
		LoggerParameters loggerParameters = (LoggerParameters) model.getLoggingParameters(stationKey);

		XMLParameter name = new XMLParameter(XML_LOG_FILENAME, "java.lang.String", null, loggerParameters.name.toString(), false);
		name.appendParameterElement(doc, elem);
		loggerParameters.path = model.getLoggingGlbParameter("path"); // temporary fix
		XMLParameter path = new XMLParameter(XML_LOG_FILEPATH, "java.lang.String", null, loggerParameters.getpath().toString(), false);
		path.appendParameterElement(doc, elem);
		XMLParameter logExecTimestamp = new XMLParameter(XML_LOG_B_EXECTIMESTAMP, "java.lang.Boolean", null, loggerParameters.boolExecTimestamp
				.toString(), false);
		logExecTimestamp.appendParameterElement(doc, elem);
		XMLParameter logLoggerName = new XMLParameter(XML_LOG_B_LOGGERNAME, "java.lang.Boolean", null, loggerParameters.boolLoggername.toString(),
				false);
		logLoggerName.appendParameterElement(doc, elem);
		XMLParameter logTimeStamp = new XMLParameter(XML_LOG_B_TIMESTAMP, "java.lang.Boolean", null, loggerParameters.boolTimeStamp.toString(), false);
		logTimeStamp.appendParameterElement(doc, elem);
		XMLParameter logJobID = new XMLParameter(XML_LOG_B_JOBID, "java.lang.Boolean", null, loggerParameters.boolJobID.toString(), false);
		logJobID.appendParameterElement(doc, elem);
		XMLParameter logJobClass = new XMLParameter(XML_LOG_B_JOBCLASS, "java.lang.Boolean", null, loggerParameters.boolJobClass.toString(), false);
		logJobClass.appendParameterElement(doc, elem);
		XMLParameter logTimeSameClass = new XMLParameter(XML_LOG_B_TIMESAMECLS, "java.lang.Boolean", null, loggerParameters.boolTimeSameClass
				.toString(), false);
		logTimeSameClass.appendParameterElement(doc, elem);
		XMLParameter logTimeAnyClass = new XMLParameter(XML_LOG_B_TIMEANYCLS, "java.lang.Boolean", null,
				loggerParameters.boolTimeAnyClass.toString(), false);
		logTimeAnyClass.appendParameterElement(doc, elem);
		XMLParameter classSize = new XMLParameter("numClasses", "java.lang.Integer", null, new Integer(model.getClassKeys().size()).toString(), false);
		classSize.appendParameterElement(doc, elem);
	}

	static protected void writeDelaySection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_DELAY);
		nodeNode.appendChild(elem);
		getServiceSection(model, stationKey).appendParameterElement(doc, elem);
	}

	static protected void writeRouterSection(Document doc, Node nodeNode, CommonModel model, Object stationKey) {
		Element elem = doc.createElement(XML_E_STATION_SECTION);
		elem.setAttribute(XML_A_STATION_SECTION_CLASSNAME, CLASSNAME_ROUTER);
		nodeNode.appendChild(elem);
		//creating list of paramters for each single routing strategy
		Vector classes = model.getClassKeys();
		XMLParameter[] routingStrats = new XMLParameter[classes.size()];
		Object currentClass;
		for (int i = 0; i < routingStrats.length; i++) {
			currentClass = classes.get(i);
			routingStrats[i] = RoutingStrategyWriter.getRoutingStrategyParameter(
					(RoutingStrategy) model.getRoutingStrategy(stationKey, currentClass), model, currentClass, stationKey);
		}
		XMLParameter globalRouting = new XMLParameter("RoutingStrategy", strategiesClasspathBase + "RoutingStrategy", null, routingStrats, false);
		globalRouting.appendParameterElement(doc, elem);
	}

	//returns a list of keys for customer classes generated by a specific source station
	static protected Vector getClassesForSource(CommonModel model, Object stationKey) {
		Vector keys = model.getClassKeys();
		Vector classes = new Vector();
		for (int i = 0; i < keys.size(); i++) {
			if (CLASS_TYPE_OPEN == model.getClassType(keys.get(i)) && model.getClassRefStation(keys.get(i)) == stationKey) {
				classes.add(keys.get(i));
			}
		}
		return classes;
	}

	//returns a list of keys for customer classes generated by a specific terminal station
	static protected Vector getClassesForTerminal(CommonModel model, Object stationKey) {
		Vector keys = model.getClassKeys();
		Vector classes = new Vector();
		for (int i = 0; i < keys.size(); i++) {
			if (CLASS_TYPE_CLOSED == model.getClassType(keys.get(i)) && model.getClassRefStation(keys.get(i)) == stationKey) {
				classes.add(keys.get(i));
			}
		}
		return classes;
	}

	/*-----------------------------------------------------------------------------------
	*----------------------- Methods for construction of measures -----------------------
	*-----------------------------------------------------------------------------------*/
	static protected void writeMeasures(Document doc, Node simNode, CommonModel model) {
		Vector v = model.getMeasureKeys();
		for (int i = 0; i < v.size(); i++) {
			Element elem = doc.createElement(XML_E_MEASURE);
			Object key = v.get(i);
			String station = model.getStationName(model.getMeasureStation(key)), userClass = model.getClassName(model.getMeasureClass(key)), type = model
					.getMeasureType(v.get(i));
			elem.setAttribute(XML_A_MEASURE_TYPE, type);
			String name = "";

			// This is a region measure
			if (model.getRegionKeys().contains(model.getMeasureStation(key))) {
				station = model.getRegionName(model.getMeasureStation(key));
				elem.setAttribute(XML_A_MEASURE_NODETYPE, NODETYPE_REGION);
			} else {
				elem.setAttribute(XML_A_MEASURE_NODETYPE, NODETYPE_STATION);
			}

			if (station != null) {
				elem.setAttribute(XML_A_MEASURE_STATION, station);
				name += station + "_";
			} else {
				elem.setAttribute(XML_A_MEASURE_STATION, "");
			}

			if (userClass != null) {
				elem.setAttribute(XML_A_MEASURE_CLASS, userClass);
				name += userClass + "_";
			} else {
				elem.setAttribute(XML_A_MEASURE_CLASS, "");
			}

			name += type;

			// Inverts alpha and keeps only 10 decimal cifres
			double alpha = 1 - model.getMeasureAlpha(key).doubleValue();
			alpha = Math.rint(alpha * 1e10) / 1e10;
			elem.setAttribute(XML_A_MEASURE_ALPHA, Double.toString(alpha));
			elem.setAttribute(XML_A_MEASURE_PRECISION, model.getMeasurePrecision(key).toString());
			elem.setAttribute(XML_A_MEASURE_NAME, name);
			elem.setAttribute(XML_A_MEASURE_VERBOSE, "false");
			simNode.appendChild(elem);
		}
	}

	/*-----------------------------------------------------------------------------------
	*--------------------- Methods for construction of connections ----------------------
	*-----------------------------------------------------------------------------------*/
	static protected void writeConnections(Document doc, Node simNode, CommonModel model) {
		Vector stations = model.getStationKeys();
		String[] stationNames = new String[stations.size()];
		for (int i = 0; i < stationNames.length; i++) {
			stationNames[i] = model.getStationName(stations.get(i));
		}
		for (int i = 0; i < stationNames.length; i++) {
			for (int j = 0; j < stationNames.length; j++) {
				if (model.areConnected(stations.get(i), stations.get(j))) {
					Element elem = doc.createElement(XML_E_CONNECTION);
					elem.setAttribute(XML_A_CONNECTION_SOURCE, stationNames[i]);
					elem.setAttribute(XML_A_CONNECTION_TARGET, stationNames[j]);
					simNode.appendChild(elem);
				}
			}
		}
	}

	/*-----------------------------------------------------------------------------------
	--------------------- Methods for construction of preload data --- Bertoli Marco ----
	------------------------------------------------------------------------------------*/
	static protected void writePreload(Document doc, Node simNode, CommonModel model) {
		// Finds if and where preloading is needed
		Vector stations = model.getStationKeys();
		Vector classes = model.getClassKeys();
		// A map containing all stations that need preloading
		Vector<Element> p_stations = new Vector<Element>();
		for (int stat = 0; stat < stations.size(); stat++) {
			Object key = stations.get(stat);
			Vector<Element> p_class = new Vector<Element>();
			for (int i = 0; i < classes.size(); i++) {
				Object classKey = classes.get(i);
				Integer jobs = model.getPreloadedJobs(key, classKey);
				if (jobs.intValue() > 0) {
					Element elem = doc.createElement(XML_E_CLASSPOPULATION);
					elem.setAttribute(XML_A_CLASSPOPULATION_NAME, model.getClassName(classKey));
					elem.setAttribute(XML_A_CLASSPOPULATION_POPULATION, jobs.toString());
					p_class.add(elem);
				}
			}
			// If any preload is provided for this station, creates its element and adds it to p_stations
			if (!p_class.isEmpty()) {
				Element elem = doc.createElement(XML_E_STATIONPOPULATIONS);
				elem.setAttribute(XML_A_PRELOADSTATION_NAME, model.getStationName(key));
				while (!p_class.isEmpty()) {
					elem.appendChild(p_class.remove(0));
				}
				p_stations.add(elem);
			}
		}
		// If p_stations is not empty, creates a preload section for stations in p_stations
		if (!p_stations.isEmpty()) {
			Element preload = doc.createElement(XML_E_PRELOAD);
			while (!p_stations.isEmpty()) {
				preload.appendChild(p_stations.remove(0));
			}
			simNode.appendChild(preload);
		}
	}

	/*-----------------------------------------------------------------------------------
	------------------- Methods for construction of blocking regions --------------------
	--------------------------------- Bertoli Marco ------------------------------------*/
	static protected void writeBlockingRegions(Document doc, Node simNode, CommonModel model) {
		Vector regions = model.getRegionKeys();
		for (int reg = 0; reg < regions.size(); reg++) {
			Object key = regions.get(reg);
			Element region = doc.createElement(XML_E_REGION);
			// Set name attribute
			region.setAttribute(XML_A_REGION_NAME, model.getRegionName(key));
			// Set type attribute (optional)
			region.setAttribute(XML_A_REGION_TYPE, model.getRegionType(key));
			// Adds nodes (stations) to current region
			Iterator stations = model.getBlockingRegionStations(key).iterator();
			while (stations.hasNext()) {
				Element node = doc.createElement(XML_E_REGIONNODE);
				node.setAttribute(XML_A_REGIONNODE_NAME, model.getStationName(stations.next()));
				region.appendChild(node);
			}
			// Add class constraints
			Iterator classes = model.getClassKeys().iterator();
			// Add global constraint
			Element globalConstraint = doc.createElement(XML_E_GLOBALCONSTRAINT);
			globalConstraint.setAttribute(XML_A_GLOBALCONSTRAINT_MAXJOBS, model.getRegionCustomerConstraint(key).toString());
			region.appendChild(globalConstraint);
			while (classes.hasNext()) {
				Object classKey = classes.next();
				Element classConstraint = doc.createElement(XML_E_CLASSCONSTRAINT);
				classConstraint.setAttribute(XML_A_CLASSCONSTRAINT_CLASS, model.getClassName(classKey));
				classConstraint.setAttribute(XML_A_CLASSCONSTRAINT_MAXJOBS, model.getRegionClassCustomerConstraint(key, classKey).toString());
				region.appendChild(classConstraint);
			}
			// Add drop rules
			classes = model.getClassKeys().iterator();
			while (classes.hasNext()) {
				Object classKey = classes.next();
				Element drop = doc.createElement(XML_E_DROPRULES);
				drop.setAttribute(XML_A_DROPRULES_CLASS, model.getClassName(classKey));
				drop.setAttribute(XML_A_DROPRULES_DROP, model.getRegionClassDropRule(key, classKey).toString());
				region.appendChild(drop);
			}
			simNode.appendChild(region);
		}
	}

	/*-----------------------------------------------------------------------------------
	------------------------ Inner classes for more ease of use -------------------------
	------------------------------------------------------------------------------------*/
	protected static class XMLParameter {

		public boolean isSubParameter = false;

		public String parameterName;
		public String parameterClasspath;
		public String parameterRefClass;
		public String parameterValue;
		public String parameterArray;
		public XMLParameter[] parameters;

		public XMLParameter(String name, String classpath, String refClass, String value, boolean isSubParameter) {
			this(name, classpath, refClass, value, null, isSubParameter);
			parameterArray = "false";
		}

		public XMLParameter(String name, String classpath, String refClass, XMLParameter[] parameters, boolean isSubParameter) {
			this(name, classpath, refClass, null, parameters, isSubParameter);
			parameterArray = "true";
		}

		private XMLParameter(String name, String classpath, String refClass, String value, XMLParameter[] parameters, boolean isSubParameter) {
			parameterName = name;
			parameterClasspath = classpath;
			parameterRefClass = refClass;
			parameterValue = value;
			this.parameters = parameters;
			this.isSubParameter = isSubParameter;
			if (parameters != null) {
				if (parameters.length > 1) {
					parameterArray = "false";
				} else {
					parameterArray = "true";
				}
			} else {
				parameterArray = "false";
			}
		}

		public void appendParameterElement(Document doc, Element scope) {
			//creating inner element containing queue length
			Element parameter = doc.createElement(isSubParameter ? XML_E_SUBPARAMETER : XML_E_PARAMETER);
			if (parameterClasspath != null) {
				parameter.setAttribute(XML_A_PARAMETER_CLASSPATH, parameterClasspath);
			}
			if (parameterName != null) {
				parameter.setAttribute(XML_A_PARAMETER_NAME, parameterName);
			}
			if (parameterArray != null && "true".equals(parameterArray)) {
				parameter.setAttribute(XML_A_PARAMETER_ARRAY, parameterArray);
			}

			//adding element refclass for this parameter
			if (parameterRefClass != null) {
				Element refclass = doc.createElement(XML_E_PARAMETER_REFCLASS);
				refclass.appendChild(doc.createTextNode(parameterRefClass));
				scope.appendChild(refclass);
			}
			//adding element value of parameter
			if (parameterValue != null) {
				Element value = doc.createElement(XML_E_PARAMETER_VALUE);
				value.appendChild(doc.createTextNode(parameterValue));
				parameter.appendChild(value);
			}
			if (parameters != null) {
				for (XMLParameter parameter2 : parameters) {
					if (parameter2 != null) {
						parameter2.appendParameterElement(doc, parameter);
					}
				}
			}
			scope.appendChild(parameter);
		}
	}

	/**This class provides a simple method to obtain XMLparameter representation
	 * of a distribution object. Creation of a distribution parameter is a bit awkward, so
	 * I'll explain it as best as I can as it follows.
	 * generally a distribution is associated to a service time strategy, either it is
	 * an interarrival distribution for open classes job generation, or a proper service
	 * time distribution for a certain station. As a result, distribution parameter is
	 * inserted in a ServiceTimeStrategy parameter which is the one userclass is
	 * associated to. Inside this parameter node should be inserted 2 subParameter nodes:
	 * <br>-One for distribution description(containing distribution classpath and name)
	 * <br>- One containing all of the distribution constructor parameters.
	 * <br>The first one has null value, the second contains a list of parameters which, as
	 * they are different from each other, they are not considered as array. Then, the
	 * node which contains them has no value for array attribute.*/
	protected static class DistributionWriter {

		/*returns a distribution in XMLParameter format, to allow nesting it in
		other parameters. */
		static XMLParameter getDistributionParameter(Distribution distr, CommonModel model, Object classKey) {
			XMLParameter[] distribution = getDistributionParameter(distr);
			XMLParameter returnValue = new XMLParameter("ServiceTimeStrategy", strategiesClasspathBase + serviceStrategiesSuffix
					+ "ServiceTimeStrategy", model.getClassName(classKey), new XMLParameter[] { distribution[0], distribution[1] }, true);
			/*although this parameter contains several others, array attribute must be set
			to "false", as their type are not neccessarily equal*/
			returnValue.parameterArray = "false";
			return returnValue;
		}

		/**
		 * Returns a Distribution in XMLParameter format without refclass. This is used to write
		 * load dependent service section distributions
		 * @param distr distribution to be written
		 * @return the two object to represent a distribution: distribution and its parameter object
		 * Author: Bertoli Marco
		 */
		static XMLParameter[] getDistributionParameter(Distribution distr) {

			// a list of direct parameter -> parameter which must be passed directly to the distribution object
			List<XMLParameter> directParams = new Vector<XMLParameter>();
			// a list of parameters which are passed to the distribution parameter
			List<XMLParameter> nonDirectParams = new Vector<XMLParameter>();

			Distribution.Parameter distrPar;
			//Object valueObj;

			//parse over all parameters and add them to the apropriate list
			for (int i = 0; i < distr.getNumberOfParameters(); i++) {
				distrPar = distr.getParameter(i);
				if (distrPar.isDirectParameter()) {
					directParams.add(getParameter(distrPar));
				} else {
					nonDirectParams.add(getParameter(distrPar));
				}
			}

			//get an array of the direct parameters
			XMLParameter[] directPars = new XMLParameter[directParams.size()];
			for (int i = 0; i < directPars.length; i++) {
				directPars[i] = directParams.get(i);
			}

			//get an array of the non direct parameters
			XMLParameter[] nonDirectPars = new XMLParameter[nonDirectParams.size()];
			for (int i = 0; i < nonDirectPars.length; i++) {
				nonDirectPars[i] = nonDirectParams.get(i);
			}

			//create the distribution parameter with the direct parameters
			XMLParameter[] ret = new XMLParameter[2];
			ret[0] = new XMLParameter(distr.getName(), distr.getClassPath(), (String) null, directPars, true);
			//create the distribution parameter with the non direct parameters
			ret[1] = new XMLParameter("distrPar", distr.getParameterClassPath(), null, nonDirectPars, true);
			ret[0].parameterArray = "false";
			ret[1].parameterArray = "false";
			return ret;
		}

		/**
		 * Helper method to extract an XMLParameter from a Distribution parameter
		 * @param distrPar the distribution parameter
		 * @return the created XML Parameter
		 */
		static XMLParameter getParameter(Distribution.Parameter distrPar) {
			Object valueObj = distrPar.getValue();

			if (valueObj != null) {

				if (distrPar.getValue() instanceof Distribution) {

					XMLParameter[] distribution = getDistributionParameter((Distribution) valueObj);
					XMLParameter returnValue = new XMLParameter(distrPar.getName(), distributionContainerClasspath, null, new XMLParameter[] {
							distribution[0], distribution[1] }, true);
					/*
					 * although this parameter contains several others, array
					 * attribute must be set to "false", as their type are not
					 * neccessarily equal
					 */
					returnValue.parameterArray = "false";
					return returnValue;
				} else {
					String value = valueObj.toString();
					return new XMLParameter(distrPar.getName(), distrPar.getValueClass().getName(), null, value, true);
				}

			}

			return null;
		}

	}

	/**
	 * This class creates an xml parameter node given a
	 * jmt.gui.common.RoutingStrategy object.
	 */
	protected static class RoutingStrategyWriter {

		static XMLParameter getRoutingStrategyParameter(RoutingStrategy routingStrat, CommonModel model, Object classKey, Object stationKey) {
			//parameter containing array of empirical entries
			XMLParameter probRoutingPar = null;
			if (routingStrat.getValues() != null && routingStrat instanceof ProbabilityRouting) {
				Vector outputs = model.getForwardConnections(stationKey);
				Map values = routingStrat.getValues();
				
//				model.normalizeProbabilities(values, outputs, classKey, stationKey); //QN-Java
				
				XMLParameter[] empiricalEntries = new XMLParameter[outputs.size()];
				for (int i = 0; i < empiricalEntries.length; i++) {
					XMLParameter stationDest = new XMLParameter("stationName", String.class.getName(), null, model.getStationName(outputs.get(i)),
							true);
					String prob = values.get(outputs.get(i)).toString();
					XMLParameter routProb = new XMLParameter("probability", Double.class.getName(), null, prob, true);
					empiricalEntries[i] = new XMLParameter("EmpiricalEntry", EmpiricalEntry.class.getName(), null, new XMLParameter[] { stationDest,
							routProb }, true);
					empiricalEntries[i].parameterArray = "false";
				}
				probRoutingPar = new XMLParameter("EmpiricalEntryArray", EmpiricalEntry.class.getName(), null, empiricalEntries, true);
			}
			//creating parameter for empirical strategy: must be null if routing is empirical
			XMLParameter[] innerRoutingPar = probRoutingPar != null ? new XMLParameter[] { probRoutingPar } : null;
			XMLParameter routingStrategy = new XMLParameter(routingStrat.getName(), routingStrat.getClassPath(), model.getClassName(classKey),
					innerRoutingPar, true);
			routingStrategy.parameterArray = "false";
			return routingStrategy;
		}

		/*private static void normalizeProbabilities(Map values, Vector outputKeys){
		    Double[] probabilities = new Double[outputKeys.size()];
		    Object[] keys = new Object[outputKeys.size()];
		    outputKeys.toArray(keys);
		    //extract all values from map in array form
		    for(int i=0; i<keys.length; i++){
		        probabilities[i] = (Double)values.get(keys[i]);
		    }
		    values.clear();
		    //scan for null values and for total sum
		    double totalSum = 0.0;
		    int totalNonNull = 0;
		    boolean allNull = true;
		    for(int i=0; i<probabilities.length; i++){
		        if(probabilities[i]!=null){
		            totalSum += probabilities[i].doubleValue();
		            totalNonNull++;
		            allNull = false;
		        }
		    }
		    //modify non null values for their sum to match 1 and put null values to 1
		    for(int i=0; i<probabilities.length; i++){
		        if(probabilities[i]!=null || allNull){
		            if(totalSum==0){
		                probabilities[i] = new Double(1.0/(double)totalNonNull);
		            }else{
		                probabilities[i] = new Double(probabilities[i].doubleValue()/totalSum);
		            }
		        }else{
		            probabilities[i] = new Double(0.0);
		        }
		        values.put(keys[i], probabilities[i]);
		    }
		}*/
	}

}
