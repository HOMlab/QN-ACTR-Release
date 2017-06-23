/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

package jmt.engine.dataAnalysis;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.common.exception.NetException;
import jmt.engine.NodeSections.BlockingQueue;
import jmt.engine.QueueNet.BlockingRegion;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.QueueNet.SimConstants;
import jmt.engine.QueueNet.SimulationOutput;
import jmt.engine.log.JSimLogger;
import jmt.engine.simEngine.Simulation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Saves all the measure outputs in a xml file.
 *
 * @author Stefano, Bertoli Marco
 * @version 13-dic-2004 17.34.35
 * Modified by Bertoli Marco 01-jan-2006 --> BugFixed for linux
 * 8-mar-2006 Added support for global measures
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
 * 
 */
public class XMLSimulationOutput extends SimulationOutput {
	private static final String SEPARATOR = System.getProperty("file.separator");
	final static boolean DEBUG = false;

	private JSimLogger logger = JSimLogger.getLogger();

	//the root element of results xml
	Element root;
	//the Document object corresponding to the results xml
	Document doc;
	//the File containing the simulation results
	File resultsFile;

	//the File containing the original model definition
	File mvaModelDefinition;

	//the File containing the original model definition
	File simModelDefinition;

	public XMLSimulationOutput(Simulation simulation) {
		super(simulation);

		try {
			/////////////////////////////
			//Creating an empty XML Document

			DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();

			DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
			doc = docBuilder.newDocument();

			////////////////////////
			//Creating the XML tree

			//create the root element and add it to the document
			root = doc.createElement("solutions");
			doc.appendChild(root);

			//sets the attribute of the root
			if (sim.getName() != null) {
				root.setAttribute("modelName", sim.getName());
			} else {
				root.setAttribute("modelName", "Unknown Model Name");
			}
			root.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
			//sets the xsd schema of the results xml file
			root.setAttribute("xsi:noNamespaceSchemaLocation", "SIMmodeloutput.xsd");
			//these results have been obtained through a simulation
			root.setAttribute("solutionMethod", "simulation");

			//in the results file, we need the name of the xml file containing
			//the model definition
			String name = sim.getXmlModelDefPath();
			if (name != null) {
				mvaModelDefinition = new File(name);
			}

			//only file name is passed, not the absolute path
			//in fact definition and results will be put in the same directory

			//TODO: forse va messo assoluto...
			//root.setAttribute("modelDefinitionPath", simModelDefinition.getName());
			if (name != null) {
				root.setAttribute("modelDefinitionPath", name);
			} else {
				root.setAttribute("modelDefinitionPath", ".");
			}

		} catch (Exception e) {
			System.out.println(e);
		}

	}

	/**
	 * Writes the output of the specified measure.
	 */
	public void writeMeasure(Measure measure) {

		Element elem = doc.createElement("measure");
		root.appendChild(elem);

		DynamicDataAnalyzer analyzer = measure.getAnalyzer();

		// Checks null values to avoid problems under linux
		if (measure.getNodeName() == null || measure.getNodeName().equals("")) {
			// aggregate measure
			elem.setAttribute("station", "");
		} else {
			NetNode node = measure.getNetwork().getNode(measure.getNodeName());
			if (node.isBlockingRegionInputStation()) {
				elem.setAttribute("station", node.getBlockingRegionInputStation().getName());
				elem.setAttribute("nodeType", SimConstants.NODE_TYPE_REGION);
			} else {
				// class measure
				elem.setAttribute("station", measure.getNodeName());
				elem.setAttribute("nodeType", SimConstants.NODE_TYPE_STATION);
			}
		}
		// Checks null values to avoid problems under linux
		if (measure.getJobClassName() == null || measure.getJobClassName().equals("")) {
			//aggregate measure
			elem.setAttribute("class", "");
		} else {
			//class measure
			elem.setAttribute("class", measure.getJobClassName());
		}

		//finds and sets measure type
		int type = measure.getMeasureType();
		String typeName = "";

		switch (type) {
			case SimConstants.QUEUE_LENGTH:
				typeName = "Number of Customers";
				break;

			case SimConstants.QUEUE_TIME:
				typeName = "Queue Time";
				break;

			case SimConstants.UTILIZATION:
				typeName = "Utilization";
				break;

			case SimConstants.THROUGHPUT:
				typeName = "Throughput";
				break;

			case SimConstants.RESIDENCE_TIME:
				typeName = "Residence Time";
				break;

			case SimConstants.RESPONSE_TIME:
				typeName = "Response Time";
				break;

			case SimConstants.SYSTEM_RESPONSE_TIME:
				typeName = "System Response Time";
				break;

			case SimConstants.SYSTEM_THROUGHPUT:
				typeName = "System Throughput";
				break;

			case SimConstants.SYSTEM_JOB_NUMBER:
				typeName = "System Number of Customers";
				break;
			case SimConstants.DROP_RATE:
				typeName = "Drop Rate";
				break;
			case SimConstants.SYSTEM_DROP_RATE:
				typeName = "System Drop Rate";
				break;
			//Added by ASHANKA START
			//Adds the performance index system power for jsim
			case SimConstants.SYSTEM_POWER:
				typeName = "System Power";
				break;
			//Added by ASHANKA STOP
			case SimConstants.THROUGHPUT_PER_SINK:
				typeName = "Throughput per Sink";
				
			case SimConstants.RESPONSE_TIME_PER_SINK:
				typeName = "Response Time per Sink";
		}

		elem.setAttribute("measureType", typeName);

		//analyzer confidence requirements
		elem.setAttribute("maxSamples", Integer.toString(analyzer.getMaxData()));
		elem.setAttribute("precision", Double.toString(analyzer.getPrecision()));
		elem.setAttribute("alfa", Double.toString(analyzer.getAlfa()));

		//analyzer has been succesful?
		boolean success = analyzer.getSuccess();
		elem.setAttribute("successful", Boolean.toString(success));

		//number of analyzed and discarded samples
		elem.setAttribute("analyzedSamples", Integer.toString(measure.getAnalyzedSamples()));
		elem.setAttribute("discardedSamples", Integer.toString(measure.getDiscardedSamples()));

		//this is the extimated mean, but it may be wrong
		elem.setAttribute("meanValue", Double.toString(measure.getExtimatedMeanValue()));
		elem.setAttribute("upperLimit", Double.toString(measure.getUpperLimit()));
		elem.setAttribute("lowerLimit", Double.toString(measure.getLowerLimit()));
	}

	/**
	 * Writes the output of the drop measures, when blocking regions have been defined.
	 */
	public void writeMeasures_regionDroppedJobs() {

		BlockingRegion[] regions = sim.getAllRegions();
		JobClass[] classes = sim.getClasses();
		int classNumber = classes.length;

		if (regions != null) {
			//at least one region has been defined

			for (BlockingRegion region : regions) {
				try {
					//retrieves the blocking queue of the input station
					NodeSection ns = region.getInputStation().getSection(NodeSection.INPUT);

					if (ns instanceof BlockingQueue) {

						//For each class count dropped jobs
						for (int c = 0; c < classNumber; c++) {

							Element el = doc.createElement("measure");
							root.appendChild(el);

							String inputStation = region.getInputStation().getName();
							String className = classes[c].getName();

							el.setAttribute("station", inputStation);
							el.setAttribute("class", className);

							int arrivedJobs = ns.getIntSectionProperty(NodeSection.PROPERTY_ID_ARRIVED_JOBS);
							int droppedJobs = ((BlockingQueue) ns).getDroppedJobPerClass(c);

							double drop_percentage = (double) droppedJobs / arrivedJobs;

							//System.out.println(drop_percentage);

							el.setAttribute("meanValue", Double.toString(drop_percentage));

							//always true: it is not a confidence interval but an exact value
							boolean success = true;
							el.setAttribute("successful", Boolean.toString(success));
							el.setAttribute("measureType", "Dropped jobs");

							//number of analyzed and discarded samples: in this case their meaning is
							//received jobs and dropped jobs
							el.setAttribute("analyzedSamples", Integer.toString(arrivedJobs));
							el.setAttribute("discardedSamples", Integer.toString(droppedJobs));

							/*
							//analyzer confidence requirements
							el.setAttribute("maxSamples", Integer.toString(0));
							el.setAttribute("precision", Double.toString(0));
							el.setAttribute("alfa", Double.toString(0));
							*/
							logger.debug("Dropped jobs percentage region " + region.getName() + "-" + className + ": " + drop_percentage);
						}
					}

				} catch (NetException ne) {
					ne.printStackTrace();
					return;
				}
			}

		}

	}

	/**
	 * Writes the output of the drop measures, when blocking regions have been defined.
	 */
	public void writeMeasures_regionUtilization() {

		BlockingRegion[] regions = sim.getAllRegions();
		JobClass[] classes = sim.getClasses();
		int classNumber = classes.length;

		if (regions != null) {
			//at least one region has been defined

			for (BlockingRegion region : regions) {

				BlockingRegion reg = region;

				if (!reg.hasGlobalConstraint()) {
					continue;
				}
				//max capacity
				double maxCapacity = reg.getMaxCapacity();
				String[] stations = reg.getRegionNodeNames();

				double classTotalQueue, regUtilization;
				double classWeight;
				String className;

				for (int c = 0; c < classNumber; c++) {

					className = classes[c].getName();
					//logger.debug("Computing region U for class " + className);

					classWeight = reg.getClassWeights()[c];
					classTotalQueue = 0.0;
					boolean success = true;

					if (classWeight == 0.0) {
						regUtilization = 0.0;
					} else {
						for (String station : stations) {
							double queue = findQueueMeasure(className, station);
							if (queue == -1) {
								success = false;
								break;
							}
							//logger.debug("Queue at station " + stations[s] + ": " + Double.toString(queue));
							classTotalQueue += queue;
						}

						if (success) {
							//logger.debug("Total queue: " + Double.toString(classTotalQueue));
							//logger.debug("Weight: " + Double.toString(classWeight));
							regUtilization = (classTotalQueue * classWeight) / maxCapacity;
							logger.debug("Region Utilization for " + reg.getName() + "-" + className + ": " + Double.toString(regUtilization));
						} else {
							//regUtilization = -1;
							regUtilization = Double.NEGATIVE_INFINITY;
							logger.debug("Region Utilization for " + reg.getName() + "-" + className + " not computed");
						}
					}

					Element el = doc.createElement("measure");
					root.appendChild(el);

					String inputStation = reg.getInputStation().getName();
					el.setAttribute("station", inputStation);
					el.setAttribute("class", className);
					el.setAttribute("meanValue", Double.toString(regUtilization));
					el.setAttribute("successful", Boolean.toString(success));
					el.setAttribute("measureType", "Region Utilization");
				}

			}

		}

	}

	public File writeAllMeasures() {
		for (Measure element : measureList) {
			//writes all the measures in a Document
			writeMeasure(element);
		}

		//writeMeasures_regionDroppedJobs();
		//writeMeasures_regionUtilization();

		//writeMeasures_queueDroppedJobs();

		/////////////////
		//Output the XML
		try {
			//set up a transformer
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer trans = transfac.newTransformer();
			trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
			trans.setOutputProperty(OutputKeys.INDENT, "yes");

			//we want to save the xml results in the same directory of the model definition file
			String parent = null;
			String xmlResultsName = null;

			if (mvaModelDefinition != null) {
				//mvaModelDefinition defined
				if (mvaModelDefinition.isAbsolute()) {
					//the passed filename is absolute
					parent = mvaModelDefinition.getParent();
					if (parent.endsWith(SEPARATOR)) {
						xmlResultsName = parent + "res_sim_" + mvaModelDefinition.getName();
					} else {
						xmlResultsName = parent + SEPARATOR + "res_sim_" + mvaModelDefinition.getName();
					}

				} else {
					//the passed filename is not absolute
					xmlResultsName = SEPARATOR + "res_sim_" + mvaModelDefinition.getName();
				}
			} else {
				//mvaModelDefinition not defined

				//use sim model path
				String simModelPath = sim.getXmlSimModelDefPath();
				File simModelFile;

				if (simModelPath != null) {
					//sim model file exist
					simModelFile = new File(simModelPath);
					xmlResultsName = simModelFile.getParent() + SEPARATOR + "res_sim_" + sim.getName() + ".xml";

				} else {
					//get the user dir
					String curDir = System.getProperty("user.dir");
					String name;
					if (sim.getName() == null) {
						name = "";
					} else {
						name = "_" + sim.getName();
					}
					xmlResultsName = curDir + SEPARATOR + "res_sim_JMT" + name + ".xml";
				}

			}

			//creates the results file
			resultsFile = new File(xmlResultsName);
			if (DEBUG) {
				System.out.println(resultsFile.getAbsolutePath());
			}

			DOMSource source = new DOMSource(doc);

			// Prepare the output file
			File temp = File.createTempFile("~jmt_sim_output", ".xml", resultsFile.getParentFile());
			StreamResult result = new StreamResult(temp);

			// Write the DOM document to the file
			trans.transform(source, result);

			// commit
			if (resultsFile.exists()) {
				resultsFile.delete();
			}

			temp.renameTo(resultsFile);

			// Check because somethime rename fails...
			if (resultsFile.exists()) {
				return resultsFile;
			} else {
				return temp;
			}

		} catch (javax.xml.transform.TransformerConfigurationException exc) {
			exc.printStackTrace();
		} catch (javax.xml.transform.TransformerException exc) {
			exc.printStackTrace();
		} catch (java.io.IOException exc) {
			exc.printStackTrace();
		} catch (NullPointerException exc) {
			exc.printStackTrace();
		}
		return null;
	}

	private double findQueueMeasure(String className, String stationName) {

		try {

			org.w3c.dom.NodeList measureList = root.getElementsByTagName("measure");
			int measureNumber = measureList.getLength();
			Element currentMeasure;

			for (int m = 0; m < measureNumber; m++) {
				currentMeasure = (Element) measureList.item(m);

				if (!currentMeasure.getAttribute("measureType").equalsIgnoreCase("Queue Length")//backward compatibility condition
						&& !currentMeasure.getAttribute("measureType").equalsIgnoreCase("Customer Number")//backward compatibility condition
						&& !currentMeasure.getAttribute("measureType").equalsIgnoreCase("Number of Customers")) {//Present condition
					continue;
				}

				if (!currentMeasure.getAttribute("station").equalsIgnoreCase(stationName)) {
					continue;
				}

				if (!currentMeasure.getAttribute("class").equalsIgnoreCase(className)) {
					continue;
				}

				String success = currentMeasure.getAttribute("successful");

				if (success.equalsIgnoreCase("true")) {
					String measureValue = currentMeasure.getAttribute("meanValue");
					logger.debug("Q measure found for " + stationName + "-" + className + ": " + measureValue);
					return Double.parseDouble(measureValue);
				} else {
					logger.debug("Q measure found for " + stationName + "-" + className + ": NOT SUCCESSFUL");
					return -1;
				}

			}

			//else measure not found
			logger.debug("Q measure not found for " + stationName + "-" + className);
			return -1;

		} catch (Exception e) {
			e.printStackTrace();
			logger.debug("Q measure not found for " + stationName + "-" + className);
			return -1;
		}

	}

}
