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

import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.engine.QueueNet.SimConstants;
import jmt.gui.common.definitions.MeasureDefinition;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.StoredResultsModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * <p>Title: XML Results Reader</p>
 * <p>Description: Reads stored results information from an XML file. This class is designed
 * to work both with <code>XMLResultsWriter</code> and with engine's generated output files.
 * Obviously the first one will provide much more informations.</p>
 * 
 * @author Bertoli Marco
 *         Date: 3-ott-2005
 *         Time: 12.27.00
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
public class XMLResultsReader implements XMLResultsConstants {

	/**
	 * Loads stored GUI results into specified StoredResultsModel
	 * @param archiveName name of the archive to be opened
	 * @param model CommonModel where all loaded informations should be stored
	 */
	public static void loadGuiModel(StoredResultsModel model, File archiveName) {
		Document doc = XMLReader.loadXML(archiveName.getAbsolutePath(), XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_GUI_RESULTS));
		parseXML(doc, model);
	}

	/**
	 * Loads stored engine results into specified StoredResultsModel
	 * @param archiveName name of the archive to be opened
	 * @param model CommonModel where all loaded informations should be stored
	 */
	public static void loadModel(StoredResultsModel model, File archiveName) {
		Document doc = XMLReader.loadXML(archiveName.getAbsolutePath(), XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_RESULTS));
		parseXML(doc, model);
	}

	/**
	 * Parses given <code>XMLResultsWriter</code> or engine's generated output XML Document.
	 * @param xml Document to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Document xml, MeasureDefinition model) {
		parseXML(xml.getDocumentElement(), model);
	}

	/**
	 * Parses given <code>XMLResultsWriter</code> or engine's generated output XML Document.
	 * @param root xml document root to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Element root, MeasureDefinition model) {
		if (model instanceof PAResultsModel) {
			if (root.getNodeName().equals(XML_DOCUMENT_ROOT)) {
				setMeasures(root, (PAResultsModel) model);
			}
		} else {
			if (root.getNodeName().equals(XML_DOCUMENT_ROOT)) {
				// XMLResultsWriter output file
				setMeasures(root, (StoredResultsModel) model);
			} else if (root.getNodeName().equals(XML_DOCUMENT_O_ROOT)) {
				// Engine's output file
				setEngineMeasures(root, (StoredResultsModel) model);
			}
		}
	}

	/**
	 * Loads measures data from saved XML document. Used only for parametric
	 * analysis results.
	 * @param root root element of xml document
	 * @param model data structure
	 */
	private static void setMeasures(Element root, PAResultsModel model) {
		NodeList measures = root.getElementsByTagName(XML_E_MEASURE);
		for (int i = 0; i < measures.getLength(); i++) {
			Element current = (Element) measures.item(i);
			String measureName = current.getAttribute(XML_A_MEASURE_NAME);
			// Adds its samples
			NodeList samples = current.getElementsByTagName(XML_E_SAMPLE);
			model.addMeasure(measureName, current.getAttribute(XML_A_MEASURE_STATION), current.getAttribute(XML_A_MEASURE_CLASS), Double
					.parseDouble(current.getAttribute(XML_A_MEASURE_ALPHA)), Double.parseDouble(current.getAttribute(XML_A_MEASURE_PRECISION)),
					Integer.parseInt(current.getAttribute(XML_A_MEASURE_TYPE)), current.getAttribute(XML_A_MEASURE_NODETYPE));
			for (int j = 0; j < samples.getLength(); j++) {
				Element sample = (Element) samples.item(j);
				double mean = 0;
				double upper = 0;
				double lower = 0;
				boolean valid;
				valid = Boolean.valueOf(sample.getAttribute(XML_A_SAMPLE_VALIDITY)).booleanValue();
				mean = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_MEAN));
				// If the sample was calculated with the requested precision simply parse
				if (valid) {
					upper = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_UPPERBOUND));
					lower = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_LOWERBOUND));
				}
				//else check or fields equal to the String "Infinity"
				else {
					String u = sample.getAttribute(XML_A_SAMPLE_UPPERBOUND);
					if (u.equals("Infinity")) {
						upper = Double.POSITIVE_INFINITY;
					} else {
						upper = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_UPPERBOUND));
					}
					String l = sample.getAttribute(XML_A_SAMPLE_LOWERBOUND);
					if (l.equals("Infinity")) {
						lower = Double.POSITIVE_INFINITY;
					} else {
						lower = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_LOWERBOUND));
					}
				}
				model.addSample(measureName, lower, mean, upper, valid);
			}
		}
	}

	/**
	 * Loads measures data from saved XML document
	 * @param root root element of xml document
	 * @param model data structure
	 */
	private static void setMeasures(Element root, StoredResultsModel model) {
		double polling = Double.parseDouble(root.getAttribute(XML_A_ROOT_POLLING));
		model.setPollingInterval(polling);
		NodeList measures = root.getElementsByTagName(XML_E_MEASURE);
		for (int i = 0; i < measures.getLength(); i++) {
			Element current = (Element) measures.item(i);
			String measureName = current.getAttribute(XML_A_MEASURE_NAME);
			// Add measure
			model.addMeasure(measureName, current.getAttribute(XML_A_MEASURE_STATION), current.getAttribute(XML_A_MEASURE_CLASS), Double
					.parseDouble(current.getAttribute(XML_A_MEASURE_ALPHA)), Double.parseDouble(current.getAttribute(XML_A_MEASURE_PRECISION)),
					Integer.parseInt(current.getAttribute(XML_A_MEASURE_SAMPLES)), Integer.parseInt(current.getAttribute(XML_A_MEASURE_STATE)),
					Integer.parseInt(current.getAttribute(XML_A_MEASURE_TYPE)), current.getAttribute(XML_A_MEASURE_NODETYPE));
			// Adds its samples
			NodeList samples = current.getElementsByTagName(XML_E_SAMPLE);
			for (int j = 0; j < samples.getLength(); j++) {
				Element sample = (Element) samples.item(j);
				double mean = 0;
				double upper = 0;
				double lower = 0;
				mean = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_MEAN));
				// Gets upperBound if specified
				if (sample.getAttribute(XML_A_SAMPLE_UPPERBOUND) != null && sample.getAttribute(XML_A_SAMPLE_UPPERBOUND) != "") {
					upper = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_UPPERBOUND));
				}
				// Gets lowerBound if specified
				if (sample.getAttribute(XML_A_SAMPLE_LOWERBOUND) != null && sample.getAttribute(XML_A_SAMPLE_LOWERBOUND) != "") {
					lower = Double.parseDouble(sample.getAttribute(XML_A_SAMPLE_LOWERBOUND));
				}
				// Adds sample
				model.addMeasureSample(measureName, mean, upper, lower);
			}
		}
	}

	/**
	 * Loads measures data from XML document generated by engine
	 * @param root root element of xml document
	 * @param model data structure
	 */
	private static void setEngineMeasures(Element root, StoredResultsModel model) {
		NodeList measures = root.getElementsByTagName(XML_EO_MEASURE);
		for (int i = 0; i < measures.getLength(); i++) {
			Element current = (Element) measures.item(i);
			// Required elements
			String stationName = current.getAttribute(XML_AO_MEASURE_STATION);
			String className = current.getAttribute(XML_AO_MEASURE_CLASS);
			String type = current.getAttribute(XML_AO_MEASURE_TYPE);
			String success = current.getAttribute(XML_AO_MEASURE_SUCCESFUL);
			boolean successful = success.toLowerCase().equals("true");
			// Optional Elements
			String attr;
			attr = current.getAttribute(XML_AO_MEASURE_MEAN);
			double mean = 0;
			if (attr != null && attr != "") {
				mean = Double.parseDouble(attr);
			}
			String nodeType = current.getAttribute(XML_AO_MEASURE_NODETYPE);
			attr = current.getAttribute(XML_AO_MEASURE_UPPER);
			double upper = 0;
			if (attr != null && attr != "") {
				upper = Double.parseDouble(attr);
			}
			attr = current.getAttribute(XML_AO_MEASURE_LOWER);
			double lower = 0;
			if (attr != null && attr != "") {
				lower = Double.parseDouble(attr);
			}
			attr = current.getAttribute(XML_AO_MEASURE_SAMPLES);
			int samples = 0;
			if (attr != null && attr != "") {
				samples = Integer.parseInt(attr);
			}
			attr = current.getAttribute(XML_AO_MEASURE_ALPHA);
			double alpha = 0;
			// Inverts alpha
			if (attr != null && attr != "") {
				alpha = 1 - Double.parseDouble(attr);
			}
			attr = current.getAttribute(XML_AO_MEASURE_PRECISION);
			double precision = 0;
			if (attr != null && attr != "") {
				precision = Double.parseDouble(attr);
			}

			// Gets measure state
			int state;
			if (samples == 0) {
				state = MeasureDefinition.MEASURE_NO_SAMPLES;
			} else if (successful) {
				state = MeasureDefinition.MEASURE_SUCCESS;
			} else {
				state = MeasureDefinition.MEASURE_FAILED;
			}

			// Creates unique measure name
			String Name = stationName + "_" + className + "_" + type;

			// Decodes measure type
			String tmp = type.toLowerCase();
			int numType = 0;
			if ((tmp.startsWith("customer") && tmp.endsWith("number") && !"".equalsIgnoreCase(stationName))//condition is for backward compatibility
					|| (tmp.startsWith("queue") && tmp.endsWith("length")) //OR condition is for backward compatibility
					|| (tmp.startsWith("number") && tmp.endsWith("customers"))) {//The present condition
				numType = SimConstants.QUEUE_LENGTH;
			} else if (tmp.startsWith("utilization")) {
				numType = SimConstants.UTILIZATION;
			} else if (tmp.startsWith("throughput")) {
				numType = SimConstants.THROUGHPUT;
			} else if (tmp.startsWith("response") && tmp.endsWith("time")) {
				numType = SimConstants.RESPONSE_TIME;
			} else if (tmp.startsWith("residence") && tmp.endsWith("time")) {
				numType = SimConstants.RESIDENCE_TIME;
			} else if (tmp.startsWith("queue") && tmp.endsWith("time")) {
				numType = SimConstants.QUEUE_TIME;
			} else if (tmp.startsWith("system") && tmp.endsWith("time")) {
				numType = SimConstants.SYSTEM_RESPONSE_TIME;
			} else if (tmp.startsWith("system") && tmp.endsWith("throughput")) {
				numType = SimConstants.SYSTEM_THROUGHPUT;
			} else if ((tmp.startsWith("customer") && tmp.endsWith("number") && "".equalsIgnoreCase(stationName)) //backward comp. condition
					|| (tmp.startsWith("system") && tmp.endsWith("number")) //backward comp. cond. for Label change: customer Number to System Customer Number
					|| (tmp.startsWith("system") && tmp.endsWith("customers"))) {//present cond.
				numType = SimConstants.SYSTEM_JOB_NUMBER;
			} else if (tmp.startsWith("drop") && tmp.endsWith("rate")) {
				numType = SimConstants.DROP_RATE;
			} else if (tmp.startsWith("system") && tmp.endsWith("rate")) {
				numType = SimConstants.SYSTEM_DROP_RATE;
			}
			//Added by ASHANAK START
			//Added as a part of the System Power addition to the Performance index of JSIM
			else if (tmp.startsWith("system") && tmp.endsWith("power")) {
				numType = SimConstants.SYSTEM_POWER;
			}
			//Added by ASHANKA STOP
			else if (tmp.startsWith("Throughput") && tmp.endsWith("Sink")) {
				numType = SimConstants.THROUGHPUT_PER_SINK;
			}
			else if (tmp.startsWith("Response") && tmp.endsWith("Sink")) {
				numType = SimConstants.RESPONSE_TIME_PER_SINK;
			}
			// Adds loaded informations into model data structure
			model.addMeasure(Name, stationName, className, alpha, precision, samples, state, numType, nodeType);
			model.addMeasureSample(Name, mean, upper, lower);
		}
	}
}
