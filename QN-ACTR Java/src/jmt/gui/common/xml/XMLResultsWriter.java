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
import java.io.OutputStream;
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

import jmt.framework.gui.graph.MeasureValue;
import jmt.gui.common.definitions.MeasureDefinition;
import jmt.gui.common.definitions.PAResultsModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * <p>Title: XML Results Writer</p>
 * <p>Description: Writes calculated measures to an XML file along with temporary values. This
 * class provide methods used to store all simulation results with model definition.</p>
 * 
 * @author Bertoli Marco
 *         Date: 3-ott-2005
 *         Time: 10.36.30
 */
public class XMLResultsWriter implements XMLResultsConstants {
	/**
	 * Writes results into an XML file
	 * @param fileName name of the file to be created
	 * @param measure data structure
	 */
	public static void writeXML(String fileName, MeasureDefinition measure) {
		writeToResult(new StreamResult(new File(fileName)), measure);
	}

	/**
	 * Writes results into an XML file
	 * @param xmlFile Handler to the file to be created
	 * @param measure data structure
	 */
	public static void writeXML(File xmlFile, MeasureDefinition measure) {
		writeToResult(new StreamResult(xmlFile), measure);
	}

	/**
	 * Writes results into an XML stream
	 * @param out stream where XML should be written
	 * @param measure data structure
	 */
	public static void writeXML(OutputStream out, MeasureDefinition measure) {
		writeToResult(new StreamResult(out), measure);
	}

	/**
	 * Helper method used to call transformer to build up an XML file from a Document
	 * @param res Result where created xml should be put
	 * @param measure data structure
	 */
	private static void writeToResult(Result res, MeasureDefinition measure) {
		Document modelDoc = getDocument(measure);
		if (modelDoc == null) {
			return;
		}
		try {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty("indent", "yes");
			transformer.setOutputProperty("encoding", ENCODING);
			transformer.transform(new DOMSource(modelDoc), res);
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError transformerFactoryConfigurationError) {
			transformerFactoryConfigurationError.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Returns the entire Document rapresenting results data structure.
	 * @param measure data structure
	 * @return complete measures data structure in Document format
	 */
	public static Document getDocument(MeasureDefinition measure) {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = null;
		try {
			docBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			return null;
		}
		Document modelDoc = docBuilder.newDocument();
		// Writes all elements on Document
		writeResults(modelDoc, measure);
		return modelDoc;
	}

	/**
	 * Creates root element and adds schema informations to it
	 * @param resultsDoc Document root
	 * @param measure data structure
	 */
	static protected void writeResults(Document resultsDoc, MeasureDefinition measure) {
		Element elem = resultsDoc.createElement(XML_DOCUMENT_ROOT);
		resultsDoc.appendChild(elem);
		elem.setAttribute("xsi:noNamespaceSchemaLocation", XML_DOCUMENT_XSD);
		elem.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		elem.setAttribute(XML_A_ROOT_POLLING, Double.toString(measure.getPollingInterval()));
		writeMeasures(resultsDoc, elem, measure);
	}

	/**
	 * Write measures information
	 * @param doc Document root
	 * @param resultsNode parent node where measure informations should be added
	 * @param measure data structure
	 */
	static protected void writeMeasures(Document doc, Node resultsNode, MeasureDefinition measure) {
		if (measure instanceof PAResultsModel) {
			// Writes every measure
			for (int i = 0; i < measure.getMeasureNumber(); i++) {
				Element elem = doc.createElement(XML_E_MEASURE);
				elem.setAttribute(XML_A_MEASURE_NAME, measure.getName(i));
				elem.setAttribute(XML_A_MEASURE_TYPE, Integer.toString(measure.getMeasureType(i)));
				elem.setAttribute(XML_A_MEASURE_CLASS, measure.getClassName(i));
				elem.setAttribute(XML_A_MEASURE_STATION, measure.getStationName(i));
				elem.setAttribute(XML_A_MEASURE_SAMPLES, Integer.toString(measure.getAnalizedSamples(i)));
				elem.setAttribute(XML_A_MEASURE_ALPHA, Double.toString(measure.getAlpha(i)));
				elem.setAttribute(XML_A_MEASURE_PRECISION, Double.toString(measure.getPrecision(i)));
				elem.setAttribute(XML_A_MEASURE_NODETYPE, measure.getNodeType(i));
				writeSamples(doc, elem, measure, i, true);
				resultsNode.appendChild(elem);
			}
		} else {
			// Writes every measure
			for (int i = 0; i < measure.getMeasureNumber(); i++) {
				Element elem = doc.createElement(XML_E_MEASURE);
				elem.setAttribute(XML_A_MEASURE_NAME, measure.getName(i));
				elem.setAttribute(XML_A_MEASURE_TYPE, Integer.toString(measure.getMeasureType(i)));
				elem.setAttribute(XML_A_MEASURE_CLASS, measure.getClassName(i));
				elem.setAttribute(XML_A_MEASURE_STATION, measure.getStationName(i));
				elem.setAttribute(XML_A_MEASURE_FINALVALUE, Double.toString((measure.getValues(i).lastElement()).getMeanValue()));
				elem.setAttribute(XML_A_MEASURE_SAMPLES, Integer.toString(measure.getAnalizedSamples(i)));
				elem.setAttribute(XML_A_MEASURE_STATE, Integer.toString(measure.getMeasureState(i)));
				elem.setAttribute(XML_A_MEASURE_ALPHA, Double.toString(measure.getAlpha(i)));
				elem.setAttribute(XML_A_MEASURE_PRECISION, Double.toString(measure.getPrecision(i)));
				elem.setAttribute(XML_A_MEASURE_NODETYPE, measure.getNodeType(i));
				writeSamples(doc, elem, measure, i, false);
				resultsNode.appendChild(elem);
			}
		}
	}

	/**
	 * Writes all samples for a given measure
	 *
	 * Modified by Francesco D'Aquino
	 *
	 * @param doc Document root
	 * @param measureNode parent node where samples informations should be added
	 * @param measure data structure
	 * @param measureIndex index of the measure to be written
	 * @param parametric true if these are results from a parametric analysis simulation
	 */
	protected static void writeSamples(Document doc, Node measureNode, MeasureDefinition measure, int measureIndex, boolean parametric) {
		Vector samples = measure.getValues(measureIndex);
		if (parametric) {
			PAResultsModel.MeasureValueImpl value;
			// Writes all samples, ignoring upperBound and lowerBound if zeros or infinites
			// (that means they are unset)
			for (int i = 0; i < samples.size(); i++) {
				Element sample = doc.createElement(XML_E_SAMPLE);
				value = (PAResultsModel.MeasureValueImpl) samples.get(i);
				sample.setAttribute(XML_A_SAMPLE_MEAN, Double.toString(value.getMeanValue()));
				//if (value.getLowerBound() > 0 && !Double.isInfinite(value.getLowerBound()))
				sample.setAttribute(XML_A_SAMPLE_LOWERBOUND, Double.toString(value.getLowerBound()));
				//if (value.getUpperBound() > 0 && !Double.isInfinite(value.getUpperBound()))
				sample.setAttribute(XML_A_SAMPLE_UPPERBOUND, Double.toString(value.getUpperBound()));
				sample.setAttribute(XML_A_SAMPLE_VALIDITY, Boolean.toString(value.isValid()));
				measureNode.appendChild(sample);
			}
		} else {
			MeasureValue value;
			// Writes all samples, ignoring upperBound and lowerBound if zeros or infinites
			// (that means they are unset)
			for (int i = 0; i < samples.size(); i++) {
				Element sample = doc.createElement(XML_E_SAMPLE);
				value = (MeasureValue) samples.get(i);
				sample.setAttribute(XML_A_SAMPLE_MEAN, Double.toString(value.getMeanValue()));
				if (value.getLowerBound() > 0 && !Double.isInfinite(value.getLowerBound())) {
					sample.setAttribute(XML_A_SAMPLE_LOWERBOUND, Double.toString(value.getLowerBound()));
				}
				if (value.getUpperBound() > 0 && !Double.isInfinite(value.getUpperBound())) {
					sample.setAttribute(XML_A_SAMPLE_UPPERBOUND, Double.toString(value.getUpperBound()));
				}
				measureNode.appendChild(sample);
			}
		}
	}

}
