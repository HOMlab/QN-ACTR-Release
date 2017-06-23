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

package jmt.engine.dataAnalysis.measureOutputs;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Writer;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.engine.dataAnalysis.MeasureOutput;

import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

/**
 * @author Stefano
 * @version 6-dic-2004 18.13.39
 */
public class XMLMeasureOutput extends MeasureOutput {

	private Document document = null;
	private Writer fileWriter = null;
	private File file = null;
	private DOMParser parser;

	private String name;
	private jmt.engine.dataAnalysis.Measure measure = null;
	private boolean append;

	private Element root = null;
	private Element report = null;
	private Element samples = null;

	protected static final String VALIDATION_FEATURE_ID = "http://xml.org/sax/features/validation";
	private static final String JAXP_SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
	protected static final String SCHEMA_VALIDATION_FEATURE_ID = "http://apache.org/xml/features/validation/schema";
	protected static final String VALIDATION_DYNAMIC_FEATURE_ID = "http://apache.org/xml/features/validation/dynamic";
	private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

	/** Schema validation feature id (http://apache.org/xml/features/validation/schema). */
	//protected static final String SCHEMA_VALIDATION_FEATURE_ID = "http://apache.org/xml/features/validation/schema";
	/** Include ignorable whitespace feature ("dom/include-ignorable-whitespace"). */
	//public static final String INCLUDE_IGNORABLE_WHITESPACE = "http://apache.org/xml/features/dom/include-ignorable-whitespace";
	/** Namespaces feature id (http://xml.org/sax/features/namespaces). */
	protected static final String NAMESPACES_FEATURE_ID = "http://xml.org/sax/features/namespaces";
	/** Schema full checking feature id (http://apache.org/xml/features/validation/schema-full-checking). */
	//protected static final String SCHEMA_FULL_CHECKING_FEATURE_ID = "http://apache.org/xml/features/validation/schema-full-checking";
	public static final String EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID = "http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation";

	public XMLMeasureOutput(jmt.engine.dataAnalysis.Measure measure, boolean append, String fileName) throws IOException {
		super(measure);

		this.measure = measure;
		name = fileName;
		this.append = append;

		file = new File("D://" + fileName + ".xml");

		createDOM();

	}

	/**
	 * Creates a DOM (Document Object Model) <code>Document<code> for XMLMeasureOutput.
	 */
	private void createDOM() {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();

			//data is a Document
			document = builder.newDocument();

			root = document.createElement("measureOutput");

			root.setAttribute("name", name);
			root.setAttribute("precision", Double.toString(measure.getAnalyzer().getPrecision()));
			root.setAttribute("alfa", Double.toString(measure.getAnalyzer().getAlfa()));
			root.setAttribute("maxSamples", Integer.toString(measure.getMaxSamples()));

			report = document.createElement("report");
			report.setAttribute("meanValue", "null");
			report.setAttribute("upperBound", "null");
			report.setAttribute("lowerBound", "null");
			report.setAttribute("finished", "false");
			report.setAttribute("successful", "false");
			report.setAttribute("isZero", "false");
			report.setAttribute("analyzedSamples", "0");
			report.setAttribute("discardedSamples", "0");

			root.appendChild(report);

			samples = document.createElement("samples");

		} catch (FactoryConfigurationError factoryConfigurationError) {
			factoryConfigurationError.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}

	}

	/**
	 * This method is called for every sample.
	 */
	@Override
	public void write(double Sample, double Weight) {

		Element sample = document.createElement("sample");
		sample.setAttribute("sample", Double.toString(Sample));
		sample.setAttribute("weight", Double.toString(Weight));

		samples.appendChild(sample);

	}

	/**
	 * This method is called at the end of a measure.
	 */
	@Override
	public void writeMeasure() {
		if (measure.hasFinished()) {
			//general info
			report.setAttribute("finished", "true");
			boolean success = measure.getSuccess();
			report.setAttribute("successful", Boolean.toString(success));
			report.setAttribute("analyzedSamples", Integer.toString(measure.getAnalyzedSamples()));
			report.setAttribute("discardedSamples", Integer.toString(measure.getDiscardedSamples()));

			if (success) {
				//measure was successful
				boolean isZero = measure.getAnalyzer().isZero();
				if (isZero) {
					//measure is zero
					report.setAttribute("isZero", Boolean.toString(isZero));
					report.setAttribute("meanValue", "0.0");
					report.setAttribute("upperBound", "0.0");
					report.setAttribute("lowerBound", "0.0");
				} else {
					//not zero
					report.setAttribute("isZero", Boolean.toString(isZero));
					report.setAttribute("meanValue", Double.toString(measure.getMeanValue()));
					report.setAttribute("upperBound", Double.toString(measure.getUpperLimit()));
					report.setAttribute("lowerBound", Double.toString(measure.getLowerLimit()));
				}

			} else {
				//measure was not successful
				report.setAttribute("meanValue", Double.toString(measure.getExtimatedMeanValue()));
			}
		}
		//if not finished, do nothing

		//at the end save file
		saveXML();
	}

	public void saveXML() {

		//close file
		try {
			Source source = new DOMSource(document);

			// Prepare the output file
			File temp = File.createTempFile("~jmt_measure", ".xml", file.getParentFile());
			Result result = new StreamResult(temp);

			// Write the DOM document to the file
			Transformer xformer = TransformerFactory.newInstance().newTransformer();
			xformer.transform(source, result);

			/* validate the xml stream */
			FileReader fr = new FileReader(temp);
			InputSource in_source = new InputSource(fr);
			//TODO: il parser va prima creato!!
			parser.parse(in_source);

			/* commit */
			if (file.exists()) {
				file.delete();
			}
			temp.renameTo(file);

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("error in closing file..");
		}

	}

}
