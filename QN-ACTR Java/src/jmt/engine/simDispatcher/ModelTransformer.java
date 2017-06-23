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

package jmt.engine.simDispatcher;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import jmt.common.xml.resources.XSDSchemaLoader;

import org.apache.xerces.parsers.SAXParser;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Stefano, Bertoli Marco
 * @version 16-dic-2004 14.45.12
 */
public class ModelTransformer {

	private File xmlSource;
	private File xmlDestination;
	//private File xslTransformer;

	private String xsdSource;
	private String xsdDestination;
	private String xsltTransformer;

	private Transformer transf;
	private StreamSource dSource;
	private StreamResult dResult;

	private boolean validate = true;

	public ModelTransformer() {
		//nothing to do??
	}

	private void configureTransformer(String xmlSourcePath, String xmlDestinationPath, String xslTransformerPath, String xsdSourcePath,
			String xsdDestinationPath) {
		try {
			this.xmlSource = new File(xmlSourcePath);
			this.xmlDestination = new File(xmlDestinationPath);
			this.xmlDestination.createNewFile();
			this.xsltTransformer = xslTransformerPath;
			this.xsdSource = xsdSourcePath;
			this.xsdDestination = xsdDestinationPath;
		} catch (IOException e) {
			System.out.println("Error in creating destination file..");
			e.printStackTrace();
		}

	}

	/**
	 * Sets if parser must validate 
	 * @param validate true if parser must validate, false otherwise
	 */
	public void setValidate(boolean validate) {
		this.validate = validate;
	}

	private boolean transform() {

		try {

			//configure parser
			SAXParser parser = new SAXParser();
			parser.setFeature("http://xml.org/sax/features/validation", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);
			parser.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation", xsdSource);
			parser.setErrorHandler(new DefaultHandler());

			//parse source file to validate it
			if (validate) {
				parser.parse(new InputSource(new BufferedInputStream(new FileInputStream(xmlSource))));
			}

			//transform
			dSource = new StreamSource(xmlSource);
			dResult = new StreamResult(xmlDestination);
			transf = TransformerFactory.newInstance().newTransformer(new StreamSource(xsltTransformer));
			transf.transform(dSource, dResult);

			//configure parser
			parser.setFeature("http://xml.org/sax/features/validation", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);
			parser.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation", xsdDestination);
			parser.setErrorHandler(new DefaultHandler());

			//parse destination file to validate it
			if (validate) {
				parser.parse(new InputSource(new BufferedInputStream(new FileInputStream(xmlDestination))));
			}

			//transformation successful
			return true;

		} catch (IOException e) {
			System.out.println("Error in creating destination file..");
			e.printStackTrace();
		} catch (SAXException e) {
			System.out.println("Error with SAX parser..");
			e.printStackTrace();
		} catch (TransformerException e) {
			System.out.println("Error in transforming files..");
			e.printStackTrace();
		}

		//some exception has been thrown
		return false;
	}

	/**
	 * Only for debug
	 *
	 */
	public void printNodes(Node n) {
		if (n == null) {
			return;
		}
		String attrs = "";
		if (n.getAttributes() != null) {
			NamedNodeMap aNodes = n.getAttributes();
			for (int i = 0; i < aNodes.getLength(); i++) {
				Node item = aNodes.item(i);
				attrs = attrs + " " + item.getNodeName() + "=" + item.getNodeValue();
			}
		}
		System.out.println(n.getNodeName() + " = " + n.getNodeValue() + ";\tattrs: " + attrs);
		if (n.getChildNodes() == null) {
			return;
		}
		NodeList nl = n.getChildNodes();
		for (int i = 0; i < nl.getLength(); i++) {
			printNodes(nl.item(i));
		}
	}

	/**
	 * Transform an xml file describing the model, converting it from the
	 *  JMTmodel.xsd schema to the SIMmodeldefinition.xsd schema
	 * @param srcPath the path of the original xml file
	 * @param dstPath the path of the destination xml file
	 */
	public boolean MVAtoSIM_parallel(String srcPath, String dstPath) {

		//source xsd
		String srcXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION);

		//destination xsd
		String dstXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_DEFINITION);

		//xslt transformation
		String transPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_TO_JSIM);

		configureTransformer(srcPath, dstPath, transPath, srcXsdPath, dstXsdPath);
		return transform();

	}

	/**
	 * Transform an xml file containing the simulation results (SIMmodeloutput),
	 * converting it in a file with the model definition and the results following
	 * the JMTmodel.xsd schema. This must be used together with XSLT transformer that changes Residence times
	 * @param srcPath the path of the original xml file
	 * @param dstPath the path of the destination xml file
	 */
	public boolean OUTtoMVAScaling(String srcPath, String dstPath) {

		//source xsd
		String srcXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_RESULTS);

		//destination xsd
		String dstXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION);

		//xslt transformation
		String transPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.OUT_TO_JMVA_SCALING);

		configureTransformer(srcPath, dstPath, transPath, srcXsdPath, dstXsdPath);

		return transform();

	}

	/**
	 * Transform an xml file containing the simulation results (SIMmodeloutput),
	 * converting it in a file with the model definition and the results following
	 * the JMTmodel.xsd schema.
	 * @param srcPath the path of the original xml file
	 * @param dstPath the path of the destination xml file
	 */
	public boolean OUTtoMVA(String srcPath, String dstPath) {

		//source xsd
		String srcXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JSIM_MODEL_RESULTS);

		//destination xsd
		String dstXsdPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION);

		//xslt transformation
		String transPath = XSDSchemaLoader.loadSchema(XSDSchemaLoader.OUT_TO_JMVA);

		configureTransformer(srcPath, dstPath, transPath, srcXsdPath, dstXsdPath);

		return transform();

	}
}
