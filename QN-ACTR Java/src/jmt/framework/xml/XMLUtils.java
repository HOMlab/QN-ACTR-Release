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

package jmt.framework.xml;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.common.xml.resources.XSDSchemaLoader;

import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * @author alyf (Andrea Conti) (Date: 18-set-2003)
 *
 * Fixed by Bertoli Marco
 */

/**
 * XML loading/saving of exact models
 */
public class XMLUtils {

	private static final boolean DEBUG = false;

	private DOMParser parser;
	//private XMLSerializer serializer;
	private boolean success;

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
	//NEW
	//public static final String EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID = "http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation";
	public static final String EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID = "http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation";

	//end NEW

	public XMLUtils() {

		try {
			// Bertoli Marco - load schema inside JAR
			String externalSchemaLocation = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION);
			//String externalSchemaLocation = "xml//JMTmodel.xsd";

			parser = new DOMParser();
			parser.setFeature(NAMESPACES_FEATURE_ID, true);
			parser.setFeature(VALIDATION_FEATURE_ID, true);
			parser.setFeature(VALIDATION_DYNAMIC_FEATURE_ID, true);

			//parser.setFeature(SCHEMA_VALIDATION_FEATURE_ID, false);
			//parser.setFeature(SCHEMA_FULL_CHECKING_FEATURE_ID, true);
			//parser.setProperty(EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID, externalSchemaLocation);
			//parser.setFeature(INCLUDE_IGNORABLE_WHITESPACE, false);

			//NEW
			//TODO:
			parser.setProperty(EXTERNAL_SCHEMA_LOCATION_PROPERTY_ID, externalSchemaLocation);
			//end NEW

			//parser.setProperty(JAXP_SCHEMA_LANGUAGE,W3C_XML_SCHEMA);
			parser.setErrorHandler(new ErrorCatcher());

		} catch (Exception e) {
			throw new RuntimeException(e);
		}

	}

	public boolean saveXML(Document document, File file) throws IOException, SAXException, TransformerFactoryConfigurationError, TransformerException {
		success = true;

		// Prepare the DOM document for writing
		Source source = new DOMSource(document);

		// Prepare the output file
		//OLD
		/*
		File temp = File.createTempFile("~jmt_exact", ".xml", file.getParentFile());
		Result result = new StreamResult(temp);
		*/
		//NEW
		Result result = new StreamResult(file);
		//END

		//System.out.println("temp file received is named: "+file);
		//System.out.println("temp file created is named: "+file);

		// Write the DOM document to the file
		Transformer xformer = TransformerFactory.newInstance().newTransformer();
		xformer.transform(source, result);

		/* validate the xml stream */
		//OLD
		/*
		FileReader fr = new FileReader(temp);
		*/
		//NEW
		FileReader fr = new FileReader(file);
		//END

		InputSource in_source = new InputSource(fr);
		parser.parse(in_source);

		/* commit */
		//OLD
		/*
		if (file.exists()) file.delete();
		temp.renameTo(file);
		*/
		//END
		if (DEBUG) {
			System.out.println("finally temp file created is named: " + file);
		}

		return success;

	}

	/**
	 * @return the loaded Document of null if there were errors
	 */
	public Document loadXML(File f) throws SAXException, IOException {
		success = true;
		FileReader fr = new FileReader(f);
		parser.parse(new InputSource(fr));
		Document d = parser.getDocument();
		if (!success) {
			return null;
		}
		//d.normalize();
		return d;
	}

	public class ErrorCatcher implements ErrorHandler {
		public void error(SAXParseException exception) throws SAXException {
			System.err.println("Error while parsing: " + exception.getMessage());
			throw exception;
		}

		public void fatalError(SAXParseException exception) throws SAXException {
			System.err.println("Fatal error while parsing: " + exception.getMessage());
			throw exception;
		}

		public void warning(SAXParseException exception) throws SAXException {
			System.err.println("Warning: " + exception.getMessage());
			success = false;
		}
	}

}
