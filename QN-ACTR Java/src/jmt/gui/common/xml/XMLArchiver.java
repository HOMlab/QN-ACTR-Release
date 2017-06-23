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
import java.util.Date;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.StoredResultsModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * <p>Title: XML Manager</p>
 * <p>Description: Used to save and load model information to be used for the simulation and
 * for the save/load capability.</p>
 *
 * @author Bertoli Marco
 *         Date: 22-lug-2005
 *         Time: 13.47.51
 */
public class XMLArchiver implements GuiXMLConstants {
	/**
	 * Saves JMT Archive storing informations for simulator and for the gui
	 * @param archiveName Handler to the archive to be created
	 * @param model data structure
	 */
	public static void saveModel(File archiveName, CommonModel model) {
		Document modelDoc = getDocument(model, archiveName.getName());
		if (modelDoc == null) {
			return;
		}
		try {
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty("indent", "yes");
			transformer.setOutputProperty("encoding", ENCODING);
			transformer.transform(new DOMSource(modelDoc), new StreamResult(archiveName));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerFactoryConfigurationError transformerFactoryConfigurationError) {
			transformerFactoryConfigurationError.printStackTrace(); //To change body of catch statement use Options | File Templates.
		} catch (TransformerException e) {
			e.printStackTrace(); //To change body of catch statement use Options | File Templates.
		}
	}

	/**
	 * Loads an archive into specified CommonModel. If CommonModel is a JMODELModel,
	 * loads gui informations too.
	 * @param archiveName name of the archive to be opened
	 * @param model CommonModel where all loaded informations should be stored
	 *
	 * Modified by Francesco D'Aquino
	 */
	public static void loadModel(CommonModel model, File archiveName) {
		Document doc = XMLReader.loadXML(archiveName.getAbsolutePath(), XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMT_ARCHIVE));
		// If document is simulation file, loads it only, if it's an archive loads everything
		if (doc.getElementsByTagName(XML_ARCHIVE_DOCUMENT_ROOT).getLength() == 0) {
			XMLReader.parseXML(doc, model);
		} else {
			XMLReader.parseXML(getSimFromArchiveDocument(doc), model);
			if (containsGUI(doc)) {
				GuiXMLReader.parseXML(getGuiFromArchiveDocument(doc), model);
			}
			if (containsResults(doc)) {
				if (model.isParametricAnalysisEnabled()) {
					PAResultsModel results = new PAResultsModel(model, true);
					XMLResultsReader.parseXML(getResultsFromArchiveDocument(doc), results);
					model.setSimulationResults(results);
				} else {
					StoredResultsModel results = new StoredResultsModel();
					XMLResultsReader.parseXML(getResultsFromArchiveDocument(doc), results);
					model.setSimulationResults(results);
				}
			}
		}
	}

	/**
	 * Returns a new Archive XML Document used to store simulation and GUI data
	 * @param model data structure
	 * @param documentName name of created document (to be setted into root element)
	 * @return created Document
	 */
	public static Document getDocument(CommonModel model, String documentName) {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = null;
		try {
			docBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			return null;
		}
		Document archiveDoc = docBuilder.newDocument();
		// Retrives documents from XMLWriter and GuiXMLWriter
		Document sim = XMLWriter.getDocument(model, documentName);
		Document jmodel;
		Document results = null;
		// Stores gui information
		jmodel = GuiXMLWriter.getDocument(model);
		// If results are present, store them. Do not store results if parametric
		// analysis is enabled but results are from normal simulation, or if parametric
		// is not enabled but results come from a parametric simulation
		if (model.containsSimulationResults()) {
			if (model.isParametricAnalysisEnabled() && (model.getSimulationResults() instanceof PAResultsModel)) {
				results = XMLResultsWriter.getDocument(model.getSimulationResults());
			} else if (!model.isParametricAnalysisEnabled() && (!(model.getSimulationResults() instanceof PAResultsModel))) {
				results = XMLResultsWriter.getDocument(model.getSimulationResults());
			}
		}

		Element root = archiveDoc.createElement(XML_ARCHIVE_DOCUMENT_ROOT);
		archiveDoc.appendChild(root);

		// Adds sim and jmodel root elements to Archive root
		root.appendChild(archiveDoc.importNode(sim.getElementsByTagName(XMLConstantNames.XML_DOCUMENT_ROOT).item(0), true));
		if (jmodel != null) {
			root.appendChild(archiveDoc.importNode(jmodel.getElementsByTagName(XML_DOCUMENT_ROOT).item(0), true));
		}
		// Adds simulation results idf present
		if (results != null) {
			root.appendChild(archiveDoc.importNode(results.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_ROOT).item(0), true));
		}

		// Adds root attributes
		root.setAttribute("xsi:noNamespaceSchemaLocation", XML_ARCHIVE_DOCUMENT_XSD);
		root.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		root.setAttribute(XML_ARCHIVE_A_NAME, documentName);
		root.setAttribute(XML_ARCHIVE_A_TIMESTAMP, new Date().toString());

		return archiveDoc;
	}

	/**
	 * Returns a simulation data Element, given an input archive in Document format.
	 * @param archive Document of an archive
	 * @return Element of corresponding simulation data
	 */
	public static Element getSimFromArchiveDocument(Document archive) {
		return ((Element) archive.getElementsByTagName(XMLConstantNames.XML_DOCUMENT_ROOT).item(0));
	}

	/**
	 * Returns a Gui information data Element, given an input archive in Document format.
	 * @param archive Document of an archive
	 * @return Element of corresponding gui informations
	 */
	public static Element getGuiFromArchiveDocument(Document archive) {
		return ((Element) archive.getElementsByTagName(GuiXMLConstants.XML_DOCUMENT_ROOT).item(0));
	}

	/**
	 * Returns Results information data Element, given an input archive in Document format.
	 * @param archive Document of an archive
	 * @return Element of corresponding results informations
	 */
	public static Element getResultsFromArchiveDocument(Document archive) {
		// Retrives "results" node and returns it
		if (archive.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_ROOT).getLength() > 0) {
			return (Element) archive.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_ROOT).item(0);
		} else {
			return (Element) archive.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_O_ROOT).item(0);
		}
	}

	/**
	 * Tells if specified archive document holds gui informations
	 * @param archive archive document
	 * @return true iff gui informations are found
	 */
	private static boolean containsGUI(Document archive) {
		if (archive.getElementsByTagName(XML_DOCUMENT_ROOT).getLength() > 0) {
			return true;
		}
		return false;
	}

	/**
	 * Tells if specified archive document holds results informations
	 * @param archive archive document
	 * @return true iff results informations are found
	 */
	private static boolean containsResults(Document archive) {
		if (archive.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_ROOT).getLength() > 0) {
			return true;
		}
		if (archive.getElementsByTagName(XMLResultsConstants.XML_DOCUMENT_O_ROOT).getLength() > 0) {
			return true;
		}
		return false;
	}

}
