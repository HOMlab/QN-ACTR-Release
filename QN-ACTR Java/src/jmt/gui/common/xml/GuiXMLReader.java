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

import java.awt.Color;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.JOptionPane;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;
import jmt.gui.jmodel.definitions.JMODELModel;
import jmt.gui.jmodel.definitions.JMTPoint;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * <p>Title: Gui XML Reader</p>
 * <p>Description: Reads JModel GUI specific information from an XML file. This
 * class provide methods for model load. This class is designed to be called
 * only once simulation's specific informations have already be inserted.</p>
 *
 * @author Bertoli Marco
 *         Date: 27-lug-2005
 *         Time: 11.04.13
 */
public class GuiXMLReader implements GuiXMLConstants {
	/**
	 * Parses given Gui XML Document to reconstruct all gui related stuff. This method
	 * is designed to be called ONLY when all simulation's data has already been put into
	 * data structure (as will never create new user classes or stations)
	 * @param xml Document to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Document xml, CommonModel model) {
		parseXML(xml.getDocumentElement(), model);
	}

	/**
	 * Parses given Gui XML Document to reconstruct all gui related stuff. This method
	 * is designed to be called ONLY when all simulation's data has already been put into
	 * data structure (as will never create new user classes or stations)
	 * @param root root element of Document to be parsed
	 * @param model data model to be elaborated
	 */
	public static void parseXML(Element root, CommonModel model) {
		if (model instanceof JMODELModel) {
			setUserClasses(root, (JMODELModel) model);
			setStations(root, (JMODELModel) model);
		}
		setParametricAnalysis(root, model);
	}

	/**
	 * Sets gui specific user class parameters
	 * @param root GuiXML document root
	 * @param model data structure where properties have to be set
	 */
	protected static void setUserClasses(Element root, JMODELModel model) {
		NodeList classes = root.getElementsByTagName(XML_E_CLASS);
		// Build a map of class name -> class key
		HashMap names = new HashMap();
		Vector keys = model.getClassKeys();
		for (int i = 0; i < keys.size(); i++) {
			names.put(model.getClassName(keys.get(i)), keys.get(i));
		}

		Element userclass;
		String name, str_color;
		Color color;
		// For each stored class
		for (int i = 0; i < classes.getLength(); i++) {
			userclass = (Element) classes.item(i);
			name = userclass.getAttribute(XML_A_CLASS_NAME);
			str_color = userclass.getAttribute(XML_A_CLASS_COLOR);
			// Parses color string (# + RGB rappresentation in HEX format)
			if (str_color.startsWith("#")) {
				str_color = str_color.substring(1, str_color.length());
			} else {
				System.out.println("Warning - Bad Color format for class '" + name + "'.");
			}
			try {
				color = new Color((int) Long.parseLong(str_color, 16));
			} catch (NumberFormatException ex) {
				System.out.println("Warning - Bad Color format for class '" + name + "'. " + "Assigning a default one");
				color = model.getNewColor();
				ex.printStackTrace();
			}
			// Sets color for current class only if exists in current model
			if (names.containsKey(name)) {
				model.setClassColor(names.get(name), color);
			} else {
				System.out.println("Warning - Found color info for class '" + name + "' which is not present into current model.");
			}
		}
	}

	/**
	 * Sets gui specific station parameters
	 * @param root GuiXML document root
	 * @param model data structure where properties have to be set
	 */
	protected static void setStations(Element root, JMODELModel model) {
		NodeList stations = root.getElementsByTagName(XML_E_STATION);
		// Build a map of station name -> station key
		HashMap names = new HashMap();
		Vector keys = model.getStationKeys();
		for (int i = 0; i < keys.size(); i++) {
			names.put(model.getStationName(keys.get(i)), keys.get(i));
		}

		Element station, position;
		String name;
		double x, y;
		boolean rotate = false;
		// For each stored Station
		for (int i = 0; i < stations.getLength(); i++) {
			station = (Element) stations.item(i);
			name = station.getAttribute(XML_A_STATION_NAME);
			position = (Element) station.getElementsByTagName(XML_E_POSITION).item(0);
			x = Double.parseDouble(position.getAttribute(XML_A_POSITION_X));
			y = Double.parseDouble(position.getAttribute(XML_A_POSITION_Y));
			if (position.hasAttribute(XML_A_POSITION_ROTATE)) {
				rotate = Boolean.TRUE.toString().equalsIgnoreCase(position.getAttribute(XML_A_POSITION_ROTATE));
			}
			if (names.containsKey(name)) {
				model.setStationPosition(names.get(name), new JMTPoint(x, y, rotate));
			} else {
				System.out.println("Error - Found position info for station '" + name + "' which is not present into current model.");
			}
		}
	}

	protected static void setParametricAnalysis(Element root, CommonModel model) {
		NodeList temp = root.getElementsByTagName(XML_E_PARAMETRIC);
		if (temp.getLength() != 0) {
			Element parametric = (Element) temp.item(0);
			String enabled = parametric.getAttribute(XML_A_PARAMETRIC_ENABLED);
			if (Boolean.valueOf(enabled).booleanValue()) {
				model.setParametricAnalysisEnabled(true);
				String classPath = parametric.getAttribute(XML_A_PARAMETRIC_CLASSPATH);
				try {
					Class[] paramClasses = { ClassDefinition.class, StationDefinition.class, SimulationDefinition.class };
					Object[] params = { model, model, model };
					ParametricAnalysisDefinition pad = (ParametricAnalysisDefinition) Class.forName(classPath).getConstructor(paramClasses)
							.newInstance(params);
					NodeList fields = parametric.getElementsByTagName(XML_E_FIELD);
					for (int i = 0; i < fields.getLength(); i++) {
						Element field = (Element) fields.item(i);
						String fieldName = field.getAttribute(XML_A_FIELD_NAME);
						String fieldValue = field.getAttribute(XML_A_FIELD_VALUE);
						pad.setProperty(fieldName, fieldValue);
					}
					pad.searchForAvaibleSteps();
					pad.createValuesSet();
					model.setParametricAnalysisModel(pad);
				} catch (InvocationTargetException ite) {
					JOptionPane.showMessageDialog(null, "Invocation target exception: " + classPath);
				} catch (NoSuchMethodException nsme) {
					JOptionPane.showMessageDialog(null, "No such method: " + classPath);
				} catch (ClassNotFoundException cnfe) {
					JOptionPane.showMessageDialog(null, "Class not found: " + classPath);
				} catch (InstantiationException ie) {
					JOptionPane.showMessageDialog(null, "Instantiation exception: " + classPath);
				} catch (IllegalAccessException iae) {
					JOptionPane.showMessageDialog(null, "Illegal acces exception: " + classPath);
				}
			}
		}
	}
}
