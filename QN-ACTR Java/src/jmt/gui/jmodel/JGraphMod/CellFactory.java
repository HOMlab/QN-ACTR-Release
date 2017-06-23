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

package jmt.gui.jmodel.JGraphMod;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.font.FontRenderContext;
import java.lang.reflect.InvocationTargetException;

import javax.swing.ImageIcon;

import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jmodel.controller.Mediator;
import jmt.gui.jmodel.definitions.JmodelStationDefinition;

/**

 * @author alyf (Andrea Conti)
 * Date: 3-set-2003
 * Time: 11.11.44

 * Entirely modified by Bertoli Marco 3-giu-2005

 */

/**
 * A class grouping all cell creation suff
 */
public class CellFactory {
	private Mediator mediator;

	/**
	 * Instantiate a new CellFactory
	 * @param mediator reference to mediator
	 */
	public CellFactory(Mediator mediator) {
		this.mediator = mediator;
	}

	/**
	 * Creates a new cell of given class with default user object (CellComponent)
	 * @param className Name of the class to be instantiated
	 * @return created JmtCell
	 *
	 * Author: Bertoli Marco
	 */
	public JmtCell createCell(String className) {
		// Creates the internal component
		CellComponent component = createComponent(className);
		return createCell(className, component);
	}

	/**
	 * Creates a new cell for the station with the given search key
	 * @param key search's key (into data structure) for station cell to be created
	 * @return created cell component
	 */
	public JmtCell createStationCell(Object key) {
		CellComponent component = new CellComponent(key, mediator.getStationDefinition());
		return createCell(mediator.getStationDefinition().getStationType(key) + "Cell", component);
	}

	/**
	 * Creates a new cell of given class with given user object
	 * @param className Name of the class to be instantiated
	 * @param userObject object to be inserted into cell's data structure
	 * @return created JmtCell
	 *
	 * Author: Bertoli Marco
	 */
	public JmtCell createCell(String className, Object userObject) {
		String path = "jmt.gui.jmodel.JGraphMod.";

		// Recalls the cell constructor passing the component as user object
		JmtCell cell = null;
		try {
			Class[] parameterTypes = { Object.class };
			Object[] initargs = new Object[1];
			initargs[0] = userObject;
			cell = (JmtCell) Class.forName(path + className).getConstructor(parameterTypes).newInstance(initargs);
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		return cell;
	}

	/**
	 * Creates a new component of a given class.
	 * @param className className of cell component to be created
	 * @return created component
	 *
	 * Author: Bertoli Marco
	 */
	public CellComponent createComponent(String className) {
		JmodelStationDefinition sd = mediator.getStationDefinition();
		return new CellComponent(sd.addStation(getComponentType(className)), sd);
	}

	/**
	 * This class is used as a bridge with new data structure (common with JSIM). It will
	 * returns component type constant to be used on <code>JMODELModel</code> given
	 * *Cell classname.
	 * @param className className of cell component
	 * @return type in the format needed by JMODELModel methods
	 *
	 * Author: Bertoli Marco
	 */
	public String getComponentType(String className) {
		// For convention Component type is simply its className without "Cell"
		return className.substring(0, className.lastIndexOf("Cell"));
		/*
		// Try to get correct values from JMODELConstants through reflection to make simpler
		// deploying of new components. Otherwise a switch was needed here.
		String baseName = className.substring(0, className.lastIndexOf("Cell"));
		String baseType = "STATION_TYPE_" + baseName.toUpperCase();

		try {
		    return (String) JMODELConstants.class.getField(baseType).get(null);
		} catch (IllegalAccessException e) {
		    System.out.println("A security manager has blocked reflection");
		    e.printStackTrace();
		} catch (NoSuchFieldException e) {
		    System.out.println("Reflection failed to find " + baseType + " constant.");
		    e.printStackTrace();
		}
		return null; */
	}

	/**
	 * Deletes a JmtCell component removing its data structure
	 * @param cell component to be eliminated
	 */
	public void deleteCell(JmtCell cell) {
		JmodelStationDefinition sd = mediator.getStationDefinition();
		sd.deleteStation(((CellComponent) cell.getUserObject()).getKey());
	}

	/**
	 * Calculate exact size of a <code>JmtCell</code> component before creating it.
	 * Will be used to avoid sovrapposition of nodes on the graph before placement.
	 * @param className name of the *Cell that will be instantiated
	 * @return predicted dimensions of Node
	 */
	public Dimension predictCellSize(String className) {
		JmodelStationDefinition sd = mediator.getStationDefinition();
		String path = "jmt.gui.jmodel.JGraphMod.";
		ImageIcon icon = null;
		// Using reflection to access to public ICON field on given class
		try {
			String iconName = (String) (Class.forName(path + className)).getField("ICON").get(null);
			icon = JMTImageLoader.loadImage(iconName);
		} catch (IllegalAccessException e) {
			System.out.println("Error: A security manager has blocked reflection...");
			e.printStackTrace();
			return null;
		} catch (NoSuchFieldException e) {
			System.out.println("Error: Field 'ICON' was not found in '" + className + "'. This is needed for reflection.");
			e.printStackTrace();
			return null;
		} catch (ClassNotFoundException e) {
			System.out.println("Error: Trying to get size fron a Cell class that does not exist");
			e.printStackTrace();
			return null;
		}

		String name = sd.previewStationName(getComponentType(className));

		Dimension cellDimension = new Dimension(icon.getIconWidth(), icon.getIconHeight());
		// Gets the graph font
		Font font = mediator.getGraph().getFont();
		// Gets the graphical context
		Graphics2D g2D = (Graphics2D) mediator.getGraph().getGraphics();
		// Gets the bounds of the cell name
		FontRenderContext frc = g2D.getFontRenderContext();
		Rectangle r = font.getStringBounds(name, frc).getBounds();
		// Sets the cell dimension
		cellDimension.height += r.height + 5;
		cellDimension.width = Math.max(cellDimension.width, r.width + 10);
		return cellDimension;
	}
}
