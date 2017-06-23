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

package jmt.gui.jmodel.mainGui;

import java.lang.reflect.Field;

import jmt.framework.gui.components.JMTToolBar;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jmodel.controller.Mediator;
import jmt.gui.jmodel.controller.actions.SetInsertState;

/**
 * <p>Title: Component Toolbar</p>
 * <p>Description: A toolbar used to insert new stations in the graph, or to go
 * into "select mode" or "link mode". This class uses reflection on
 * <code>JMODELConstants</code>, in such way a future placement of new station
 * types will be easy.</p>
 * 
 * @author Bertoli Marco
 *         Date: 4-giu-2005
 *         Time: 14.13.55
 * 
 * Modified by Giuseppe De Cicco & Fabio Granara
 */
public class ComponentBar extends JMTToolBar {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public ComponentBar(Mediator m) {
		super(JMTImageLoader.getImageLoader());
		// Adds Select button
		addGenericButton(m.getSetSelect());
		addSeparator();
		// Adds insert mode buttons.
		String[] stations = getStationList();
		for (String station : stations) {
			addGenericButton(new SetInsertState(m, station));
		}
		addSeparator();
		// Adds link button
		addGenericButton(m.getSetConnect());
		// Blocking region button
		addGenericButton(m.getAddBlockingRegion());

		addSeparator();
		// Rotate button 
		addGenericButton(m.getRotate());
		// Set right button
		addGenericButton(m.getSetRight());

		// Disables all components button
		enableButtons(false);
	}

	/**
	 * Finds all possible station types using reflection on <code>JMODELConstants</code>
	 * In such way a future placement of new station types will be easy.
	 * @return All station types element found
	 */
	private String[] getStationList() {
		String path = "jmt.gui.jmodel.JGraphMod.";
		Field[] fields = jmt.gui.jmodel.JMODELConstants.class.getFields();
		String[] stationNames = new String[fields.length];
		int index = 0;
		try {
			for (Field field : fields) {
				if (field.getName().startsWith("STATION_TYPE_")) {
					// Checks for existance of graphic component
					String name = ((String) field.get(null)).replaceAll(" ", "") + "Cell";
					try {
						boolean enabled = Class.forName(path + name).getDeclaredField("canBePlaced").getBoolean(null);
						if (enabled) {
							stationNames[index++] = name;
						}
					} catch (ClassNotFoundException e) {
						// Never Thrown
						e.printStackTrace();
					} catch (NoSuchFieldException ex) {
						// Never Thrown
						ex.printStackTrace();
					}
				}
			}
		} catch (IllegalAccessException ex) {
			System.out.println("A security manager has blocked reflection");
			ex.printStackTrace();
		}
		// Build *Cell array to be given as output
		String[] tmp = new String[index];
		for (int i = 0; i < index; i++) {
			tmp[i] = stationNames[i];
		}
		return tmp;
	}
}
