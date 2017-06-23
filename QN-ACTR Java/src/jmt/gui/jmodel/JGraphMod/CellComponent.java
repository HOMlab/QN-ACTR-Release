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

import jmt.gui.jmodel.definitions.JmodelStationDefinition;

/**
 * <p>Title: Cell Component</p>
 * <p>Description: This class is the structure saved on each jmtgraph cell.
 * Basically it will only store object key and provide toString() method
 * to display component name on the gui.</p>
 * 
 * @author Bertoli Marco
 *         Date: 3-giu-2005
 *         Time: 10.48.11
 */
public class CellComponent {
	protected Object key;
	protected JmodelStationDefinition sd;

	/**
	 * Creates a new data structure with given key and referencing given StationDefinition
	 * @param key station key
	 * @param sd referencing station definition
	 */
	public CellComponent(Object key, JmodelStationDefinition sd) {
		this.key = key;
		this.sd = sd;
	}

	/**
	 * Returns name of this station
	 * @return Name of this station
	 */
	@Override
	public String toString() {
		return sd.getStationName(key);
	}

	/**
	 * Returns key referencing this station
	 * @return this station's key
	 */
	public Object getKey() {
		return key;
	}
}
