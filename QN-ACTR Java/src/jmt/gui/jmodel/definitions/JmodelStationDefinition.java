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

package jmt.gui.jmodel.definitions;

import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title:Jmodel Station Definition Interface</p>
 * <p>Description: This interface provides methods for editing stations for JMODEL models.
 * It actually extends JSIM StationDefinition interface to provide a compatibility layer</p>
 * 
 * @author Bertoli Marco
 *         Date: 3-giu-2005
 *         Time: 10.08.11
 */
public interface JmodelStationDefinition extends StationDefinition {
	/**
	 * Adds a new station to the model. Only type is required as a default name will be set.
	 * @param type Constant string from JMODELConstants interface.
	 * @return key of search for newly created station
	 */
	public Object addStation(String type);

	/**
	 * Gets a preview of name that will be assigned automatically to new created object of
	 * specified type. This method is used to calculate dimensions of caption to be displayed
	 * under component on jgraph window
	 * @param type Constant string from JMODELConstants interface.
	 * @return name assigned to next station of 'type' type.
	 */
	public String previewStationName(String type);

	/**
	 * Returns a serialized form of a given station to support cut/copy operations
	 * @param key Search's key for station
	 * @return serialized station or null if station key does not exists
	 */
	public Object serializeStation(Object key);

	/**
	 * Inserts a serialized station into data structure, used to support paste operation.
	 * @param serializedForm station's Serialized form got from <code>serializeStation</code>
	 * method.
	 * @return search's key for inserted station
	 */
	public Object deserializeStation(Object serializedForm);

	/**
	 * Sets position for a target station into jgraph window. This method is used to
	 * implement save routine
	 * @param key key of search for target station
	 * @param position position where target station is placed
	 */
	public void setStationPosition(Object key, JMTPoint position);

	/**
	 * Returns stored position for a given station into jgraph window
	 * (Note that this is stored position, not original position. This method
	 * is used to implement load routine)
	 * @param key key of search for target station
	 * @return position where target station has to be placed
	 */
	public JMTPoint getStationPosition(Object key);

}
