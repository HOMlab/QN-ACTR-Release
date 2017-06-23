/**    
  * Copyright (C) 2008, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.framework.data;

import java.util.HashMap;

/**
 * <p><b>Name:</b> ConstMap</p> 
 * <p><b>Description:</b> 
 * A data structure suitable to create constant maps. Its values cannot be changed.
 * </p>
 * <p><b>Date:</b> 09/ott/2008
 *    <b>Time:</b> 18:57:49</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public abstract class ConstMap<E, F> {
	private HashMap<E, F> map = new HashMap<E, F>();

	/**
	 * Fills this constant map with values. Call putConst method to fill.
	 * @see #putConst(Object, Object)
	 */
	protected abstract void fill();

	public ConstMap() {
		fill();
	}

	/**
	 * Put an element in this constmap. May be called only from the fill method.
	 * @param key the key for the constant
	 * @param value constant value
	 */
	protected void putConst(E key, F value) {
		map.put(key, value);
	}

	/**
	 * Returns an object in this map.
	 * @param key the key for the object
	 * @return the object or null if not found
	 */
	public F get(Object key) {
		return map.get(key);
	}
}
