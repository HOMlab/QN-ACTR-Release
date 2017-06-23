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

package jmt.framework.data;

import java.util.Map;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 13-giu-2005
 * Time: 15.51.14
 * This interface provides methods for implementation of a bi-dimensional hash map.
 * This type of data structure must provide a set of entries acessed through a combination
 * of two search key. It is a sort of table in which objects are accessed via a couple of
 * objects (representing a search key) instead of a couple of indices.
 */
public interface BDMap {

	/**code for x coordinate*/
	static final int X = 0;
	/**code for y coordinate*/
	static final int Y = 1;

	/**returns keyset along a single dimension.
	 * @param coordName name of dimension.
	 * @return set containig search keys for specified dimension.*/
	Set<Object> keySet(int coordName);

	/**tells wether a key mapping is present in this BDMap along a specified coord
	 * @param coordName: name of coordinate along which search must be performed.
	 * @param key: key to be searched.
	 * @return : true if mapping for specified key has been defined, false otherwise.*/
	boolean containsKey(Object key, int coordName);

	/**Tells wether this BDMap is empty.*/
	boolean isEmpty();

	/**Puts a value into this BDMap, given keys along the two coordinates
	 * @param keyX: key along x coordinate.
	 * @param keyY: key along y coordinate.
	 * @param value: value to be stored inside bdmap.*/
	void put(Object keyX, Object keyY, Object value);

	/**Puts a map into this BDMap, e.g. puts a row or a column into the table
	 * represented by this BDMap. If, for example, a map is to be added on the
	 * X coordinate, this hashmap should contain mapping for each entry along Y
	 * coordinate, otherwise new mapping (returning null value) are created. All of
	 * the mappings contained inside given map and not defined along Y coord will be
	 * deleted
	 * @param key: key along specified coordinate.
	 * @param coordName: coordinate along which mapping should be added.
	 * @param newMap: new row or column to be added.*/
	void put(Object key, int coordName, Map newMap);

	/**Removes an entry from this BDMAp, given the two search keys pointing to it.
	 * @param keyX: key along x coordinate.
	 * @param keyY: key along y coordinate.
	 * @return : object removed from bdmap.
	 */
	Object remove(Object keyX, Object keyY);

	/**Removes an entry from this BDMap, given the two search keys pointing to it.
	 * @param key: key along specified coordinate.
	 * @param coordName: coordinate along which mapping should be removed.
	 * @return : map containing objects removed from this BDMap. Returned map contains
	 * mappings for the other coordinate search keys {@see put(Object key, int coordName,
	        *  HashMap newMap)}
	 */
	Map remove(Object key, int coordName);

	/**Returns an object contained into this BDMAp, given the two search keys
	 * pointing to it.
	 * @param keyX: key along x coordinate.
	 * @param keyY: key along y coordinate.
	 * @return : object searched from bdmap.
	 */
	Object get(Object keyX, Object keyY);

	/**Returns a row or a column contained into this BDMap, given the search
	 * key pointing to it and the coordname this key refers to.
	 * @param key: key along specified coordinate.
	 * @param coordName: coordinate along which mapping should be returned.
	 * @return : map containing objects contained into this BDMap. Returned map contains
	 * mappings for the other coordinate search keys {@see put(Object key, int coordName,
	        *  HashMap newMap)}
	 */
	Map get(Object key, int coordName);

	/**returns size of this BDMap along specified coordinate. For example, if x coordinate
	 * is specified, where x is the coordinate for cells in a row of table represented by
	 * this BDMap, total number of columns is returned.
	 * @param coordName: name of the coordinate along which size must be given*/
	int size(int coordName);

}
