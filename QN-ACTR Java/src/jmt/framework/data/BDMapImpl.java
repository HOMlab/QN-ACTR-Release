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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 14-giu-2005
 * Time: 13.46.33
 * To change this template use Options | File Templates.
 */
public class BDMapImpl implements BDMap {

	//contains all of the elements inserted into this BDMapImpl
	private Object[][] contents = new Object[0][0];

	private Object[] xKeys = new Object[0], yKeys = new Object[0];

	/**returns keyset along a single dimension.
	 * @param coordName name of dimension.
	 * @return set containig search keys for specified dimension.*/
	public Set<Object> keySet(int coordName) {
		if (coordName == X) {
			return getSetFromArray(xKeys);
		}
		if (coordName == Y) {
			return getSetFromArray(yKeys);
		} else {
			return null;
		}
	}

	//returns a set containing elements of an array
	private Set<Object> getSetFromArray(Object[] array) {
		Set<Object> s = new TreeSet<Object>();
		for (Object element : array) {
			s.add(element);
		}
		return s;
	}

	/**tells wether a key mapping is present in this BDMap along a specified coord
	 * @param coordName: name of coordinate along which search must be performed.
	 * @param key: key to be searched.
	 * @return : true if mapping for specified key has been defined, false otherwise.*/
	public boolean containsKey(Object key, int coordName) {
		if (coordName == X) {
			return -1 != getIndex(xKeys, key);
		}
		if (coordName == Y) {
			return -1 != getIndex(yKeys, key);
		} else {
			return false;
		}
	}

	//checks if an object is contained into an array
	private int getIndex(Object[] array, Object key) {
		for (int i = 0; i < array.length; i++) {
			if (array[i] != null) {
				if (array[i].equals(key)) {
					return i;
				}
			}
		}
		return -1;
	}

	/**Tells wether this BDMap is empty.*/
	public boolean isEmpty() {
		return contents.length == 0;
	}

	/**Puts a value into this BDMap, given keys along the two coordinates
	 * @param keyX: key along x coordinate.
	 * @param keyY: key along y coordinate.
	 * @param value: value to be stored inside bdmap.*/
	public void put(Object keyX, Object keyY, Object value) {
		int i = getIndex(yKeys, keyY);
		int j = getIndex(xKeys, keyX);
		//if index of row was not found, add a row
		if (i == -1) {
			i = yKeys.length;
			addRow(yKeys.length, new Object[xKeys.length]);
			yKeys = add(yKeys, yKeys.length, keyY);
		}
		//if index of row was not found, add a row
		if (j == -1) {
			j = xKeys.length;
			addColumn(xKeys.length, new Object[yKeys.length]);
			xKeys = add(xKeys, xKeys.length, keyX);
		}
		contents[i][j] = value;
	}

	/**Puts a map into this BDMap, e.g. puts a row or a column into the table
	 * represented by this BDMap. If, for example, a map is to be added on the
	 * X coordinate, this hashmap should contain mapping for each entry along Y
	 * coordinate, otherwise new mapping (returning null value) are created. All of
	 * the mappings contained inside given map and not defined along Y coord will be
	 * deleted
	 * @param key: key along specified coordinate.
	 * @param coordName: coordinate along which mapping should be added.
	 * @param newMap: new row or column to be added.*/
	public void put(Object key, int coordName, Map newMap) {
		//Object array to be inserted as a column
		Object[] objects;
		//index of key, if present
		int keyIndex = -1;
		if (coordName == X) {
			keyIndex = getIndex(xKeys, key);
			objects = new Object[yKeys.length];
			//prepare array to be inserted as a column
			for (int i = 0; i < yKeys.length; i++) {
				//if key is contained in given map, put correspondent object in the array
				objects[i] = newMap.get(yKeys[i]);
			}
			//if key was found, replace current column, otherwise add mapping and column
			if (keyIndex != -1) {
				replaceColumn(keyIndex, objects);
			} else {
				addColumn(xKeys.length, objects);
				xKeys = add(xKeys, xKeys.length, key);
			}
		} else if (coordName == Y) {
			keyIndex = getIndex(yKeys, key);
			objects = new Object[xKeys.length];
			//prepare array to be inserted as a row
			for (int i = 0; i < xKeys.length; i++) {
				//if key is contained in given map, put correspondent object in the array
				objects[i] = newMap.get(xKeys[i]);
			}
			//if key was found, replace current row, otherwise add mapping and column
			if (keyIndex != -1) {
				replaceRow(keyIndex, objects);
			} else {
				addRow(yKeys.length, objects);
				yKeys = add(yKeys, yKeys.length, key);
			}
		}
	}

	/**Removes an entry from this BDMAp, given the two search keys pointing to it.
	 * @param keyX: key along x coordinate.
	 * @param keyY: key along y coordinate.
	 * @return : object removed from bdmap.
	 */
	public Object remove(Object keyX, Object keyY) {
		Object retval = null;
		int xIndex = getIndex(xKeys, keyX), yIndex = getIndex(yKeys, keyY);
		if (xIndex != -1 && yIndex != -1) {
			retval = contents[yIndex][xIndex];
			contents[yIndex][xIndex] = null;
		}
		return retval;
	}

	/**Removes an entry from this BDMap, given the two search keys pointing to it.
	 * @param key: key along specified coordinate.
	 * @param coordName: coordinate along which mapping should be removed.
	 * @return : map containing objects removed from this BDMap. Returned map contains
	 * mappings for the other coordinate search keys {@see put(Object key, int coordName,
	        *  HashMap newMap)}
	 */
	public Map remove(Object key, int coordName) {
		//creating returned map optimized on maximum dimension.
		Map retval = null;
		if (coordName == X) {
			retval = new HashMap(yKeys.length);
			int index = getIndex(xKeys, key);
			//key not found
			if (index == -1) {
				return null;
			} else {
				//build returned map
				for (int i = 0; i < yKeys.length; i++) {
					retval.put(yKeys[i], contents[i][index]);
				}
				deleteColumn(index);
				xKeys = delete(xKeys, index);
				return retval;
			}
		} else if (coordName == Y) {
			retval = new HashMap(xKeys.length);
			int index = getIndex(yKeys, key);
			//key not found
			if (index == -1) {
				return null;
			} else {
				//build returned map
				for (int i = 0; i < xKeys.length; i++) {
					retval.put(xKeys[i], contents[index][i]);
				}
				deleteRow(index);
				yKeys = delete(yKeys, index);
				return retval;
			}
		}
		return null;
	}

	public Object get(Object keyX, Object keyY) {
		int xIndex = getIndex(xKeys, keyX), yIndex = getIndex(yKeys, keyY);
		if (xIndex != -1 && yIndex != -1) {
			return contents[yIndex][xIndex];
		} else {
			return null;
		}
	}

	public Map get(Object key, int coordName) {
		//creating returned map.
		Map retval = null;
		if (coordName == X) {
			retval = new HashMap(yKeys.length);
			int index = getIndex(xKeys, key);
			//key not found
			if (index == -1) {
				return null;
			} else {
				//build returned map
				for (int i = 0; i < yKeys.length; i++) {
					retval.put(yKeys[i], contents[i][index]);
				}
				return retval;
			}
		} else if (coordName == Y) {
			retval = new HashMap(xKeys.length);
			int index = getIndex(yKeys, key);
			//key not found
			if (index == -1) {
				return null;
			} else {
				//build returned map
				for (int i = 0; i < xKeys.length; i++) {
					retval.put(xKeys[i], contents[index][i]);
				}
				return retval;
			}
		}
		return null;
	}

	/**returns size of this BDMap along specified coordinate. For example, if x coordinate
	 * is specified, where x is the coordinate for cells in a row of table represented by
	 * this BDMap, total number of columns is returned.
	 * @param coordName: name of the coordinate along which size must be given*/
	public int size(int coordName) {
		if (coordName == X) {
			return xKeys.length;
		}
		if (coordName == Y) {
			return yKeys.length;
		}
		return 0;
	}

	//adds a row to bidimensional array
	private void addRow(int index, Object[] row) {
		//index out of bounds, do nothing
		if (index > contents.length || index < 0) {
			return;
		}
		if (contents.length == 0) {
			contents = new Object[][] { row };
			return;
		}
		Object[][] newConts = new Object[contents.length + 1][contents[0].length];
		System.arraycopy(contents, 0, newConts, 0, index);
		newConts[index] = row;
		System.arraycopy(contents, index, newConts, index + 1, contents.length - index);
		contents = newConts;
	}

	//replaces  a row into bidimensional array and returns former one
	private Object[] replaceRow(int index, Object[] row) {
		//index out of bounds, do nothing
		if (index >= contents.length || index < 0) {
			return null;
		}
		if (contents.length == 0) {
			return null;
		}
		//if row has not same size as it should, resize it
		int correctRowSize = contents[0].length;
		if (row.length != correctRowSize) {
			Object[] newRow = new Object[correctRowSize];
			System.arraycopy(row, 0, newRow, 0, Math.min(correctRowSize, row.length));
			row = newRow;
		}
		Object[] retval = contents[index];
		contents[index] = row;
		return retval;
	}

	//deletes a row from bidimensional array
	private Object[] deleteRow(int index) {
		//index out of bounds, do nothing
		if (index >= contents.length || index < 0) {
			return null;
		}
		if (contents.length == 0) {
			return null;
		}
		Object[][] newConts = new Object[contents.length - 1][contents[0].length];
		Object[] retVal = contents[index];
		System.arraycopy(contents, 0, newConts, 0, index);
		System.arraycopy(contents, index + 1, newConts, index, newConts.length - index);
		contents = newConts;
		return retVal;
	}

	//replaces  a column into bidimensional array and returns former one
	private Object[] replaceColumn(int index, Object[] column) {
		if (contents.length == 0) {
			return null;
		}
		//index out of bounds, do nothing
		if (index >= contents[0].length || index < 0) {
			return null;
		}
		//if row has not same size as it should, resize it
		int correctColSize = contents.length;
		if (column.length != correctColSize) {
			Object[] newCol = new Object[correctColSize];
			System.arraycopy(column, 0, newCol, 0, Math.min(correctColSize, column.length));
			column = newCol;
		}
		Object[] retval = contents[index];
		for (int i = 0; i < contents.length; i++) {
			retval[i] = contents[i][index];
			contents[i][index] = column[i];
		}
		return retval;
	}

	//adds a column to the bidimensional array
	private void addColumn(int index, Object[] column) {
		if (contents.length == 0) {
			contents = new Object[column.length][1];
			for (int i = 0; i < contents.length; i++) {
				contents[i][0] = column[i];
			}
		}
		//index out of bounds, do nothing
		if (index > contents[0].length || index < 0) {
			return;
		}
		for (int i = 0; i < contents.length; i++) {
			contents[i] = add(contents[i], index, column[i]);
		}
	}

	//deletes a column from bidimensional array
	private Object[] deleteColumn(int index) {
		if (contents.length == 0) {
			return null;
		}
		//index out of bounds, do nothing
		if (index >= contents[0].length || index < 0) {
			return null;
		}
		Object[] retval = new Object[contents.length];
		for (int i = 0; i < contents.length; i++) {
			retval[i] = contents[i][index];
			contents[i] = delete(contents[i], index);
		}
		return retval;
	}

	//inserts a null value to specified array at specified index resizing it
	private static Object[] add(Object[] array, int index, Object toAdd) {
		if (index < 0 || index > array.length) {
			return array;
		}
		if (array.length == 0) {
			return new Object[] { toAdd };
		}
		Object[] newArray = new Object[array.length + 1];
		System.arraycopy(array, 0, newArray, 0, index);
		newArray[index] = toAdd;
		System.arraycopy(array, index, newArray, index + 1, array.length - index);
		return newArray;
	}

	//deletes an object from specified array at specified index
	private static Object[] delete(Object[] array, int index) {
		if (index < 0 || index >= array.length) {
			return null;
		}
		if (array.length == 0) {
			return new Object[0];
		}
		Object[] newArray = new Object[array.length - 1];
		System.arraycopy(array, 0, newArray, 0, index);
		System.arraycopy(array, index + 1, newArray, index, newArray.length - index);
		return newArray;
	}

	public static void main(String[] args) {
		BDMapImpl bdmi = new BDMapImpl();
		int testSize = 3, tstSize2 = 2;
		for (int i = 0; i < testSize; i++) {
			for (int j = 0; j < testSize; j++) {
				String str = "elemento (" + i + "," + j + ")";
				System.out.println("adding " + str);
				System.out.println("xKeys: " + bdmi.xKeys.length + ", yKeys: " + bdmi.yKeys.length + " elements");
				bdmi.put(new Integer(i), new Integer(j), str);
				System.out.println("xKeys: " + bdmi.xKeys.length + ", yKeys: " + bdmi.yKeys.length + " elements");
			}
		}
		for (int i = 0; i < tstSize2; i++) {
			for (int j = 0; j < tstSize2; j++) {
				String str = "element (" + i + "," + j + ")";
				System.out.println("adding " + str);
				System.out.println("xKeys: " + bdmi.xKeys.length + ", yKeys: " + bdmi.yKeys.length + " elements");
				bdmi.put(new Integer(i), new Integer(j), str);
				System.out.println("xKeys: " + bdmi.xKeys.length + ", yKeys: " + bdmi.yKeys.length + " elements");
			}
		}
		for (int i = 0; i < testSize; i++) {
			for (int j = 0; j < testSize; j++) {
				String str = "elemento (" + i + "," + j + ")";
				System.out.println("requiring element " + str + ": " + bdmi.get(new Integer(i), new Integer(j)));
			}
		}
		for (int i = 0; i < testSize; i++) {
			for (int j = 0; j < testSize; j++) {
				String str = "elemento (" + i + "," + j + ")";
				System.out.println("deleting " + str);
				System.out.println("xKeys: " + bdmi.xKeys.length + ", yKeys: " + bdmi.yKeys.length + " elements");
				bdmi.remove(new Integer(i), new Integer(j));
				System.out.println("xKeys: " + bdmi.xKeys[i] + ", yKeys: " + bdmi.yKeys[j]);
			}
		}
		for (int i = 0; i < testSize; i++) {
			for (int j = 0; j < testSize; j++) {
				String str = "elemento (" + i + "," + j + ")";
				System.out.println("requiring element " + str + ": " + bdmi.get(new Integer(i), new Integer(j)));
			}
		}
	}

}
