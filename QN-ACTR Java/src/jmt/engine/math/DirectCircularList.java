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
package jmt.engine.math;

import java.util.Arrays;

/**
 * <p>Title: Direct Circular List</p>
 * <p>Description: This class implements a direct access O(1) fixed size circular
 * list extremely useful for caching purposes.</p>
 *
 * @author Bertoli Marco
 *         Date: 14-feb-2006
 *         Time: 11.17.24
 */
public class DirectCircularList<E> {
	// Pointer to last inserted element and its index for direct access
	protected int last, lastIndex;
	// Array of datas
	protected Object[] data;
	// Size of the list
	protected int size;

	/**
	 * Creates a new DirectCircularList with specified size
	 * @param size maximum size of the data structure
	 */
	public DirectCircularList(int size) {
		this.size = size;
		data = new Object[size];
		last = lastIndex = 0;
	}

	/**
	 * Sets element in position index to specified value. This method is O(1) if values are inserted
	 * with adiacent indices, otherwise some computation is needed to mark null elements.
	 * @param index position of item to be set
	 * @param value value for the item
	 */
	public void set(int index, E value) {
		// Poition where new object has to be placed
		int pos = index % size;

		// Discards invalid data
		if (index > lastIndex + size || index <= lastIndex - 2 * size) {
			// Entire array must be invalidated
			Arrays.fill(data, null);
			lastIndex = index;
			last = pos;
		} else if (index > lastIndex + 1) {
			// Array is going up
			if (pos > last) {
				Arrays.fill(data, last + 1, pos, null);
			} else {
				// Last part of the array
				Arrays.fill(data, last + 1, size, null);
				// First part of the array
				Arrays.fill(data, 0, pos, null);
			}
			lastIndex = index;
			last = pos;
		} else if (index > lastIndex) {
			// Item is added at the end of the list
			lastIndex = index;
			last = pos;
		} else if (index <= lastIndex - size) {
			// Array is going down
			if (pos < last) {
				Arrays.fill(data, pos, last + 1, null);
			} else {
				// Last part of the array
				Arrays.fill(data, pos, size, null);
				// First part of the array
				Arrays.fill(data, 0, last + 1, null);
			}
			last = (pos + (size - 1)) % size; // Subtract 1 to pos
			lastIndex = index + size - 1;
		}
		data[pos] = value;

	}

	/**
	 * Returns value for item in position index or null if it doesn't exist.
	 * This method is always O(1).
	 * @param index index of element to be retrived
	 * @return requested element or null if it doesn't exist
	 * @see this.getSize()
	 */
	@SuppressWarnings("unchecked")
	public E get(int index) {
		int pos = index % size;
		if (index > lastIndex || index <= lastIndex - size) {
			return null;
		} else {
			return (E)data[pos];
		}
	}

	/**
	 * Tells if specified element is present in the list. This method is O(1).
	 * @param index index of specified element
	 * @return true iff element is not null
	 */
	public boolean exists(int index) {
		return get(index) != null;
	}

	/**
	 * Returns size of this Circular List. This method is O(1).
	 * @return size of this Circular List
	 */
	public int getSize() {
		return size;
	}

	/**
	 * Used for debugging purposes: provides a string rappresentation of every object in the data structure
	 * @return a string rappresentation of every object in the data structure
	 */
	@Override
	public String toString() {
		String ret = "last: " + last + "     lastIndex: " + lastIndex + "\n";
		for (int i = 0; i < data.length; i++) {
			ret += i + ": " + ((data[i] != null) ? data[i].toString() : "null") + "\n";
		}

		return ret;
	}
}
