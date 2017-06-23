/**    
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

package jmt.engine.QueueNet;

import java.util.LinkedList;
import java.util.ListIterator;

/** This class implements a network list. Note that only classes of QueueNet
 * package can add or remove objects to/from the list.
 * @author Francesco Radaelli, Bertoli Marco
 */
public class NetworkList {

	private LinkedList<QueueNetwork> networks;

	/** Creates a new instance of NetworkList object
	 */
	NetworkList() {
		networks = new LinkedList<QueueNetwork>();
	}

	/** Adds a new network to the list.
	 * @param Network Reference to the NetWork to be added.
	 */
	void add(QueueNetwork network) {
		networks.add(network);
	}

	/** Removes a network from the list.
	 * @param Network Reference to the Network to be removed.
	 */
	void remove(QueueNetwork network) {
		networks.remove(network);
	}

	/** Gets first network in the list.
	 * @return First network in the list.
	 */
	public QueueNetwork getFirst() {
		return networks.getFirst();
	}

	/** Gets last network in the list.
	 * @return Last network in the list.
	 */
	public QueueNetwork getLast() {
		return networks.getLast();
	}

	/** Gets i-th network in the list.
	 * @return Index-th network in the list.
	 */
	public QueueNetwork get(int index) {
		return networks.get(index);
	}

	/** Gets the network in the list, if it exists, which has the specified Name.
	 * @return The network called "Name". Returns Null if it doesn't exist.
	 */
	public QueueNetwork get(String name) {
		ListIterator<QueueNetwork> iterator = networks.listIterator();
		QueueNetwork jc;
		while (iterator.hasNext()) {
			jc = iterator.next();
			if (jc.getName().compareTo(name) == 0) {
				return jc;
			}
		}
		return null;
	}

	/** Gets list size.
	 * @return Number of networks in the list.
	 */
	public int size() {
		return networks.size();
	}

	/**
	 * Gets a list iterator
	 *
	 */
	public ListIterator<QueueNetwork> listIterator() {
		return networks.listIterator();
	}

	/**Returns the index in this list of the first
	 * occurrence of the specified element, or -1
	 * if the List does not contain this element.
	 */

	int indexOf(QueueNetwork network) {
		return networks.indexOf(network);
	}
}
