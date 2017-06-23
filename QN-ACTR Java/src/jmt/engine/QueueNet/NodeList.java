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

package jmt.engine.QueueNet;

import java.util.LinkedList;
import java.util.ListIterator;

/** This class implements a node list. Note that only classes of QueueNet
 * package can add or remove objects to/from the list.
 * @author Francesco Radaelli
 */
public class NodeList {

	private LinkedList<NetNode> nodes;

	/** Creates a new instance of NodeList class.
	 */
	public NodeList() {
		nodes = new LinkedList<NetNode>();
	}

	/** Adds a new node to the list
	 * @param node Reference to the node to be added.
	 */
	public void add(NetNode node) {
		nodes.add(node);
	}

	/** Removes a node from the list
	 * @param node Reference to the node to be removed.
	 */
	void remove(NetNode node) {
		nodes.remove(node);
	}

	/** Gets the first node in the list.
	 * @return Reference to the first node.
	 */
	public NetNode getFirst() {
		return nodes.getFirst();
	}

	/** Gets the last node in the list.
	 * @return Reference to the last node.
	 */
	public NetNode getLast() {
		return nodes.getLast();
	}

	/** Gets the node with a specific name in the list.
	 * @return Reference to the node .
	 */
	public NetNode get(String name) {
		ListIterator<NetNode> iterator = nodes.listIterator();
		NetNode node;
		while (iterator.hasNext()) {
			node = iterator.next();
			if (node.getName().compareTo(name) == 0) {
				return node;
			}
		}
		return null;
	}

	/** Gets the i-th node in the list.
	 * @param index index.
	 * @return Reference to the i-th node.
	 */
	public NetNode get(int index) {
		return nodes.get(index);
	}

	/** Gets list size.
	 * @return Number of nodes in the list.
	 */
	public int size() {
		return nodes.size();
	}

	/** Gets a list iterator.
	 * @return List iterator.
	 */
	public ListIterator<NetNode> listIterator() {
		return nodes.listIterator();
	}

	/**
	 * Converts the NodeList into an array of NetNode objects.
	 */
	public NetNode[] toArray() {
		return nodes.toArray(new NetNode[nodes.size()]);
	}

	/**
	 * Used to know whether a node is contained or not in the NodeList
	 * @param node Node to search
	 * @return true if the specified node is contained in the list
	 */
	public boolean contains(NetNode node) {
		return nodes.contains(node);
	}
}
