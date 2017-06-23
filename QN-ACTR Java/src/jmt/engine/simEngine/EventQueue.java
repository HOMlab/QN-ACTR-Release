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
package jmt.engine.simEngine;

import java.util.Iterator;

/**
 * <p><b>Name:</b> EventQueue</p> 
 * <p><b>Description:</b> 
 * Event Queue specifies a queue of SimEvent objects sorted by time and insertion order.
 * This class holds all simulation events, thus its performances impact dramatically the simulation.
 * </p>
 * <p><b>Date:</b> 12/mag/2009
 * <b>Time:</b> 08:02:18</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public interface EventQueue extends Iterable<SimEvent> {
	/**
	 * @return the number of events in this queue
	 */
	public int size();

	/**
	 * Adds an event to the queue
	 * @param event the event to add
	 * @return true if element was added, false otherwise.
	 */
	public boolean add(SimEvent event);

	/**
	 * @return the first event of the queue, removing it from the queue
	 */
	public SimEvent pop();

	/**
	 * @return the first event of the queue, without removing it
	 */
	public SimEvent peek();

	/**
	 * Removes given event
	 * @param ev the event to remove
	 * @return true if event was found, false otherwise
	 */
	public boolean remove(SimEvent ev);

	/**
	 * Clears this queue, removing all pointers
	 */
	public void clear();

	/**
	 * Returns an iterator that iterates over this queue elements in no special order. Iterator must implement remove() method.
	 * @return an iterator with no special order.
	 */
	public Iterator<SimEvent> iterator();
}
