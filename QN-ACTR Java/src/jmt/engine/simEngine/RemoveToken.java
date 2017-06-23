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

/**
 * <p><b>Name:</b> RemoveToken</p> 
 * <p><b>Description:</b> 
 * This class works as a token to remove future and deferred events. It's designed to disallow access to the internal
 * data structure.
 * </p>
 * <p><b>Date:</b> 25/mag/2009
 * <b>Time:</b> 09:52:11</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class RemoveToken {
	private SimEvent event;
	private boolean deferred;

	/**
	 * Builds a new future remove token for given event.
	 * @param event the event to remove
	 */
	// Please do not change visibility
	RemoveToken(SimEvent event) {
		this(event, false);
	}

	/**
	 *  Builds a new remove token for given event.
	 * @param event the event
	 * @param deferred true if event is deferred, false otherwise
	 */
	// Please do not change visibility
	RemoveToken(SimEvent event, boolean deferred) {
		this.event = event;
		this.deferred = deferred;
	}

	/**
	 * @return the event to remove from future events
	 */
	// Please do not change visibility
	SimEvent getEvent() {
		return event;
	}

	/**
	 * @return if the event was deferred
	 */
	// Please do not change visibility
	boolean isDeferred() {
		return deferred;
	}
}
