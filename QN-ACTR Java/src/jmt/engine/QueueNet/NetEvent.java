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

/**
 * This class declares all the constants related to message event types.
 * @author Francesco Radaelli
 */
public class NetEvent {

	/*------------------------------BIT MASK----------------------------------*/

	static final int EVENT_MASK = 0x0000FFFF;

	/*------------------------------EVENTS------------------------------------*/

	/** Event ID: Aborts the simulation.*/
	public static final int EVENT_ABORT = 0x0000;
	/** Event ID: Stops the simulation.*/
	public static final int EVENT_STOP = 0x0001;
	/** Event ID: Starts the simulation.*/
	public static final int EVENT_START = 0x0002;
	/** Event ID: The event contains a job.*/
	public static final int EVENT_JOB = 0x0004;
	/** Event ID: Job ack event.*/
	public static final int EVENT_ACK = 0x0008;
	/** Event ID: Keeps node awake.*/
	public static final int EVENT_KEEP_AWAKE = 0x0010;
	//todo: eventualmente chiamarlo fork-join?? Oppure dividere in due eventi distinti
	/** Event ID: join ack.*/
	public static final int EVENT_JOIN = 0x0020;

	//todo: evento per job che lasciano regione critica
	/** Event ID: join ack.*/
	public static final int EVENT_JOB_OUT_OF_REGION = 0x0040;

	/** Event ID: Distribution change event.*/
	public static final int EVENT_DISTRIBUTION_CHANGE = 0x0080;

}
