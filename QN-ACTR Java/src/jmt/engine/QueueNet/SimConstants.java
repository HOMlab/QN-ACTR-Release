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
 * Constants used by QueueNet package.
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 */
public class SimConstants {

	//-------------------- SIMULATION MEASURE IDENTIFIERS ----------------------------//

	/** Measure identifier: number of jobs in queue.   */
	public static final int QUEUE_LENGTH = 0;

	/** Measure identifier: utilization of server.   */
	public static final int UTILIZATION = 2;

	/** Measure identifier: throughput of server.   */
	public static final int THROUGHPUT = 3;

	/** Measure identifier: response of the the node.   */
	public static final int RESPONSE_TIME = 4;

	/** Measure identifier: residence time of the node.   */
	public static final int RESIDENCE_TIME = 5;

	/** Measure identifier: queue time of the node.   */
	public static final int QUEUE_TIME = 6;

	/** Measure identifier: drop rate of the node. (Bertoli Marco)  */
	public static final int DROP_RATE = 7;

	/** Measure identifier: response time of the system (Bertoli Marco)  */
	public static final int SYSTEM_RESPONSE_TIME = 8;

	/** Measure identifier: throughput of the system (Bertoli Marco)  */
	public static final int SYSTEM_THROUGHPUT = 9;

	/** Measure identifier: number of jobs in the system (Bertoli Marco)  */
	public static final int SYSTEM_JOB_NUMBER = 10;

	/** Measure identifier: drop rate of the system (Bertoli Marco)  */
	public static final int SYSTEM_DROP_RATE = 11;

	//Added by ASHANKA START
	//Id of the system power
	public static final int SYSTEM_POWER = 12;
	//Added by ASHANKA STOP
	public static final int THROUGHPUT_PER_SINK = 13;
	public static final int RESPONSE_TIME_PER_SINK = 14;

	//-------------------- end SIMULATION MEASURE IDENTIFIERS -------------------------//

	//-------------------- JOB LIST MEASURE IDENTIFIERS ----------------------------//

	/** Measure identifier: number of jobs in queue.   */
	public static final int LIST_NUMBER_OF_JOBS = 21;

	/** Measure identifier: throughput of server.   */
	public static final int LIST_THROUGHPUT = 22;

	/** Measure identifier: response of the system until the section of the node.   */
	public static final int LIST_RESIDENCE_TIME = 23;

	/** Measure identifier: time spent before reaching this node.   */
	public static final int LIST_SYSTEM_RESPONSE_TIME = 24;

	/** Measure identifier: residence of the system until the section of the node.   */
	public static final int LIST_RESPONSE_TIME = 25;

	/** Measure identifier: drop rate of a finite queue.   */
	public static final int LIST_DROP_RATE = 26;

	//-------------------- end JOB LIST MEASURE IDENTIFIERS -------------------------//
	/** To be used for a blocking region measure */
	public static final String NODE_TYPE_REGION = "region";
	/** To be used for a station measure. This is default */
	public static final String NODE_TYPE_STATION = "station";

}
