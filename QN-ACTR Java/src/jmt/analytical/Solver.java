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

package jmt.analytical;

import jmt.engine.math.Printer;

/**
 * Solver is an abstract class, which contains the methods to
 * initialize and solve models with a single class of jobs.<br>
 * @author Federico Granata
 * @version Date: 8-set-2003; Time: 9.43.21
 */
public abstract class Solver {

	/**--------------CONSTANTS DEFINITION------------------------*/

	/** constant for Load Dependent service center  */
	public final static int LD = 0;

	/** constant for Load Independent service center  */
	public final static int LI = 1;

	/** constant for delay center*/
	public final static int DELAY = 2;

	/**---------------MODEL DEFINITION------------------------*/

	/**number of resources*/
	protected int stations = 0;

	/** array containing the names of the stations  */
	protected String[] name;

	/** array containing the types of the stations  */
	protected int[] type;

	/** array of array (i.e. matrix) containing the service rates of the service centers
	 * [station] [population]  */
	protected double[][] servTime;

	/** array containing the number of visits to each service center  */
	protected double[] visits;

	/**---------------MODEL SOLUTION------------------------*/

	/** array containing the throughput of each service center  */
	protected double[] throughput;

	/** array containing the utilization of each service center  */
	protected double[] utilization;

	/** array containing the queue length of each service center  */
	protected double[] queueLen;

	/** array containing the residence time of each service center.
	 * residence time = time spent in queue + time spent in service
	 */
	protected double[] residenceTime;

	/**  total throughput */
	protected double totThroughput = 0;

	/**  total response time */
	protected double totRespTime = 0;

	/**  total number of users */
	protected double totUser = 0;

	/**
	 *  Initializes the solver with the model parameters.
	 *  It must be called before trying to solve the model.
	 *  @param  n   array of n of service centers.
	 *  @param  t   array of the types (LD or LI) of service centers.
	 *  @param  s   matrix of service time of the service centers.
	 *  @param  v   array of visits to the service centers.
	 *  @return true if the operation is completed with success
	 */
	public boolean input(String[] n, int[] t, double[][] s, double[] v) {

		//OLD
		//if ((n.length > stations) || (t.length > stations) || (s.length > stations) || (v.length > stations))
		//NEW
		//@author Stefano Omini
		if ((n.length != stations) || (t.length != stations) || (s.length != stations) || (v.length != stations)) {
			return false; // wrong input.
		}
		for (int i = 0; i < stations; i++) {
			name[i] = n[i];
			type[i] = t[i];
			visits[i] = v[i];
			//OLD
			//if (type[i] != SolverSingleClosedExact.LD) {
			//NEW
			if (type[i] != Solver.LD) {
				//service centre is LI or DELAY
				servTime[i][0] = s[i][0];
			} else {
				//LD center
				//the input method is overridden in SolverSingleClosedMVA, where LD
				//centers are allowed
				return false;
			}
		}
		return true;
	}

	/** prints all the calculated indexes in the system.out
	 *  it is used to debug or test the class.
	 */
	public void printIndexes() {
		for (int i = 0; i < stations; i++) {
			System.out.println("Indexes " + name[i]);
			System.out.println("throughput        : " + throughput[i]);
			System.out.println("utilization       : " + utilization[i]);
			System.out.println("mean queue length : " + queueLen[i]);
			System.out.println();
		}
	}

	/*
	OLD

	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(getClass().getName());
		buf.append("\n-------------------------");
		for (int i = 0; i < stations; i++) {
			buf.append("\nIndexes " + name[i]);
			buf.append("\nthroughput          : " + throughput[i]);
			buf.append("\nutilization         : " + utilization[i]);
			buf.append("\nmean queue length   : " + queueLen[i]);
			buf.append("\nmean residence time : " + residenceTime[i]);
		}
		buf.append("\n\nsystem throughput    : " + totThroughput);
		buf.append("\nsystem response time : " + totRespTime);
		buf.append("\nsystem users : " + totUser);
		return buf.toString();
	}
	*/
	/** generates a string with all the calculated indexes.
	 * @return the string
	 */

	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append("\n------------------------------------");
		buf.append("\nAnalysis with MVA Singleclass");

		buf.append("\n\n");
		buf.append("Queue Length (Q)");
		buf.append("\n------------------------------------\n");
		for (int m = 0; m < stations; m++) {
			buf.append("[   ");
			buf.append(Printer.print(queueLen[m], 6));
			buf.append("   ]\n");
		}

		buf.append("\n\n");
		buf.append("Residence Time (R)");
		buf.append("\n------------------------------------\n");
		for (int m = 0; m < stations; m++) {
			buf.append("[   ");
			buf.append(Printer.print(residenceTime[m], 6));
			buf.append("   ]\n");
		}

		buf.append("\n\n");
		buf.append("Throughput (X)");
		buf.append("\n------------------------------------\n");
		for (int m = 0; m < stations; m++) {
			buf.append("[   ");
			buf.append(Printer.print(throughput[m], 6));
			buf.append("   ]\n");
		}

		buf.append("\n\n");
		buf.append("Utilization (U)");
		buf.append("\n------------------------------------\n");
		for (int m = 0; m < stations; m++) {
			buf.append("[   ");
			buf.append(Printer.print(utilization[m], 6));
			buf.append("   ]\n");
		}
		return buf.toString();
	}

	/** returns the throughput of the system
	 *  @return the throughput
	 */
	public double getTotThroughput() {
		return totThroughput;
	}

	/** returns the throughput for the requested station
	 *  @param  serv the number of the service center
	 *  @return the throughput
	 */
	public double getThroughput(int serv) {
		return throughput[serv];
	}

	/** returns the throughput of each service center
	 *  @return the throughput
	 */
	public double[] getThroughput() {
		return throughput;
	}

	/** returns the utilization of each service center
	 *  @return the utilization
	 */
	public double[] getUtilization() {
		return utilization;
	}

	/** returns the utilization for the requested station
	 *  @param  serv the number of the service center
	 *  @return the utilization
	 */
	public double getUtilization(int serv) {
		return utilization[serv];
	}

	/** returns the queue length for the requested station
	 *  @param  serv the number of the service center
	 *  @return the queue length
	 */
	public double getQueueLen(int serv) {
		return queueLen[serv];
	}

	/** returns the queue length of each service center
	 *  @return the queue length
	 */
	public double[] getQueueLen() {
		return queueLen;
	}

	/** returns the residence time for the requested station
	 *  @param  serv the number of the service center
	 *  @return the residence time
	 */
	public double getResTime(int serv) {
		return residenceTime[serv];
	}

	/** return the residence time of each service center
	 *  @return the residence time
	 */
	public double[] getResTime() {
		return residenceTime;
	}

	/** returns the response time for the system
	 *  @return the residence time
	 */
	public double getTotResTime() {
		return totRespTime;
	}

	/**
	 * returns the mean number of customers in the system
	 * @return
	 */
	public double getTotUser() {
		return totUser;
	}

	//NEW
	//@author Stefano Omini
	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of such a load.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 */

	public abstract boolean hasSufficientProcessingCapacity();

	//end NEW

	/**
	 * Must be implemented to create a single class model solver.
	 */
	public abstract void solve();

}
