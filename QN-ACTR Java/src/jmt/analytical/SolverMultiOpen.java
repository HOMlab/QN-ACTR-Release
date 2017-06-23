/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

import java.util.Arrays;

import jmt.engine.math.Erlang;

/**
 * Solves a multiclass open model (LD stations are not allowed).
 * @author Federico Granata, Stefano Omini, Bertoli Marco
 */
public class SolverMultiOpen extends SolverMulti {

	private double[] lambda;

	/**
	 * Creates a SolverMultiOpen
	 * @param classes number of classes
	 * @param stations number of stations
	 * @param lambda array of arrival rates
	 */
	public SolverMultiOpen(int classes, int stations, double[] lambda) {
		this(classes, stations, lambda, null);
	}

	/**
	 * Creates a SolverMultiOpen with multiple server support
	 * @param classes number of classes
	 * @param stations number of stations
	 * @param lambda array of arrival rates
	 */
	public SolverMultiOpen(int classes, int stations, double[] lambda, int[] servers) {
		super(classes, stations);
		this.lambda = lambda;
		this.servers = servers;
		if (servers == null) {
			// Assumes one server for each station (obviously ignore delays)
			servers = new int[stations];
			Arrays.fill(servers, 1);
		}
	}

	/**
	* Solves the model, using an appropriate technique (LI or LD model).
	*/
	@Override
	public void solve() {
		//tests if all the resources, stations, are load independent
		boolean loadIndep = true;
		for (int i = 0; i < stations && loadIndep; i++) {
			if (type[i] == LD) {
				loadIndep = false;
			}
		}

		if (loadIndep) {
			solveLI();
		} else {
			solveLD();
		}
	}

	/**
	 * Solves a model with only delay or load independent stations.
	 */
	private void solveLI() {
		for (int i = 0; i < classes; i++) {
			//throughput of class i
			clsThroughput[i] = lambda[i];
			for (int j = 0; j < stations; j++) {
				//throughput of class i for station j
				throughput[j][i] = lambda[i] * visits[j][i];
				//utilization of class i for station j
				// TODO: now utilization in JMT is expressed for each server. This quantity should be divided for number of servers
				utilization[j][i] = lambda[i] * visits[j][i] * servTime[j][i][0];
				//aggregate utilization for station j
				scUtilization[j] += utilization[j][i];

				//mean number of jobs of class i in the system
				clsNumJobs[i] += queueLen[j][i];
			}
			//mean number of jobs in the system
			sysNumJobs += clsNumJobs[i];
		}

		for (int i = 0; i < classes; i++) {
			for (int j = 0; j < stations; j++) {
				if (type[j] == Solver.DELAY) {
					//residence time of class i in station j
					residenceTime[j][i] = visits[j][i] * servTime[j][i][0];
				} else
				//residence time of class i in station j
				if (servers[j] == 1) {
					// Single server (distiontion is done for speed purposes only)
					residenceTime[j][i] = visits[j][i] * servTime[j][i][0] / (1 - scUtilization[j]);
				} else {
					// Multiple server (uses Erlang-C)
					residenceTime[j][i] = visits[j][i] * servTime[j][i][0]
							* (1 + (Erlang.erlangC(scUtilization[j], servers[j]) / (servers[j] - scUtilization[j])));
				}
				//queue length of class i for station j
				queueLen[j][i] = residenceTime[j][i] * lambda[i];
				//aggregate response time for class i
				clsRespTime[i] += residenceTime[j][i];

			}
		}

		for (int j = 0; j < stations; j++) {
			for (int i = 0; i < classes; i++) {
				//aggregate throughput for station j
				scThroughput[j] += throughput[j][i];
				//aggregate queue length for station j
				scQueueLen[j] += queueLen[j][i];

				//aggregate residence time for station j
				scResidTime[j] += residenceTime[j][i];
			}
			//system response time
			sysResponseTime += scResidTime[j];
		}
		//system throughput
		sysThroughput = sysNumJobs / sysResponseTime;
	}

	/**
	 * Solves a model with load dependent stations too.
	 */
	private void solveLD() {

		//TODO: still to be implemented.
		return;
	}

	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of the combined loads
	 * of all the classes.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 */
	@Override
	public boolean hasSufficientProcessingCapacity() {

		//the maximum aggregate utilization between all the stations must be < 1
		//otherwise the system has no sufficient processing capacity
		for (int j = 0; j < stations; j++) {

			if (type[j] == SolverMulti.DELAY) {
				//delay station: don't check saturation
				continue;
			}

			//utiliz is the aggregate utilization for station j
			double utiliz = 0;
			for (int i = 0; i < classes; i++) {
				utiliz += lambda[i] * visits[j][i] * servTime[j][i][0];
			}
			if (utiliz >= servers[j]) {
				return false;
			}
		}
		//there are no stations with aggregate utilization >= number of servers
		return true;
	}
}
