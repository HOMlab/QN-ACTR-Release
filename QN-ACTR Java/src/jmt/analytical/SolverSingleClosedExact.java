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

/*
 * SolverSingleClosedExact.java
 *
 * Created on 27 aprile 2002, 10.31
 */

package jmt.analytical;

import java.io.PrintWriter;

/**
 *  Solves a single class closed model, using the normalization constant
 *  algorithm. It also use a preordering algorithm to obtain a stable network.
 *
 * @author  Federico Granata
 *
 */
public class SolverSingleClosedExact extends Solver {

	/**
	 * number of customers or jobs in the system
	 */
	protected int customers = 0;

	/**
	 * normalization constant
	 */
	protected double[] G;

	/**
	 * computes the auxiliary function of the system
	 */
	protected double[] auxFun;

	/* used in dinamic Scaling, now disabled */
	//	private ArrayList scaleFact = new ArrayList();
	/**
	 * Creates a new SolverSingleClosedExact
	 *  @param  stations number of service centers (stations)
	 *  @param  customers number of customers
	 */
	public SolverSingleClosedExact(int customers, int stations) {

		this.customers = customers;
		this.stations = stations;

		name = new String[stations];
		type = new int[stations];
		//one service time for each possible population (from 1 to customers)
		//position 0 is used for LI stations
		servTime = new double[stations][customers + 1];
		visits = new double[stations];

		throughput = new double[stations];
		queueLen = new double[stations];
		utilization = new double[stations];
		residenceTime = new double[stations];
		G = new double[customers + 1];
		auxFun = new double[customers + 1];
	}

	/**
	 * Calculates the indexes of interest for the system.<br>
	 *  For a description of the algorithm see:<br>
	 *  <em>
	 *  S.C. Bruell, G. Balbo,<br>
	 * "Computational Algorithms for closed Queueing Networks",<br>
	 * 1980, Elsevier North Holland
	 * </em>
	 */
	public void indexes() {
		long start; // initial time.
		long end; // termination time.
		double FM = 1;
		double sum = 0;

		totUser = customers;
		System.out.println("Start parameters calculation.");
		start = System.currentTimeMillis();
		/* calculation for last station. we do it because the auxiliary
		 * function is calculated for free for this station in the norm const
		 * algorithm. */
		throughput[stations - 1] = visits[stations - 1] * G[customers - 1] / G[customers];

		if (type[stations - 1] == Solver.LI) {
			utilization[stations - 1] = servTime[stations - 1][0] * throughput[stations - 1];
			queueLen[stations - 1] = 0;
			for (int j = 1; j <= customers; j++) {
				queueLen[stations - 1] = (servTime[stations - 1][0] * visits[stations - 1] * G[j - 1] / G[j]) * (1 + queueLen[stations - 1]);
			}

		} else {//LD station
			utilization[stations - 1] = 1 - (auxFun[customers] / G[customers]);
			queueLen[stations - 1] = queueLen[stations - 1] / G[customers];
		}
		residenceTime[stations - 1] = queueLen[stations - 1] / throughput[stations - 1];
		totRespTime = residenceTime[stations - 1];

		/* index calculation for all other stations */
		for (int i = (stations - 2); i >= 0; i--) {
			throughput[i] = visits[i] * G[customers - 1] / G[customers];
			if (type[i] == Solver.LI) {
				utilization[i] = servTime[i][0] * throughput[i];
				queueLen[i] = 0;
				for (int j = 1; j <= customers; j++) {
					queueLen[i] = (servTime[i][0] * visits[i] * G[j - 1] / G[j]) * (1 + queueLen[i]);
				}
			} else {//LD station
				auxFun = calcAuxFunc(i);
				utilization[i] = 1 - (auxFun[customers] / G[customers]);
				FM = 1;
				sum = 0;
				for (int n = 1; n < G.length; n++) {
					FM = FM * visits[i] * servTime[i][n];
					sum += n * FM * auxFun[customers - n] / G[customers];
				}
				queueLen[i] = sum;
			}
			residenceTime[i] = queueLen[i] / throughput[i];
			totRespTime += residenceTime[i];
		}
		totThroughput = customers / totRespTime;

		/* Generate output */
		System.out.println("End of parameters calculation.");
		end = System.currentTimeMillis();
		System.out.println("Time elapsed in milliseconds : " + (end - start));
		return;
	}

	/** Calculates the auxiliary function needful to calculate marginal
	 * probabilities
	 * @param cent service center for which the auxiliary function is calculated.
	 * @return an array of all auxilialiary function for every valid population.
	 */
	private double[] calcAuxFunc(int cent) {
		double[] temp = new double[customers + 1];
		double sum = 0;
		double[] FM = new double[customers + 1];

		FM[0] = 1;
		for (int i = 1; i < FM.length; i++) {
			FM[i] = visits[cent] * FM[i - 1] * servTime[cent][i];
		}

		temp[0] = 1;
		for (int i = 1; i <= customers; i++) {
			if (type[cent] == Solver.LI) {
				temp[i] = G[customers] - (visits[cent] * servTime[cent][i] * G[customers - 1]);
			} else {
				sum = 0;
				for (int k = 1; k <= i; k++) {
					sum += FM[k] * temp[i - k];
				}
				temp[i] = G[i] - sum;
			}
		}
		return temp;
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
	@Override
	public boolean hasSufficientProcessingCapacity() {
		//closed class: no saturation problem
		return true;
	}

	//end NEW

	/**
	 * Solves the system throught the normalization constant algorithm.
	 *  For a description of the algorithm see:
	 *  <em>
	 *  S.C. Bruell, G. Balbo,<br>
	 * "Computational Algorithms for closed Queueing Networks",<br>
	 * 1980, Elsevier North Holland
	 * </em>
	 *
	 */
	@Override
	public void solve() {
		PrintWriter pw = new PrintWriter(System.out, true);
		double Y = 0;
		double[] FM = new double[customers + 1];
		double temp;
		double sum = 0;
		//		double MAX = Double.parseDouble("1.0e100");
		//		double MIN = Double.parseDouble("1.0e-100");
		long start; // used in time elapsed calculating
		long end; // used in time elapsed calculating
		int center = 0; // center index
		int cust = 0; // customer index
		int auxCust = 0; // an other customer index

		start = System.currentTimeMillis();
		pw.println("solving");
		staticScale();

		/* Start calculation of Norm Const*/
		/* first service center */
		G[0] = 1;

		if (type[0] == Solver.LI) {
			Y = visits[0] * servTime[0][0];
			for (int n = 1; n < G.length; n++) {
				G[n] = Y * G[n - 1];
			}
			//pw.println("G of center 0 : " + G[customers]);
		}

		if (type[0] == Solver.LD) {
			for (int n = 1; n < G.length; n++) {
				G[n] = visits[0] * G[n - 1] * servTime[0][n];

			}
			//pw.println("G of center 0 : " + G[customers]);
		}

		/* all others service center */
		for (center = 1; center < stations; center++) {
			if (type[center] == Solver.LI) {
				Y = visits[center] * servTime[center][0];
				for (cust = 1; cust < G.length; cust++) {
					G[cust] = G[cust] + Y * G[cust - 1];
				}
				//pw.println(" G of center " + center + " : " + G[customers]);
			}

			if (type[center] == Solver.LD) {
				FM[0] = 1;
				Y = visits[center];
				for (cust = 1; cust < G.length; cust++) {
					FM[cust] = FM[cust - 1] * Y * servTime[center][cust];
				}
				//FM[cust] = FM[cust - 1] * Y / servTime[center][cust];
				for (cust = (G.length - 1); cust > 0; cust--) {
					sum = 0;
					//sum = G[cust];
					for (auxCust = 0; auxCust <= cust; auxCust++) {
						if ((center == stations - 1) && (cust == G.length - 1)) {
							/* this is used in calculation of queueLen for last
							 * station */
							temp = FM[auxCust] * G[cust - auxCust];
							sum += temp;
							queueLen[stations - 1] += temp * auxCust;
						} else {
							sum += FM[auxCust] * G[cust - auxCust];
						}
					}
					G[cust] = sum;
				}
				//pw.println("G of center " + center + " : " + G[customers]);
			}
			/* this is used in calculation of queueLen for last station */
			if (center == stations - 2) {
				System.arraycopy(G, 0, auxFun, 0, (customers + 1));
			}
		}
		pw.println("End solving");
		end = System.currentTimeMillis();
		pw.println("Time elapsed in milliseconds : " + (end - start));
		return;
	}

	/**
	* Static scaling to control magnitude of G. It's not optimal, and in
	* some situations it does not overcome overflow problem, but generally
	* it's enough */
	private void staticScale() {
		int center = 0;
		int cust = 0;
		double sum = 0;
		double scalCons = 0;
		double Y = 0;
		int max = 0;

		for (center = 0; center < stations; center++) {
			if (type[center] == Solver.LI) {
				Y = (visits[center] * servTime[center][0]);
			} else {// load dependent
				sum = 0;
				for (cust = 1; cust < servTime[center].length; cust++) {
					Y = visits[center] * servTime[center][cust];
					sum += Y;
				}
				Y = sum / cust--;
			}
			if (Y >= scalCons) {
				scalCons = Y;
				max = center;
			}
		}
		for (int i = 0; i < stations; i++) {
			visits[i] = visits[i] / scalCons;
		}
		reorder(max);
	}

	private void reorder(int max) {
		String n = name[0];
		name[0] = name[max];
		name[max] = n;

		double[] s = new double[customers + 1];
		System.arraycopy(servTime[0], 0, s, 0, s.length);
		System.arraycopy(servTime[max], 0, servTime[0], 0, s.length);
		System.arraycopy(s, 0, servTime[max], 0, s.length);

		int t = type[0];
		type[0] = type[max];
		type[max] = t;

		//		double v = visits[0];
		visits[0] = visits[max];
		visits[max] = visits[0];
	}

}
