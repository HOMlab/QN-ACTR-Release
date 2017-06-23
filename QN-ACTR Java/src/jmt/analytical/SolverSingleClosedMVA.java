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
 * SolverSingleClosedMVA.java
 * Created on 13 novembre 2002, 10.48
 */

package jmt.analytical;

import jmt.engine.math.Printer;

/**
 * Solves a single class closed model, using Aggregated Mean Value Analysis algorithm.
 * @author  Federico Granata, Stefano Omini
 */
public class SolverSingleClosedMVA extends Solver {

	public static final boolean DEBUG = false;

	//NEW
	//Now SolverSingleClosedMVA extends Solver
	//
	//@author Stefano Omini

	/**
	 * number of customers or jobs in the system
	 */
	protected int customers = 0;

	//end NEW

	//NEW
	//Intermediate results
	//@author stefano Omini

	/**
	 * Tells whether intermediate results have been computed or not
	 */
	protected boolean intermediate_results = false;

	/** array containing the throughput for each population and for each
	 * service center
	 * <br>[population][service center]
	 */
	protected double[][] interm_throughput;

	/** array containing the utilization for each population and for each
	 * service center
	 * <br>[population][service center]
	 */
	protected double[][] interm_utilization;

	/** array containing the queue length  for each population and for each
	 * service center
	 * <br>[population][service center]
	 */
	protected double[][] interm_queueLen;

	/** array containing the residence time  for each population and for each
	 * service center (residence time = time spent in queue + time spent in service)
	 * <br>[population][service center]
	 */
	protected double[][] interm_residenceTime;

	/**  total throughput for each population */
	protected double[] interm_totThroughput;

	/**  total response time for each population*/
	protected double[] interm_totRespTime;

	/**  total number of users for each population*/
	protected double[] interm_totUser;

	//end NEW

	/* ------------------------
	   Class variables
	 * ------------------------ */

	private int[] position;//the original position of the station before the
	//reordering

	private int countLI; //number of LI & Delay stations

	/* ------------------------
	   Constructor
	 * ------------------------ */

	/**
	 * Creates a new instance of SolverSingleClosedMVA
	 * @param stat number of stations
	 * @param cust number of customers
	 */
	public SolverSingleClosedMVA(int cust, int stat) {
		//OLD
		//super(stat, cust);
		//NEW
		//old parameters were inverted!
		//@author Stefano Omini

		//OLD
		//Unuseful: no longer extends SolverSingleClosedExact
		//super(cust, stat);

		stations = stat;
		customers = cust;
		name = new String[stat];
		type = new int[stat];
		//one service time for each possible population (from 1 to customers)
		//position 0 is used for LI stations
		servTime = new double[stat][];
		visits = new double[stat];

		throughput = new double[stat];
		queueLen = new double[stat];
		utilization = new double[stat];
		residenceTime = new double[stat];
		position = new int[stat];

		//NEW
		//@author Stefano Omini

		//the structures with intermediate results are allocated only if
		//requested
		interm_throughput = null;
		interm_utilization = null;
		interm_queueLen = null;
		interm_residenceTime = null;

		interm_totThroughput = null;
		interm_totRespTime = null;
		interm_totUser = null;

		//end NEW

	}

	/* ------------------------
	   Methods: initialization
	 * ------------------------ */

	/** Initializes the solver with the system parameters.
	 * It must be called before trying to solve the model.
	 * <br>
	 * WARNING: This method changes the order of the stations (LI and DELAY before, LD after):
	 * the original order is restored only after calling the "solve" method
	 * (so if you solve the model, you don't need to reorder results,
	 * they are reordered by the "solve" method itself; otherwise, if you
	 * don't solve the model after calling "input" method, you may have some problems).
	 *  @param  n   array of names of service centers.
	 *  @param  t   array of the types (LD or LI) of service centers.
	 *  @param  s   matrix of service times of the service centers.
	 *  @param  v   array of visits to the service centers.
	 *  @return true if the operation is completed with success
	 */
	@Override
	public boolean input(String[] n, int[] t, double[][] s, double[] v) {
		if ((n.length > stations) || (t.length > stations) || (s.length > stations) || (v.length > stations)) {
			// wrong input.
			return false;
		}

		//number of not LD stations
		countLI = 0;
		for (int element : t) {
			if (element != Solver.LD) {
				countLI++;
			}
		}

		//OLD
		//if (!(countLI != 0 || countLI != t.length)) {
		//NEW
		//@author Stefano Omini
		if (!(countLI > 0 && countLI < t.length)) {
			//end NEW
			//TODO: controllare la nuova condizione
			//stations are either only LI or only LD
			for (int i = 0; i < stations; i++) {
				position[i] = i;
				//copy names, types and visits
				name[i] = n[i];
				type[i] = t[i];
				visits[i] = v[i];
				//copy service times
				if (type[i] == Solver.LD) {
					//LD
					//an array of customers+1 elements is created
					//the first element for LD center is set to 0)
					//the others are the values for each population
					servTime[i] = new double[customers + 1];
					if (s[i].length != customers + 1) {
						return false;
					}
					for (int j = 0; j <= customers; j++) {
						if (j == 0) {
							servTime[i][j] = 0;
						} else {
							servTime[i][j] = s[i][j];
						}
					}
				} else {
					//LI
					//only one element
					servTime[i] = new double[1];
					servTime[i][0] = s[i][0];
				}
			}
		} else {

			//there are both LI and LD stations: change the order
			//LI stations are put first (i.e. from position 0 to position countLI-1)
			//LD stations are put after all LI stations (i.e. from position count to the end)

			int countLD = 0;
			int countInserted = 0;

			for (int i = 0; i < stations; i++) {
				if (t[i] != Solver.LD) {
					//LI + Delay
					position[countInserted] = i;
					name[countInserted] = n[i];
					type[countInserted] = t[i];
					visits[countInserted] = v[i];
					servTime[countInserted] = new double[1];
					servTime[countInserted][0] = s[i][0];
					countInserted++;
				} else {
					//LD
					position[countLI + countLD] = i;
					name[countLI + countLD] = n[i];
					type[countLI + countLD] = t[i];
					visits[countLI + countLD] = v[i];

					servTime[countLI + countLD] = new double[customers + 1];
					if (s[i].length != customers + 1) {
						return false;
					}
					for (int j = 0; j <= customers; j++) {
						if (j == 0) {
							servTime[countLI + countLD][j] = 0;
						} else {
							servTime[countLI + countLD][j] = s[i][j];
						}
					}
					countLD++;
				}
			}

		}
		return true;
	}

	/* ------------------------
	   Methods to solve the model with MVA
	 * ------------------------ */

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

	/*
		public void indexes() {
	        //TODO: non implementato!!!!
		}
	    //overrides indexes() of SolverSingleClosedExact
	*/

	/**
	 *  Solves a single class closed model using MVA algorithm.<br>
	 *  "input(...)" method must have been called before solving the model!!<br><br>
	 *  Reference:<br>
	 * <em>
	 *  G.Balbo, S.C.Bruell, L.Cerchio, D.Chiaberto, L.Molinatti<br>
	 *  "Mean Value Analysis of Closed Load Dependent Networks"<br>
	 * </em>
	 */
	@Override
	public void solve() {
		//tests if all the resources, stations, are load independent
		boolean loadIndep = true;
		for (int i = 0; i < stations && loadIndep; i++) {
			if (type[i] == Solver.LD) {
				loadIndep = false;
			}
		}

		if (loadIndep) {
			solveSingleLI();
			//solveSingleLI_approx();
		} else {

			solveSingleLD();
			//solveSingleLDErrCtr();
			//solveSingleLDErrCtrFast();
			//solveSingleLDErrCtrFaster();
			//solveSingleLD2();
		}

		//"input" method has changed the order of the stations.
		//this method has to be called to restore the original order.
		reorder();

	}

	/**
	 * Solves a single class closed model using MVA algorithm.<br>
	 *
	 * "input(...)" method must have been called before solving the model!!<br><br>
	 *
	 * To solve a closed model with N customers, all the models with
	 * 0, 1, 2, ... N-1 customers must be solved before.
	 * This method allows to save the results for these intermediate models.
	 *
	 * Reference:<br>
	 * <em>
	 *  G.Balbo, S.C.Bruell, L.Cerchio, D.Chiaberto, L.Molinatti<br>
	 *  "Mean Value Analysis of Closed Load Dependent Networks"<br>
	 * </em>
	 *
	 * @param intermediate true to save results for intermediate populations
	 */
	public void solve(boolean intermediate) {
		//tests if all the stations are load independent
		boolean loadIndep = true;
		for (int i = 0; i < stations && loadIndep; i++) {
			if (type[i] == Solver.LD) {
				loadIndep = false;
			}
		}

		//this var is used by "toString" method: if true, also intermediate
		//results will be written
		intermediate_results = intermediate;

		if (loadIndep) {
			solveSingleLI(intermediate);
		} else {
			//TODO: aggiungere caso intermediate=true per LD (dopo averlo implementato!!)

			//TODO: debug
			solveSingleLD(intermediate);
			//solveSingleLDErrCtrFaster(intermediate);
		}

		reorder();

	}

	/** Solves a single class closed model with only LI stations */
	private void solveSingleLI() {
		int k;//index of station
		int n;//index of customer
		double sum;

		//service demands: D = visits * servTime
		double[] D = new double[stations];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < stations; k++) {
			queueLen[k] = 0.0;
			D[k] = servTime[k][0] * visits[k];//uses D = S * V to speed up
		}

		//calculation of throughput for the whole network and of queue length for each station
		//recursively starting from 0 customer in network to max customers
		for (n = 1; n <= customers; n++) {
			sum = 0.0;

			for (k = 0; k < stations; k++) {
				switch (type[k]) {
					case Solver.DELAY:
						residenceTime[k] = D[k];
						break;
					case Solver.LI:
						//recursion uses the queue length of the model with n-1 customers
						residenceTime[k] = D[k] * (1 + queueLen[k]);
						break;
					case Solver.LD://not supported use another method
						throw new IllegalArgumentException("error in solveSingleLI, type of service centers must be delay or load Independent");
				}//end case

				sum += residenceTime[k];
			}//end loop through stations

			//OLD
			//totThroughput = n / sum;
			//NEW
			//@author Stefano Omini
			if (sum == 0) {
				//D = 0 --> v = 0
				totThroughput = 0;
			} else {
				totThroughput = n / sum;
			}

			//end NEW

			for (k = 0; k < stations; k++) {
				queueLen[k] = totThroughput * residenceTime[k];
				//end loop through stations
			}

		}//end loop through customers

		//the other indexes dependent on totThroughput
		for (k = 0; k < stations; k++) {
			throughput[k] = totThroughput * visits[k];
			totRespTime = customers / totThroughput;
			utilization[k] = totThroughput * D[k];
		}//end loop through stations

	}//end of solveSingleLI

	/** Solves a single class closed model with only LI stations, using
	 * an approximated technique (which avoids recursion)
	 *
	 * @author Stefano Omini */
	//TODO: non funziona ancora tanto bene, in particolare alcuni valori di residence e queue sembrano poco precisi
	private void solveSingleLI_approx() {
		int k;//index of station
		int n;//index of customer
		double sum;

		double[] old_queueLen = new double[stations];

		//service demands: D = visits * servTime
		double[] D = new double[stations];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < stations; k++) {
			old_queueLen[k] = customers / stations;
			queueLen[k] = 0.0;
			D[k] = servTime[k][0] * visits[k];//uses D = S * V to speed up
		}

		//calculation of throughput for the whole network and of queue length for each station
		//recursively starting from 0 customer in network to max customers

		boolean finished = false; //true, when the specified precision has been reached

		do {
			sum = 0.0;

			for (k = 0; k < stations; k++) {
				switch (type[k]) {
					case Solver.DELAY:
						residenceTime[k] = D[k];
						break;
					case Solver.LI:
						//approximate technique doesn't use recursion!!
						residenceTime[k] = D[k] * customers * old_queueLen[k] / (customers - 1);
						;
						break;
				}//end case

				sum += residenceTime[k];
			}//end loop through stations

			totThroughput = customers / sum;
			int counter = 0;

			for (k = 0; k < stations; k++) {
				queueLen[k] = totThroughput * residenceTime[k];

				if ((Math.abs(queueLen[k] - old_queueLen[k])) < 0.0000001) {
					counter++;
				}
			}
			//end loop through stations

			if (counter == stations) {
				finished = true;
			}

			for (k = 0; k < stations; k++) {
				old_queueLen[k] = queueLen[k];
			}

		} while (!finished);

		//the other indexes dependent on totThroughput
		for (k = 0; k < stations; k++) {
			throughput[k] = totThroughput * visits[k];
			totRespTime = customers / totThroughput;
			utilization[k] = totThroughput * D[k];
		}//end loop through stations

	}//end of solveSingleLI

	//NEW
	//@author Stefano Omini

	/**
	 * Solves a single class closed model with only LI stations.
	 * To solve a closed model with N customers, all the models with
	 * 0, 1, 2, ... N-1 customers must be solved before.
	 * This method allows to save the results for these intermediate models.
	 * @param intermediate true to save results for intermediate populations
	 *
	 */
	private void solveSingleLI(boolean intermediate) {

		if (!intermediate) {
			//intermediate results mustn't be saved, then use
			//the base method
			solveSingleLI();
			return;
		}

		//create structures with intermediate results

		interm_throughput = new double[customers + 1][stations];
		interm_queueLen = new double[customers + 1][stations];
		interm_utilization = new double[customers + 1][stations];
		interm_residenceTime = new double[customers + 1][stations];

		interm_totThroughput = new double[customers + 1];
		interm_totRespTime = new double[customers + 1];
		interm_totUser = new double[customers + 1];

		int k;//index of station
		int n;//index of customer
		double sum;

		//service demands: D = visits * servTime
		double[] D = new double[stations];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < stations; k++) {

			//intermediate results: population = n = 0;
			interm_queueLen[0][k] = 0.0;
			interm_residenceTime[0][k] = 0.0;
			interm_throughput[0][k] = 0.0;
			interm_utilization[0][k] = 0.0;

			interm_totThroughput[0] = 0;
			interm_totRespTime[0] = 0;
			interm_totUser[0] = 0;

			D[k] = servTime[k][0] * visits[k];//uses D = S * V to speed up
		}

		//calculation of throughput for the whole network and of queue length for each station
		//recursively starting from 0 customer in network to max customers
		//intermediate results are saved
		for (n = 1; n <= customers; n++) {
			sum = 0.0;

			for (k = 0; k < stations; k++) {
				switch (type[k]) {
					case Solver.DELAY:
						interm_residenceTime[n][k] = D[k];
						break;
					case Solver.LI:
						//recursion uses the queue length of the model with n-1 customers
						interm_residenceTime[n][k] = D[k] * (1 + interm_queueLen[n - 1][k]);
						break;
					case Solver.LD://not supported use another method
						throw new IllegalArgumentException("error in solveSingleLI, type of service centers must be delay or load Independent");
				}//end case

				sum += interm_residenceTime[n][k];
			}//end loop through stations

			interm_totThroughput[n] = n / sum;

			interm_totUser[n] = 0;

			for (k = 0; k < stations; k++) {
				interm_queueLen[n][k] = interm_totThroughput[n] * interm_residenceTime[n][k];
				interm_throughput[n][k] = interm_totThroughput[n] * visits[k];
				interm_utilization[n][k] = interm_totThroughput[n] * D[k];

				interm_totRespTime[n] = customers / interm_totThroughput[n];
				interm_totUser[n] += interm_queueLen[n][k]; //of course must be equals to n
			}
			//end loop through stations

		}//end loop through customers

		//copy final results from last intermediate results (n=customers)
		//into the usual structure
		for (k = 0; k < stations; k++) {

			queueLen[k] = interm_queueLen[customers][k];
			residenceTime[k] = interm_residenceTime[customers][k];
			throughput[k] = interm_throughput[customers][k];
			utilization[k] = interm_utilization[customers][k];
		}
		totThroughput = interm_totThroughput[customers];
		totRespTime = interm_totRespTime[customers];
		totUser = interm_totUser[customers];

	}//end of solveSingleLI

	//end NEW
	//@author Stefano Omini

	/**
	 * Solves a model with LI and LD stations
	 */
	private void solveSingleLD() {
		int stat;//index of station
		int cust;//index of customer
		int k;//index of customers, in marginal probability
		//		int sum;//internal variable for summations
		double[] X = new double[customers + 1];//throughput of subsystem

		//queue length vectors of precedent subsystem
		double[][] nsPrec = new double[stations][customers + 1];
		//queue length vectors of actual subsystem
		double[][] nsCorr = new double[stations][customers + 1];
		//utilization vectors of precedent subsystem
		double[][] usPrec = new double[stations][customers + 1];
		//utilization vectors of actual subsystem
		double[][] usCorr = new double[stations][customers + 1];

		//maginal probability of precedent subsystem
		double[] pPrec = new double[customers + 1];
		//marginal probability of current subsystem
		double[] pCorr = new double[customers + 1];

		double[] Y = new double[customers + 1];// 1/throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time
		//		double prod;//internal variable

		/*      OLD

				//loop over all stations, transform all stations in load dependent
				//it's not efficient must be changed
				for (stat = 0; stat < stations; stat++) {
					if (type[stat] != Solver.LD) {
						wcomp = servTime[stat][0];
						if (type[stat] == Solver.DELAY) {
							for (cust = 1; cust <= customers; cust++)
								//TODO: attenzione, qui va in out of bound, non c'è un elemento x ogni popolazione!
		                        servTime[stat][cust] = wcomp / cust;
						} else {
							for (cust = 1; cust <= customers; cust++)
								servTime[stat][cust] = servTime[stat][0];
						}
					}
				}//end loop over all stations

		*/

		//NEW
		//@author Stefano Omini
		//loop over all stations, transform all stations in load dependent
		//it's not efficient must be changed
		for (stat = 0; stat < stations; stat++) {
			if (type[stat] != Solver.LD) {
				wcomp = servTime[stat][0];
				//create an element for each population n
				servTime[stat] = new double[customers + 1];

				if (type[stat] == Solver.DELAY) {
					servTime[stat][0] = wcomp;
					for (cust = 1; cust <= customers; cust++) {
						servTime[stat][cust] = wcomp / cust;
					}
				} else {
					for (cust = 0; cust <= customers; cust++) {
						servTime[stat][cust] = wcomp;
					}
				}
			}
		}//end loop over all stations
		//end NEW

		//initialization
		for (cust = 1; cust <= customers; cust++) {
			X[cust] = 1 / (servTime[0][cust]);
			nsPrec[0][cust] = cust;
			usPrec[0][cust] = 1;
		}//end initialization

		//loop over all stations except the first, it merges a station at every
		//pass when all the stations are merged, the system is solved
		for (stat = 1; stat < stations; stat++) {
			pPrec[0] = 1.0;
			for (cust = 1; cust <= customers; cust++) {
				//Y[cust] = X[cust] * visits[stat]/visits[stat-1];
				Y[cust] = visits[stat - 1] / (X[cust] * visits[stat]);
			}

			//loop oover all customers
			for (cust = 1; cust <= customers; cust++) {
				ws = 0.0;
				wcomp = 0.0;
				for (k = 1; k <= cust; k++) {
					ws += k * servTime[stat][k] * pPrec[k - 1];
					//wcomp += k * pPrec[cust-k] / Y[k];
					wcomp += k * pPrec[cust - k] * Y[k];
				}

				X[cust] = cust / (ws + wcomp);
				//computation of all marginal probabilities of the subsystem
				nsPrec[stat][cust] = X[cust] * ws;
				for (k = 1; k < cust; k++) {
					if (pPrec[k - 1] == 0) {
						//pCorr[k] = pPrec[k] * X[cust] / Y[cust-k];
						pCorr[k] = pPrec[k] * X[cust] * Y[cust - k];
					} else {
						pCorr[k] = servTime[stat][k] * X[cust] * pPrec[k - 1];
					}
				}
				pCorr[cust] = servTime[stat][cust] * X[cust] * pPrec[cust - 1];
				//pCorr[0] = pPrec[0] * X[cust] / Y[cust];
				pCorr[0] = pPrec[0] * X[cust] * Y[cust];
				//end computation of marginal probabilities

				//computation of utilization & queque length
				usPrec[stat][cust] = 0;
				for (k = 1; k <= cust; k++) {
					usPrec[stat][cust] += pCorr[k];
				}

				//loop over all inserted stations
				for (int i = 0; i < stat; i++) {
					nsCorr[i][cust] = 0;
					usCorr[i][cust] = 0;
					for (k = 1; k <= cust; k++) {
						nsCorr[i][cust] += pCorr[cust - k] * nsPrec[i][k];
						usCorr[i][cust] += pCorr[cust - k] * usPrec[i][k];
					}
				}//end loop over all inserted stations
				//end calculation of utilization & queque length

				System.arraycopy(pCorr, 0, pPrec, 0, cust + 1);
			}//end of loop over all customers

			//adjourns the utilization & queue length precedent vectors
			for (int c = 0; c < stat; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}
		}//end loop over all stations

		//calculation of other parameters
		totThroughput = X[customers] / visits[stations - 1];
		totRespTime = customers / totThroughput;
		//loop over all stations
		for (stat = 0; stat < stations; stat++) {
			throughput[stat] = totThroughput * visits[stat];
			if (type[stat] == Solver.DELAY) {
				//OLD
				//utilization[stat] = visits[stat] * servTime[stat][0];
				utilization[stat] = usPrec[stat][customers];
				//queueLen[stat] = 0;
				queueLen[stat] = nsPrec[stat][customers];

				residenceTime[stat] = servTime[stat][0];
			} else {
				utilization[stat] = usPrec[stat][customers];
				queueLen[stat] = nsPrec[stat][customers];
				residenceTime[stat] = queueLen[stat] / totThroughput;
			}
		}//end loop over all stations

	}//end solveSingleLD

	/**
	 * Solves a model with LI and LD stations
	 *
	 */
	private void solveSingleLD(boolean intermediate) {

		//TODO: ancora da sistemare!!!
		if (!intermediate) {
			//intermediate results mustn't be saved, then use
			//the base method
			solveSingleLD();
			return;
		}

		//create structures with intermediate results

		interm_throughput = new double[customers + 1][stations];
		interm_queueLen = new double[customers + 1][stations];
		interm_utilization = new double[customers + 1][stations];
		interm_residenceTime = new double[customers + 1][stations];

		interm_totThroughput = new double[customers + 1];
		interm_totRespTime = new double[customers + 1];
		interm_totUser = new double[customers + 1];

		int stat;//index of station
		int n;//index of customer
		int k;//index of customers, in marginal probability
		//		int sum;//internal variable for summations
		double[] X = new double[customers + 1];//throughput of subsystem

		//queue length vectors of precedent subsystem
		double[][] nsPrec = new double[stations][customers + 1];
		//queue length vectors of actual subsystem
		double[][] nsCorr = new double[stations][customers + 1];
		//utilization vectors of precedent subsystem
		double[][] usPrec = new double[stations][customers + 1];
		//utilization vectors of actual subsystem
		double[][] usCorr = new double[stations][customers + 1];

		//maginal probability of precedent subsystem
		double[] pPrec = new double[customers + 1];
		//marginal probability of current subsystem
		double[] pCorr = new double[customers + 1];

		double[] Y = new double[customers + 1];// 1/throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time

		//loop over all stations, transform all stations in load dependent
		//it's not efficient must be changed
		for (stat = 0; stat < stations; stat++) {
			if (type[stat] != Solver.LD) {
				wcomp = servTime[stat][0];
				//create an element for each population n
				servTime[stat] = new double[customers + 1];

				if (type[stat] == Solver.DELAY) {
					servTime[stat][0] = wcomp;
					for (n = 1; n <= customers; n++) {
						//OLD
						//servTime[stat][n] = wcomp / n;
						servTime[stat][n] = wcomp;
					}
				} else {
					for (n = 0; n <= customers; n++) {
						servTime[stat][n] = wcomp;
					}
				}
			}
		}//end loop over all stations

		//initialization
		for (n = 1; n <= customers; n++) {
			X[n] = 1 / (servTime[0][n]);
			nsPrec[0][n] = n;
			usPrec[0][n] = 1;
		}//end initialization

		//loop over all stations except the first, it merges a station at every
		//pass when all the stations are merged, the system is solved
		for (stat = 1; stat < stations; stat++) {
			pPrec[0] = 1.0;
			for (n = 1; n <= customers; n++) {
				//Y[n] = X[n] * visits[stat]/visits[stat-1];
				Y[n] = visits[stat - 1] / (X[n] * visits[stat]);
			}

			//loop oover all customers
			for (n = 1; n <= customers; n++) {
				ws = 0.0;
				wcomp = 0.0;
				for (k = 1; k <= n; k++) {
					ws += k * servTime[stat][k] * pPrec[k - 1];
					//wcomp += k * pPrec[n-k] / Y[k];
					wcomp += k * pPrec[n - k] * Y[k];
				}

				X[n] = n / (ws + wcomp);
				//computation of all marginal probabilities of the subsystem
				nsPrec[stat][n] = X[n] * ws;
				for (k = 1; k < n; k++) {
					if (pPrec[k - 1] == 0) {
						//pCorr[k] = pPrec[k] * X[n] / Y[n-k];
						pCorr[k] = pPrec[k] * X[n] * Y[n - k];
					} else {
						pCorr[k] = servTime[stat][k] * X[n] * pPrec[k - 1];
					}
				}
				pCorr[n] = servTime[stat][n] * X[n] * pPrec[n - 1];
				//pCorr[0] = pPrec[0] * X[n] / Y[n];
				pCorr[0] = pPrec[0] * X[n] * Y[n];
				//end computation of marginal probabilities

				//computation of utilization & queque length
				usPrec[stat][n] = 0;
				for (k = 1; k <= n; k++) {
					usPrec[stat][n] += pCorr[k];
				}

				//loop over all inserted stations
				for (int i = 0; i < stat; i++) {
					nsCorr[i][n] = 0;
					usCorr[i][n] = 0;
					for (k = 1; k <= n; k++) {
						nsCorr[i][n] += pCorr[n - k] * nsPrec[i][k];
						usCorr[i][n] += pCorr[n - k] * usPrec[i][k];
					}
				}//end loop over all inserted stations
				//end calculation of utilization & queque length

				System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
			}//end of loop over all customers

			//adjourns the utilization & queue length precedent vectors
			for (int c = 0; c < stat; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}
		}//end loop over all stations

		//calculation of other parameters
		totThroughput = X[customers] / visits[stations - 1];
		totRespTime = customers / totThroughput;
		//loop over all stations
		for (stat = 0; stat < stations; stat++) {
			throughput[stat] = totThroughput * visits[stat];
			if (type[stat] == Solver.DELAY) {

				//utilization[stat] = visits[stat] * servTime[stat][0];
				utilization[stat] = usPrec[stat][customers];
				//queueLen[stat] = 0;
				queueLen[stat] = nsPrec[stat][customers];
				residenceTime[stat] = servTime[stat][0];
			} else {
				utilization[stat] = usPrec[stat][customers];
				queueLen[stat] = nsPrec[stat][customers];
				residenceTime[stat] = queueLen[stat] / totThroughput;
			}
		}//end loop over all stations

	}//end solveSingleLD

	/**
	 * Solves a model with LI and LD stations
	 */
	private void solveSingleLD2() {
		int m;//index of station
		int n;//index of customer
		int k;//index of customers, in marginal probability
		double sum;
		double[] X = new double[customers + 1];

		double[][] nsPrec;
		double[][] nsCorr;
		double[][] usPrec;
		double[][] usCorr;
		//double[][][] us = new double[stations+1][stations][customers+1];
		//double[][][] ns = new double[stations+1][stations][customers+1];

		double[] pPrec = new double[customers + 1];
		double[] pCorr = new double[customers + 1];
		//double[][] p = new double[customers+1][customers+1];

		double[] Y = new double[customers + 1];//throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time
		//		double prod;//internal variable
		int LICount = 0;
		double[] D;//D = visits * servTime
		boolean noLI = false;

		int zeroCount = 0;

		for (m = 0; m < stations; m++) {
			if (type[m] != Solver.LD) {
				LICount++;
			}
		}
		if (LICount == 0) {
			noLI = true;
		}
		D = new double[LICount];
		nsPrec = new double[stations][customers + 1];
		nsCorr = new double[stations][customers + 1];
		usPrec = new double[stations][customers + 1];
		usCorr = new double[stations][customers + 1];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < LICount; k++) {
			queueLen[k] = 0;
			D[k] = servTime[k][0] * visits[k];//D = S * V to speed up
		}//end loop through stations

		//calculation of throughput for all LI subnetwork, and queue length
		//recoursively starting from 0 customer in network to max customers
		if (!noLI) {
			for (n = 1; n <= customers; n++) {
				sum = 0;
				for (k = 0; k < LICount; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = D[k];
							break;
						case Solver.LI:
							residenceTime[k] = D[k] * (1 + queueLen[k]);
							break;
						case Solver.LD://not supported use another method
							throw new IllegalArgumentException("error in solveSingleLI, type of service centers must be delay or load Independent");
					}//end case

					sum += residenceTime[k];
				}//end loop through stations
				totThroughput = n / sum;
				X[n] = totThroughput;
				for (k = 0; k < stations; k++) {
					queueLen[k] = totThroughput * residenceTime[k];
					//end loop through stations
				}

			}//end loop through customers
		}

		//initialization
		for (n = 1; n <= customers; n++) {
			if (!noLI) {
				Y[n] = X[n] * visits[LICount];
				nsPrec[LICount - 1][n] = n;
				usPrec[LICount - 1][n] = 1;
			} else {
				X[n] = 1 / servTime[0][n];
				nsPrec[0][n] = n;
				usPrec[0][n] = 1;
			}

		}//end initialization

		//loop over all LD stations
		//for(m = LICount; m < stations; m++) {
		for (m = noLI ? 1 : LICount; m < stations; m++) {
			//p[0][0] = 1;
			pPrec[0] = 1;
			for (n = 1; n <= customers; n++) {
				//Y[n] = X[n] * visits[m]/visits[m-1];
				if (m != LICount) {
					Y[n] = X[n] * visits[m] / visits[m - 1];
				}
			}

			for (n = 1; n <= customers; n++) {
				ws = 0;
				wcomp = 0;
				for (k = 1; k <= n; k++) {
					//ws += k * servTime[m][k] * p[k-1][n-1];
					ws += k * servTime[m][k] * pPrec[k - 1];
					//wcomp += k * p[n-k][n-1] / Y[k];
					wcomp += k * pPrec[n - k] / Y[k];
				}

				X[n] = n / (ws + wcomp);

				//ns[m+1][m][n] = X[n] * ws;
				nsPrec[m][n] = X[n] * ws;
				sum = 0;
				zeroCount = 0;
				for (k = 1; k < n; k++) {
					//if (p[k-1][n-1] == 0)
					if (pPrec[k - 1] == 0) {
						//p[k][n] = p[k][n-1] * X[n] / Y[n-k];
						pCorr[k] = pPrec[k] * X[n] / Y[n - k];
						if (pPrec[k] == 0) {
							zeroCount++;
						}
					} else {
						//p[k][n] = servTime[m][k] * X[n] * p[k-1][n-1];
						pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1];
					}
					sum += pCorr[k];
				}
				//p[n][n] = servTime[m][n] * X[n] * p[n-1][n-1];
				pCorr[n] = servTime[m][n] * X[n] * pPrec[n - 1];
				sum += pCorr[n];
				//p[0][n] = p[0][n-1] * X[n] / Y[n];
				pCorr[0] = pPrec[0] * X[n] / Y[n];
				sum += pCorr[0];

				if (Math.abs(sum - 1) >= 0.01 && (n % 500 == 0 || n == customers)) {
					;
					//System.out.println(n+" ERROR   Sum of prob : "+sum+" zeros "+zeroCount);
				}

				//us[m+1][m][n] = 0;
				usPrec[m][n] = 0;
				for (k = 1; k <= n; k++) {
					//us[m+1][m][n]  += p[k][n];
					//us[m+1][m][n]  += pCorr[k];
					usPrec[m][n] += pCorr[k];
				}

				//for(int i=LICount; i < m; i++) {
				for (int i = noLI ? 0 : LICount; i < m; i++) {
					//ns[m+1][i][n] = 0;
					nsCorr[i][n] = 0;
					//us[m+1][i][n] = 0;
					usCorr[i][n] = 0;
					for (k = 1; k <= n; k++) {
						//ns[m+1][i][n] += p[n-k][n] * ns[m][i][k];
						//ns[m+1][i][n] += pCorr[n-k] * ns[m][i][k];
						nsCorr[i][n] += pCorr[n - k] * nsPrec[i][k];

						//us[m+1][i][n] += p[n-k][n] * us[m][i][k];
						//us[m+1][i][n] += pCorr[n-k] * us[m][i][k];
						usCorr[i][n] += pCorr[n - k] * usPrec[i][k];
					}
				}
				System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
			}//end of loop over all customers
			for (int c = LICount; c < m; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}
		}//end loop over all stations
		//System.arraycopy(nsPrec[stations-1], 0, nsCorr[stations-1], 0, customers +1);
		//System.arraycopy(usPrec[stations-1], 0, usCorr[stations-1], 0, customers +1);

		//calculation of other parameters
		totThroughput = X[customers] / visits[stations - 1];

		//for(m = LICount; m < stations; m++) {
		for (m = noLI ? 0 : LICount; m < stations; m++) {
			throughput[m] = totThroughput * visits[m];
			if (type[m] == Solver.DELAY) {
				utilization[m] = visits[m] * servTime[m][0];
			} else {
				//utilization[m] = us[stations][m][customers];
				utilization[m] = usPrec[m][customers];
			}
			//queueLen[m] = ns[stations][m][customers];
			queueLen[m] = nsPrec[m][customers];
			residenceTime[m] = queueLen[m] / totThroughput;
		}

		if (!noLI) {
			for (m = 0; m < LICount; m++) {
				throughput[m] = totThroughput * visits[m];
				utilization[m] = D[m] * totThroughput;
				queueLen[m] = 0;
				for (n = 1; n <= customers; n++) {
					if (type[m] == Solver.DELAY) {
						residenceTime[m] = D[m];
					} else {
						residenceTime[m] = D[m] * (1 + queueLen[m]);
					}
					queueLen[m] = (X[n] / visits[stations - 1]) * residenceTime[m];
				}
				residenceTime[m] = queueLen[m] / totThroughput;
			}
		}
		//System.out.println("used LD2");
	}

	/**
	 * Solves a model with LI and LD stations
	 */
	private void solveSingleLDErrCtr() {
		int m;//index of station
		int n;//index of customer
		int k;//index of customers, in marginal probability
		double sum = 0;
		double sumW;
		double sumWcomp;
		double wsPar;
		double wcompPar;

		double[] X = new double[customers + 1];
		double MIN = 1.0E-290;
		double MAX = 1.0E290;

		int min = 0;
		int[] expMultPrec = new int[customers + 1];
		int[] expMultCorr = new int[customers + 1];

		double[][] nsPrec;
		double[][] nsCorr;
		double[][] usPrec;
		double[][] usCorr;

		double[] pPrec = new double[customers + 1];
		double[] pCorr = new double[customers + 1];

		double[] Y = new double[customers + 1];//throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time
		double prod;//internal variable
		int LICount = 0;
		double[] D;//D = visits * servTime
		boolean noLI = false;

		int zeroCount = 0;
		int firstLD = -1;

		for (m = 0; m < stations; m++) {
			if (type[m] != Solver.LD) {
				LICount++;

			} else if (firstLD == -1) {
				firstLD = m;
			}
		}
		if (LICount == 0) {
			noLI = true;
		}
		D = new double[LICount];
		//DEK (Federico Granata) 23-09-2003
		//		nsPrec = new double[stations][customers + 1];
		//		nsCorr = new double[stations][customers + 1];
		//		usPrec = new double[stations][customers + 1];
		//		usCorr = new double[stations][customers + 1];
		nsPrec = new double[stations - LICount + 1][customers + 1];
		nsCorr = new double[stations - LICount + 1][customers + 1];
		usPrec = new double[stations - LICount + 1][customers + 1];
		usCorr = new double[stations - LICount + 1][customers + 1];

		//22-09-2003 DEK (Federico Granata)
		/*//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < LICount; k++) {
			queueLen[k] = 0;
			D[k] = servTime[k][0] * visits[k];//D = S * V to speed up
		}//end loop through stations*/
		//put all queue length to 0 for n = 0 customers in the network
		int counter = 0;
		for (k = 0; k < stations; k++) {
			if (type[k] != Solver.LD) {
				queueLen[k] = 0;
				D[counter] = servTime[k][0] * visits[k];//D = S * V to speed up
				counter++;
			}
		}//end loop through stations*/

		//calculation of throughput for all LI subnetwork, and queue length
		//recoursively starting from 0 customer in network to max customers
		if (!noLI) {
			for (n = 1; n <= customers; n++) {
				sum = 0;
				//22-09-2003 DEK (Federico Granata)
				/*for (k = 0; k < LICount; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = D[k];
							break;
						case Solver.LI:
							residenceTime[k] = D[k] * (1 + queueLen[k]);
							break;
						case Solver.LD://not supported use another method
							throw new IllegalArgumentException("error in solveSingleLI, type of service centers must be delay or load Independent");
					}//end case

					sum += residenceTime[k];
				}//end loop through stations*/
				counter = 0;
				for (k = 0; k < stations; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = D[counter];
							sum += residenceTime[k];
							counter++;
							break;
						case Solver.LI:
							residenceTime[k] = D[counter] * (1 + queueLen[k]);
							sum += residenceTime[k];
							counter++;
							break;
						case Solver.LD://not supported use another method
							break;
					}//end case

				}
				totThroughput = n / sum;
				X[n] = totThroughput;
				for (k = 0; k < stations; k++) {
					if (type[k] != Solver.LD) {
						queueLen[k] = totThroughput * residenceTime[k];
						//end loop through stations
					}
				}

				//DEBUG
				/*System.out.println("n = " + n);
				for (int i = 0; i < residenceTime.length; i++) {
					System.out.print(residenceTime[i]+ "  ");
				}
				System.out.println();
				for (int i = 0; i < queueLen.length; i++) {
					System.out.print(queueLen[i]+ "  ");
				}
				System.out.println();
				System.out.println();*/

			}//end loop through customers
		}

		//initialization
		for (n = 1; n <= customers; n++) {
			if (!noLI) {
				//DEK (Federico Granata) 23-9-2003
				//				Y[n] = 1 / (X[n] * visits[LICount]);
				Y[n] = 1 / (X[n] * visits[firstLD]);
				//				nsPrec[LICount - 1][n] = n;
				//				usPrec[LICount - 1][n] = 1;
				nsPrec[0][n] = n;
				usPrec[0][n] = 1;

			} else {
				X[n] = 1 / servTime[0][n];
				//				nsPrec[0][n] = n;
				//				usPrec[0][n] = 1;
				nsPrec[1][n] = n;
				usPrec[1][n] = 1;
			}

			//DEBUG
			//			System.out.println("n = " + n);
			//			System.out.print("Y = ");
			//			for (int i = 0; i < Y.length; i++) {
			//				System.out.print(Y[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			//			System.out.print("X = ");
			//			for (int i = 0; i < X.length; i++) {
			//				System.out.print(X[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			//			for (int i = 0; i < nsPrec.length; i++) {
			//				for(int j = 0; j < nsPrec[i].length; j++) {
			//				System.out.print(nsPrec[i][j]+" ");
			//				}
			//				System.out.println();
			//			}
			//			System.out.println();
			//			for (int i = 0; i < usPrec.length; i++) {
			//				for(int j = 0; j < usPrec[i].length; j++) {
			//				System.out.print(usPrec[i][j]+" ");
			//				}
			//				System.out.println();
			//			}
			//			System.out.println();
			//
		}//end initialization

		//loop over all LD stations
		//i have to use 2 counters to make it work correctly
		//1 to loop over all stations, 1 to loop over the arrays that
		//are avaliable only fot load dependent stations.

		//		for (m = noLI ? 1 : LICount; m < stations; m++) {
		counter = 1;
		int lastLD = firstLD;
		for (m = 1; m < stations; m++) {
			if (type[m] == Solver.LD) {
				pPrec[0] = 1;
				expMultPrec[0] = 0;
				for (n = 1; n <= customers; n++) {
					//DEK (Federico Granata) 23-09-2003
					//					if (m != LICount)
					if (m != firstLD) {
						Y[n] = visits[lastLD] / (X[n] * visits[m]);
						/*System.out.println("m = " + m);
						System.out.println("n = " + n);
						System.out.print("Y = ");
						for (int i = 0; i < Y.length; i++) {
							System.out.print(Y[i] + " ");

						}
						System.out.println();
						System.out.println();*/
					}
				}

				for (n = 1; n <= customers; n++) {
					//calculation of waiting time at station m an of the aggregated m-1
					//stations.
					ws = 0;
					wcomp = 0;
					for (int c = min; c < 0; c++) {
						sumW = 0;
						sumWcomp = 0;
						wsPar = 0;
						wcompPar = 0;
						for (k = 1; k <= n; k++) {
							if (expMultPrec[k - 1] == c) {
								sumW += k * servTime[m][k] * pPrec[k - 1];
							}
							if (sumW > MAX) {
								wsPar += sumW * MIN;
								sumW = 0;
							}
							if (expMultPrec[n - k] == c) {
								sumWcomp += k * pPrec[n - k] * Y[k];
							}
							if (sumWcomp > MAX) {
								wcompPar += sumWcomp * MIN;
								sumWcomp = 0;
							}
						}
						ws = (ws + sumW) * MIN + wsPar;
						wcomp = (wcomp + sumWcomp) * MIN + wcompPar;
					}

					for (k = 1; k <= n; k++) {
						if (expMultPrec[k - 1] == 0) {
							ws += k * servTime[m][k] * pPrec[k - 1];
						}
						if (expMultPrec[n - k] == 0) {
							wcomp += k * pPrec[n - k] * Y[k];
						}
					}

					X[n] = n / (ws + wcomp);

					//					nsPrec[m][n] = X[n] * ws;
					//					System.out.println("counter = " + counter);
					//					System.out.println("m = " + m);
					nsPrec[counter][n] = X[n] * ws;
					//calculation of marginal probabilities
					for (k = 1; k <= n; k++) {
						pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1];
						expMultCorr[k] = expMultPrec[k - 1];
						if (pCorr[k] < MIN) {
							pCorr[k] = servTime[m][k] * X[n] * (pPrec[k - 1] * MAX);
							expMultCorr[k]--;
							if (expMultCorr[k] < min) {
								min = expMultCorr[k];
							}
						} else if (pCorr[k] > MAX) {
							pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1] * MIN;
							expMultCorr[k]++;
						}
					}

					pCorr[0] = pPrec[0] * X[n] * Y[n];

					expMultCorr[0] = expMultPrec[0];
					if (pCorr[0] < MIN) {
						pCorr[0] = (pPrec[0] * MAX) * X[n] * Y[n];
						expMultCorr[0]--;
						if (expMultCorr[0] < min) {
							min = expMultCorr[0];
						}
					} else if (pCorr[0] > MAX) {
						pCorr[0] = pPrec[0] * X[n] * MIN * Y[n];
						expMultCorr[0]++;
					}

					//DEK (Federico Granata)
					//				usPrec[m][n] = 0;
					usPrec[counter][n] = 0;
					for (int c = min; c < 0; c++) {
						for (k = 1; k <= n; k++) {
							if (expMultCorr[k] == c) {
								//DEK (Federico Granata)
								//							usPrec[m][n] += pCorr[k];
								usPrec[counter][n] += pCorr[k];
							}
						}
						//DEK (Federico Granata)
						//					usPrec[m][n] *= MIN;
						usPrec[counter][n] *= MIN;
					}

					for (k = 1; k <= n; k++) {
						if (expMultCorr[k] == 0) {
							//DEK (Federico Granata)
							//						usPrec[m][n] += pCorr[k];
							usPrec[counter][n] += pCorr[k];
						}
					}

					//if(Math.abs(usPrec[m][n]+(pCorr[0]*Math.pow(MIN,
					//- expMultCorr[0]) )- 1) > 0.0000000000001)
					//    System.out.println(n+" ERROR   Sum of prob : "
					//    +(usPrec[m][n]+(pCorr[0]*Math.pow(MIN,- expMultCorr[0]) ))
					//    +" zeros "+zeroCount);

					//DEK (Federico Granata)
					//				for (int i = noLI ? 0 : LICount; i < m; i++) {
					int cStat = 1;
					for (int i = 0; i < m; i++) {

						if (type[i] == Solver.LD) {
							nsCorr[cStat][n] = 0;
							usCorr[cStat][n] = 0;
							for (int c = min; c < 0; c++) {
								for (k = 1; k <= n; k++) {
									if (expMultCorr[n - k] == c) {
										nsCorr[cStat][n] += pCorr[n - k] * nsPrec[cStat][k];
										usCorr[cStat][n] += pCorr[n - k] * usPrec[cStat][k];
									}
								}

								nsCorr[cStat][n] *= MIN;
								usCorr[cStat][n] *= MIN;
							}

							for (k = 1; k <= n; k++) {
								if (expMultCorr[n - k] == 0) {
									nsCorr[cStat][n] += pCorr[n - k] * nsPrec[cStat][k];
									usCorr[cStat][n] += pCorr[n - k] * usPrec[cStat][k];
								}

							}
							cStat++;
						}

					}

					System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
					System.arraycopy(expMultCorr, 0, expMultPrec, 0, n + 1);

					//DEBUG
					/*	System.out.println("m = " + m);
					System.out.println("n = " + n);
					System.out.print("Y = ");
					for (int i = 0; i < Y.length; i++) {
					System.out.print(Y[i]+" ");

					}
					System.out.println();
					System.out.println();
					System.out.print("X = ");
					for (int i = 0; i < X.length; i++) {
					System.out.print(X[i]+" ");

					}
					System.out.println();
					System.out.println();
					for (int i = 0; i < nsCorr.length; i++) {
					for(int j = 0; j < nsCorr[i].length; j++) {
					System.out.print(nsCorr[i][j]+" ");
					}
					System.out.println();
					}
					System.out.println();
					for (int i = 0; i < usCorr.length; i++) {
					for(int j = 0; j < usCorr[i].length; j++) {
					System.out.print(usCorr[i][j]+" ");
					}
					System.out.println();
					}
					System.out.println();*/

				}//end of loop over all customers
				//DEBUG
				//					System.out.println("m = " + m);
				//			System.out.println("n = " + n);
				//			System.out.print("Y = ");
				//			for (int i = 0; i < Y.length; i++) {
				//				System.out.print(Y[i]+" ");
				//
				//			}
				//			System.out.println();
				//			System.out.println();
				//			System.out.print("X = ");
				//			for (int i = 0; i < X.length; i++) {
				//				System.out.print(X[i]+" ");
				//
				//			}
				//			System.out.println();
				//			System.out.println();
				//			for (int i = 0; i < nsCorr.length; i++) {
				//				for(int j = 0; j < nsCorr[i].length; j++) {
				//				System.out.print(nsCorr[i][j]+" ");
				//				}
				//				System.out.println();
				//			}
				//			System.out.println();
				//			for (int i = 0; i < usCorr.length; i++) {
				//				for(int j = 0; j < usCorr[i].length; j++) {
				//				System.out.print(usCorr[i][j]+" ");
				//				}
				//				System.out.println();
				//			}
				//			System.out.println();

				//DEK (Federico Granata) 23-09-2003
				//				for (int c = LICount; c < m; c++) {

				int cStat = 1;
				for (int c = 0; c < m; c++) {
					if (type[c] == Solver.LD) {
						System.arraycopy(nsCorr[cStat], 1, nsPrec[cStat], 1, customers);
						System.arraycopy(usCorr[cStat], 1, usPrec[cStat], 1, customers);
						cStat++;
					}
				}
				counter++;
				lastLD = m;
			}
		}//end loop over all stations

		//		DEBUG
		/*System.out.println("m = " + m);
			System.out.println("n = " + n);
			System.out.print("Y = ");
			for (int i = 0; i < Y.length; i++) {
				System.out.print(Y[i]+" ");

			}
			System.out.println();
			System.out.println();
			System.out.print("X = ");
			for (int i = 0; i < X.length; i++) {
				System.out.print(X[i]+" ");

			}
			System.out.println();
			System.out.println();
			for (int i = 0; i < nsPrec.length; i++) {
				for(int j = 0; j < nsPrec[i].length; j++) {
				System.out.print(nsPrec[i][j]+" ");
				}
				System.out.println();
			}
			System.out.println();
			for (int i = 0; i < usPrec.length; i++) {
				for(int j = 0; j < usPrec[i].length; j++) {
				System.out.print(usPrec[i][j]+" ");
				}
				System.out.println();
			}
			System.out.println();*/

		//calculation of other parameters
		totThroughput = X[customers] / visits[stations - 1];
		totRespTime = customers / totThroughput;

		//DEK (Federico Granata)
		//LD stations
		//		for (m = noLI ? 0 : LICount; m < stations; m++) {
		counter = 1;
		for (m = 0; m < stations; m++) {
			if (type[m] == Solver.LD) {
				throughput[m] = totThroughput * visits[m];
				//				if (type[m] == Solver.DELAY)
				//					utilization[m] = visits[m] * servTime[m][0];
				//				else
				//					utilization[m] = usPrec[m][customers];
				utilization[m] = usPrec[counter][customers];
				//				queueLen[m] = nsPrec[m][customers];
				queueLen[m] = nsPrec[counter][customers];
				residenceTime[m] = queueLen[m] / totThroughput;
				counter++;
			}
		}

		//LI stations
		counter = 0;
		if (!noLI) {
			//DEK (Federico Granata)
			//			for (m = 0; m < LICount; m++) {
			for (m = 0; m < stations; m++) {
				if (type[m] != Solver.LD) {
					throughput[m] = totThroughput * visits[m];
					utilization[m] = D[counter] * totThroughput;
					queueLen[m] = 0;
					for (n = 1; n <= customers; n++) {
						if (type[m] == Solver.DELAY) {
							residenceTime[m] = D[counter];
						} else {
							residenceTime[m] = D[counter] * (1 + queueLen[m]);
						}
						queueLen[m] = (X[n] / visits[stations - 1]) * residenceTime[m];
					}
					residenceTime[m] = queueLen[m] / totThroughput;
					counter++;
				}
			}
		}
		//System.out.println("used LDErrCtr");
	}

	/**
	 * Solves a model with LI and LD stations
	 */
	private void solveSingleLDErrCtrFast() {
		//index of station
		int m;
		//index of customer
		int n;
		//index of customers, in marginal probability
		int k;

		double sum = 0.0;
		double sumW;
		double sumWcomp;
		double wsPar;
		double wcompPar;

		//throughput of subsystem  (?)
		double[] X = new double[customers + 1];

		double MIN = 1.0E-290;
		double MAX = 1.0E290;

		int min = 0;

		byte[] expMultPrec = new byte[customers + 1];
		byte[] expMultCorr = new byte[customers + 1];
		int[][] puntProb = new int[128][];
		puntProb[0] = new int[customers + 2];
		int num;// number of elements in vector puntProb[c]
		int punt;//value of puntProb[c][k], "pointer" to a probability

		//queue length vectors of precedent subsystem
		double[][] nsPrec;
		//queue length vectors of actual subsystem
		double[][] nsCorr;
		//utilization vectors of precedent subsystem
		double[][] usPrec;
		//utilization vectors of actual subsystem
		double[][] usCorr;

		//maginal probability of precedent subsystem
		double[] pPrec = new double[customers + 1];
		//marginal probability of current subsystem
		double[] pCorr = new double[customers + 1];

		//throughput of composite system
		double[] Y = new double[customers + 1];

		//waiting time
		double ws;
		//composite waiting time
		double wcomp;

		//double prod;//internal variable       //NEVER USED

		//number of LI stations
		int LICount = 0;
		//D = visits * servTime
		double[] D;
		//true if there are no LI stations
		boolean noLI = false;

		//int zeroCount = 0;                    //NEVER USED

		//count the number of LI stations
		for (m = 0; m < stations; m++) {
			if (type[m] != Solver.LD) {
				LICount++;
			}
		}
		if (LICount == 0) {
			noLI = true;
		}

		//service demands of LI stations: D = S * V (used to speed up)
		D = new double[LICount];

		nsPrec = new double[stations][customers + 1];
		nsCorr = new double[stations][customers + 1];
		usPrec = new double[stations][customers + 1];
		usCorr = new double[stations][customers + 1];

		//put queue length = 0 for all LI stations, for n = 0 customers in the network
		for (k = 0; k < LICount; k++) {
			queueLen[k] = 0.0;
			D[k] = servTime[k][0] * visits[k];
		}//end loop through stations

		//calculation of throughput for all LI subnetwork, and queue length
		//recursively starting from 0 customer in network to max customers
		if (!noLI) {
			for (n = 1; n <= customers; n++) {
				sum = 0.0;

				for (k = 0; k < LICount; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = D[k];
							break;
						case Solver.LI:
							//recursion: uses queue length of the model with n-1 customers
							residenceTime[k] = D[k] * (1 + queueLen[k]);
							break;
						case Solver.LD:
							//not supported use another method
							throw new IllegalArgumentException("error in solveSingleLI, type of service centers must be delay or load Independent");
					}//end case

					sum += residenceTime[k];
				}//end loop through stations

				totThroughput = n / sum;
				X[n] = totThroughput;

				for (k = 0; k < stations; k++) {
					queueLen[k] = totThroughput * residenceTime[k];
					//end loop through stations
				}

			}//end loop through customers
		}

		//initialization
		for (n = 1; n <= customers; n++) {
			if (!noLI) {

				Y[n] = 1 / (X[n] * visits[LICount]);

				nsPrec[LICount - 1][n] = n;
				usPrec[LICount - 1][n] = 1;
			} else {
				X[n] = 1 / servTime[0][n];
				nsPrec[0][n] = n;
				usPrec[0][n] = 1;
			}

			//DEBUG
			//			System.out.println("n = " + n);
			//			System.out.print("Y = ");
			//			for (int i = 0; i < Y.length; i++) {
			//				System.out.print(Y[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			//			System.out.print("X = ");
			//			for (int i = 0; i < X.length; i++) {
			//				System.out.print(X[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			//			for (int i = 0; i < nsPrec.length; i++) {
			//				for(int j = 0; j < nsPrec[i].length; j++) {
			//				System.out.print(nsPrec[i][j]+" ");
			//				}
			//				System.out.println();
			//			}
			//			System.out.println();
			//			for (int i = 0; i < usPrec.length; i++) {
			//				for(int j = 0; j < usPrec[i].length; j++) {
			//				System.out.print(usPrec[i][j]+" ");
			//				}
			//				System.out.println();
			//			}
			//			System.out.println();
		}//end initialization

		//loop over all LD stations

		//OLD
		//for (m = noLI ? 1 : LICount; m < stations; m++) {
		//TODO: se noLI è vero non dovrebbe partire da 0??

		//TODO: a meno che:: (COMMENTO tratto da solveSingleLD)
		//loop over all stations except the first, it merges a station at every
		//pass when all the stations are merged, the system is solved

		int m0 = noLI ? 1 : LICount;
		for (m = m0; m < stations; m++) {
			pPrec[0] = 1.0;
			expMultPrec[0] = 0;
			puntProb[0][0] = 1;
			puntProb[0][1] = 0;
			for (int c = 1; c <= min; c++) {
				puntProb[c][0] = 0;
			}

			if (m != LICount) {
				for (n = 1; n <= customers; n++) {
					Y[n] = visits[m - 1] / (X[n] * visits[m]);
				}
			}

			//DEBUG
			/*System.out.println("m = " + m);
			System.out.println("n = " + n);
			System.out.print("Y = ");
			for (int i = 0; i < Y.length; i++) {
				System.out.print(Y[i] + " ");
			}
			System.out.println();
			System.out.println();*/

			for (n = 1; n <= customers; n++) {
				//calculation of waiting time at station m an of the aggregated m-1
				//stations.
				ws = 0.0;
				wcomp = 0.0;
				//min is the index of the smallest probability values
				for (int c = min; c > 0; c--) {
					sumW = 0.0;
					wsPar = 0.0;
					sumWcomp = 0.0;
					wcompPar = 0.0;
					num = puntProb[c][0];
					for (k = 1; k <= num; k++) {
						punt = puntProb[c][k];
						sumW += (punt + 1) * servTime[m][punt + 1] * pPrec[punt];
						if (sumW > MAX) {
							wsPar += sumW * MIN;
							sumW = 0.0;
						}
						sumWcomp += (n - punt) * pPrec[punt] * Y[n - punt];
						if (sumWcomp > MAX) {
							wcompPar += sumWcomp * MIN;
							sumWcomp = 0.0;
						}
					}

					ws = (ws + sumW) * MIN + wsPar;
					wcomp = (wcomp + sumWcomp) * MIN + wcompPar;
				}

				num = puntProb[0][0];
				for (k = 1; k <= num; k++) {
					punt = puntProb[0][k];
					ws += (punt + 1) * servTime[m][punt + 1] * pPrec[punt];
					wcomp += (n - punt) * pPrec[punt] * Y[n - punt];
				}
				//throughput of aggregated subsystem
				X[n] = n / (ws + wcomp);

				nsPrec[m][n] = X[n] * ws;
				//rest of all "pointers"
				for (int c = 0; c <= min; c++) {
					puntProb[c][0] = 0;
				}

				//calculation of marginal probabilities p(k,n) except k = 0, n
				for (k = 1; k < n; k++) {
					pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1];
					expMultCorr[k] = expMultPrec[k - 1];
					if (pCorr[k] < MIN) {
						pCorr[k] *= MAX;
						expMultCorr[k]++;
						if (expMultCorr[k] > min) {
							min = expMultCorr[k];
							puntProb[expMultCorr[k]] = new int[customers + 2];
						}
					} else if (pCorr[k] > MAX) {
						pCorr[k] *= MIN;
						expMultCorr[k]--;
					}
					puntProb[expMultCorr[k]][0]++;
					puntProb[expMultCorr[k]][puntProb[expMultCorr[k]][0]] = k;
				}

				//calculation of p(n,n)
				pCorr[n] = servTime[m][n] * X[n] * pPrec[n - 1];
				expMultCorr[n] = expMultPrec[n - 1];
				if (pCorr[n] < MIN) {
					pCorr[n] *= MAX;
					expMultCorr[n]++;
				} else if (pCorr[n] > MAX) {
					pCorr[n] *= MIN;
					expMultCorr[n]--;
				}
				//p(n,n) it is calculated, but not inserted for now....

				//utilization of subsystem m at n
				usPrec[m][n] = 0;
				for (int c = min; c > 0; c--) {
					num = puntProb[c][0];
					for (k = 1; k <= num; k++) {
						usPrec[m][n] += pCorr[puntProb[c][k]];
					}
					if (expMultCorr[n] == c) {
						usPrec[m][n] += pCorr[n];
					}
					usPrec[m][n] *= MIN;
				}

				num = puntProb[0][0];
				for (k = 1; k <= num; k++) {
					usPrec[m][n] += pCorr[puntProb[0][k]];
				}
				if (expMultCorr[n] == 0) {
					usPrec[m][n] += pCorr[n];
				}

				//calculation of p(0,n)
				pCorr[0] = pPrec[0] * X[n] * Y[n];
				expMultCorr[0] = expMultPrec[0];
				if (pCorr[0] < MIN) {
					pCorr[0] *= MAX;
					expMultCorr[0]++;
					if (expMultCorr[0] > min) {
						min = expMultCorr[0];
						puntProb[min] = new int[customers + 2];
					}
				} else if (pCorr[0] > MAX) {
					pCorr[0] *= MIN;
					expMultCorr[0]--;
				}
				puntProb[expMultCorr[0]][0]++;
				puntProb[expMultCorr[0]][puntProb[expMultCorr[0]][0]] = 0;

				//test of correctness sum(p(k,n)) = 1. only for debug

				//if (Math.abs(usPrec[m][n] + (pCorr[0] * Math.pow(MIN, expMultCorr[0])) - 1) > 0.0000000000001)
				//System.out.println(n + " ERROR   Sum of prob : " + (usPrec[m][n] + (pCorr[0] * Math.pow(MIN, -expMultCorr[0]))));

				//calculation of utilization & queue lenght of al m subsystem
				//from n
				for (int i = noLI ? 0 : LICount; i < m; i++) {
					nsCorr[i][n] = 0.0;
					usCorr[i][n] = 0.0;
					for (int c = min; c > 0; c--) {
						num = puntProb[c][0];
						for (k = 1; k <= num; k++) {
							nsCorr[i][n] += pCorr[puntProb[c][k]] * nsPrec[i][n - puntProb[c][k]];
							usCorr[i][n] += pCorr[puntProb[c][k]] * usPrec[i][n - puntProb[c][k]];
						}

						if (nsCorr[i][n] > MAX) {
							new Error("overflow at" + n);
						}

						nsCorr[i][n] *= MIN;
						usCorr[i][n] *= MIN;
					}

					num = puntProb[0][0];
					for (k = 1; k <= num; k++) {
						nsCorr[i][n] += pCorr[puntProb[0][k]] * nsPrec[i][n - puntProb[0][k]];
						usCorr[i][n] += pCorr[puntProb[0][k]] * usPrec[i][n - puntProb[0][k]];
					}

				}

				if (expMultCorr[n] > min) {
					min = expMultCorr[n];
					puntProb[min] = new int[customers + 2];
				}

				//add p(n,n)
				puntProb[expMultCorr[n]][0]++;
				puntProb[expMultCorr[n]][puntProb[expMultCorr[n]][0]] = n;

				System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
				System.arraycopy(expMultCorr, 0, expMultPrec, 0, n + 1);

				//DEBUG
				/*	System.out.println("m = " + m);
				System.out.println("n = " + n);
				System.out.print("Y = ");
				for (int i = 0; i < Y.length; i++) {
					System.out.print(Y[i] + " ");

				}
				System.out.println();
				System.out.println();
				System.out.print("X = ");
				for (int i = 0; i < X.length; i++) {
					System.out.print(X[i] + " ");

				}
				System.out.println();
				System.out.println();
				for (int i = 0; i < nsPrec.length; i++) {
					for (int j = 0; j < nsPrec[i].length; j++) {
						System.out.print(nsPrec[i][j] + " ");
					}
					System.out.println();
				}
				System.out.println();
				for (int i = 0; i < usPrec.length; i++) {
					for (int j = 0; j < usPrec[i].length; j++) {
						System.out.print(usPrec[i][j] + " ");
					}
					System.out.println();
				}
				System.out.println();*/

			}//end of loop over all customers
			//DEBUG
			//System.out.println("m = " + m);
			//System.out.println("n = " + n);

			//			System.out.print("Y = ");
			//			for (int i = 0; i < Y.length; i++) {
			//				System.out.print(Y[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			//			System.out.print("X = ");
			//			for (int i = 0; i < X.length; i++) {
			//				System.out.print(X[i]+" ");
			//
			//			}
			//			System.out.println();
			//			System.out.println();
			for (double[] element : nsCorr) {
				for (int j = 0; j < element.length; j++) {
					//System.out.print(nsCorr[i][j] + " ");
				}
				//System.out.println();
			}
			//System.out.println();
			for (double[] element : usCorr) {
				for (int j = 0; j < element.length; j++) {
					//System.out.print(usCorr[i][j] + " ");
				}
				//System.out.println();
			}
			//System.out.println();

			for (int c = LICount; c < m; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}

		}//end loop over all stations

		//DEBUG
		//System.out.println("m = " + m);
		//System.out.println("n = " + n);
		//System.out.print("Y = ");
		for (double element : Y) {
			//System.out.print(Y[i] + " ");

		}
		//System.out.println();
		//System.out.println();
		//System.out.print("X = ");
		for (double element : X) {
			//System.out.print(X[i] + " ");

		}
		//System.out.println();
		//System.out.println();
		for (double[] element : nsPrec) {
			for (int j = 0; j < element.length; j++) {
				//System.out.print(nsPrec[i][j] + " ");
			}
			//System.out.println();
		}
		//System.out.println();
		for (double[] element : usPrec) {
			for (int j = 0; j < element.length; j++) {
				//System.out.print(usPrec[i][j] + " ");
			}
			//System.out.println();
		}
		//System.out.println();

		//calculation of other parameters
		//TODO: perchè moltiplica per visits[stations-1] ??
		totThroughput = X[customers] / visits[stations - 1];
		for (m = noLI ? 0 : LICount; m < stations; m++) {
			throughput[m] = totThroughput * visits[m];
			if (type[m] == Solver.DELAY) {
				utilization[m] = visits[m] * servTime[m][0];
			} else {
				utilization[m] = usPrec[m][customers];
			}
			queueLen[m] = nsPrec[m][customers];
			residenceTime[m] = queueLen[m] / totThroughput;
		}

		if (!noLI) {
			for (m = 0; m < LICount; m++) {
				throughput[m] = totThroughput * visits[m];
				utilization[m] = D[m] * totThroughput;
				queueLen[m] = 0.0;
				for (n = 1; n <= customers; n++) {
					if (type[m] == Solver.DELAY) {
						residenceTime[m] = D[m];
					} else {
						residenceTime[m] = D[m] * (1 + queueLen[m]);
					}
					queueLen[m] = (X[n] / visits[stations - 1]) * residenceTime[m];
				}
				residenceTime[m] = queueLen[m] / totThroughput;
			}
		}
		//System.out.println("used LDErrCtrFast");
	}

	/**
	 * Solves a model with LI and LD stations
	 */
	//TODO: by DEK (Federico Granata) ti sei impestato nel metodo meno testato, e piu' incasinato! complimenti
	//e' il meno testato perche' Serazzi non mi e' mai stato convito ad usare un metodo la cui stabilita'
	//non era nota e nemmeno un bound sull'errore..
	//inoltre non e' vero che sia piu' veloce lo e' solo se le probabilita' significative sono poche.
	//e' poco testato perche' non ho avuto granche tempo di lavorarci sopra visto che non ero autorizzato.
	//e' inoltre poco ottimizzato (rispetto a quello che si puo' fare in quanto c'era poco tempo)
	// saprei spiegarti molto meglio il metoto Fast, sicuramente stabile.
	//comunque l'idea che sta dietro a questo deriva da quello sopra, in questo caso dico approx perche'
	// non tutti gli elemnti vengono sommati ad ogni passo. il motivo e' semplice non servono,
	//sommare elemnti molto molto piccoli ad uno grande e' inutile. questo metodo fa esattamente questo.
	private void solveSingleLDErrCtrFaster() {
		int m;//index of station
		int n;//index of customer
		int k;//index of customers, in marginal probability
		double sum = 0.0;

		//TODO: questo dovrebbero essere il throughput: quale differenza c'è tra X e Y??
		//R: non hai guardato la bibliografia? l'articolo di balbo bruell????
		//si X e Y sono il throughput ma calcolato in maniera diversa, mi spiego X e' quello
		//classico da Lazowska, Y invece vience calcolato mano a mano ponendo 1 alla volta il tempo di
		//servizio del nodo pari a 0
		double[] X = new double[customers + 1];
		double MIN = 1.0E-290;
		double MAX = 1.0E290;

		//TODO: che cosa sono puntProb e expMult?
		//R1: expMult e' il esponente che serve come nei metodi che vedi sopra per tenere
		//conto delle grandi oscillazioni delle probabilita' in pratica devo avere un array grande
		//N+1 customers che contine il vaolre dell-esponente del moltiplicatore di ogni probabilita'
		//anche questo problema e' nella mia tesi... quello che succede e' che nei sistemi LD le probabilita'
		//mentre si risolve il sistema oscillano tra valori molto piccoli (dipende dai parametri dei sistema
		//ma possono arrivare a 10^-1000 )e vaolri significativi (10^/2) ovvimanete un semplice double non farebbe
		//altro che trasformare il numero in 0 e quindi perdere l'informazione connessa a quella probabilita'.
		//il risultato e' un errore anche molto consistente nella soluzione.
		//
		//R2:puntProb, e' un "puntatore" cioe' punta al valore
		//puntProb[0] contiene il numero di elemnti usati di puntProb, per evitare di farsi tutto il giro inutile dell'array
		//ad ogni passo. tiene traccia degli elemnti significativi nel sistema. se uan probabilita' e'
		//troppo piccola non verra' contata.
		int min = 0;
		byte[] expMultPrec = new byte[customers + 1];
		byte[] expMultCorr = new byte[customers + 1];
		int[] puntProb = new int[customers + 2];

		int num;// number of elemnts in vector puntProb[c]

		double[][] nsPrec = new double[stations][customers + 1];
		double[][] nsCorr = new double[stations][customers + 1];
		double[][] usPrec = new double[stations][customers + 1];
		double[][] usCorr = new double[stations][customers + 1];

		//TODO: queste dovrebbero essere probabilità marginali di avere j customer nalla stazione (con o<j<n)
		double[] pPrec = new double[customers + 1];
		double[] pCorr = new double[customers + 1];

		double[] Y = new double[customers + 1];//throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time

		int LICount = 0;//number of LI and DELAY centers
		double[] D;//Service demands: D = visits * servTime
		boolean noLI = false;//true if there are only LD centers

		//count the number of LI and DELAY stations
		for (m = 0; m < stations; m++) {
			if (type[m] != Solver.LD) {
				LICount++;
			}
		}
		if (LICount == 0) {
			noLI = true;//all centers are LD
		}
		D = new double[LICount];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < LICount; k++) {
			queueLen[k] = 0.0;
			D[k] = servTime[k][0] * visits[k];//D = S * V to speed up
		}//end loop through stations

		//now computes throughput and queue length for all LI subnetwork,
		//recoursively starting from 0 customer in network to max customers
		//later all the LI subnetworks will be transformed into a single LD station: service time for n customers
		//is equal to response time of the whole LI subnetwork with n customers
		//then the LD stations will be merged one at each time

		if (!noLI) {
			for (n = 1; n <= customers; n++) {
				sum = 0.0;
				for (k = 0; k < LICount; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = D[k];
							break;
						case Solver.LI:
							residenceTime[k] = D[k] * (1 + queueLen[k]);
							break;
					}//end case

					sum += residenceTime[k];
				}//end loop through stations
				totThroughput = n / sum;
				X[n] = totThroughput;
				for (k = 0; k < stations; k++) {
					queueLen[k] = totThroughput * residenceTime[k];
					//end loop through stations
				}

			}//end loop through customers
		}

		//initialization
		for (n = 1; n <= customers; n++) {
			if (!noLI) {
				Y[n] = 1 / (X[n] * visits[LICount]);

				nsPrec[LICount - 1][n] = n;
				usPrec[LICount - 1][n] = 1;
			} else {
				X[n] = 1 / servTime[0][n];
				nsPrec[0][n] = n;
				usPrec[0][n] = 1;
			}

		}//end initialization

		//loop over all LD stations (except the first which does not require to be
		//short-circuited (see Balbo-Bruell)
		for (m = noLI ? 1 : LICount; m < stations; m++) {
			pPrec[0] = 1.0;
			expMultPrec[0] = 0;
			puntProb[0] = 1;
			puntProb[1] = 0;

			if (m != LICount) {
				for (n = 1; n <= customers; n++) {
					Y[n] = visits[m - 1] / (X[n] * visits[m]);
				}
			}

			for (n = 1; n <= customers; n++) {
				//calculation of waiting time of station m and of the aggregated m-1
				//stations.
				ws = 0.0;
				wcomp = 0.0;

				num = puntProb[0];
				for (k = 1; k <= num; k++) {
					ws += (puntProb[k] + 1) * servTime[m][puntProb[k] + 1] * pPrec[puntProb[k]];
					wcomp += (n - puntProb[k]) * pPrec[puntProb[k]] * Y[n - puntProb[k]];
				}
				//throughput of aggregated subsystem
				X[n] = n / (ws + wcomp);

				//R:esatto
				nsPrec[m][n] = X[n] * ws;
				puntProb[0] = 0;

				//calculation of marginal probabilities p(k,n) except k = 0, n
				for (k = 1; k < n; k++) {
					pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1];
					//TODO: exp ?? Forse serve per probabilità troppo basse??
					//R: hai aganciato il concetto
					expMultCorr[k] = expMultPrec[k - 1];
					if (pCorr[k] < MIN) {
						pCorr[k] *= MAX;
						expMultCorr[k]++;
						if (expMultCorr[k] > min) {
							min = expMultCorr[k];
						}
						//TODO: idem per prob alte??
						//R: esatto pero' cosidera che alte vuole dire tenendo conto del moltiplicatore...
					} else if (pCorr[k] > MAX) {
						pCorr[k] *= MIN;
						expMultCorr[k]--;
					}
					//TODO: punto non chiaro: cosa succede??
					//R: quando il moltiplicatore per la prob k-esima e' 0 aumento il numero
					if (expMultCorr[k] == 0) {
						puntProb[0]++;
						puntProb[puntProb[0]] = k;
					}
				}

				//calculation of p(n,n)
				pCorr[n] = servTime[m][n] * X[n] * pPrec[n - 1];
				expMultCorr[n] = expMultPrec[n - 1];
				if (pCorr[n] < MIN) {
					pCorr[n] *= MAX;
					expMultCorr[n]++;
				} else if (pCorr[n] > MAX) {
					pCorr[n] *= MIN;
					expMultCorr[n]--;
				}
				//p(n,n) it is calculated, but not inserted for now....

				//initialize utilization of subsystem m at n
				usPrec[m][n] = 0;

				num = puntProb[0];
				for (k = 1; k <= num; k++) {
					usPrec[m][n] += pCorr[puntProb[k]];
				}
				if (expMultCorr[n] == 0) {
					usPrec[m][n] += pCorr[n];
				}

				//calculation of p(0,n)
				pCorr[0] = pPrec[0] * X[n] * Y[n];
				expMultCorr[0] = expMultPrec[0];
				if (pCorr[0] < MIN) {
					pCorr[0] *= MAX;
					expMultCorr[0]++;
					if (expMultCorr[0] > min) {
						min = expMultCorr[0];
					}
				} else if (pCorr[0] > MAX) {
					pCorr[0] *= MIN;
					expMultCorr[0]--;
				}
				if (expMultCorr[0] == 0) {
					puntProb[0]++;
					puntProb[puntProb[0]] = 0;
				}

				//test of correctness sum(p(k,n)) = 1. only for debug
				//if(Math.abs(usPrec[m][n]+(pCorr[0]
				//*Math.pow(MIN, expMultCorr[0]))- 1) > 0.0000000000001)
				//    System.out.println(n+" ERROR   Sum of prob : "
				//    +(usPrec[m][n]+(pCorr[0]*Math.pow(MIN, expMultCorr[0]))));

				//calculation of utilization & queue lenght of al m subsystem
				//from n
				for (int i = noLI ? 0 : LICount; i < m; i++) {
					nsCorr[i][n] = 0.0;
					usCorr[i][n] = 0.0;

					num = puntProb[0];
					for (k = 1; k <= num; k++) {
						nsCorr[i][n] += pCorr[puntProb[k]] * nsPrec[i][n - puntProb[k]];
						usCorr[i][n] += pCorr[puntProb[k]] * usPrec[i][n - puntProb[k]];
					}

				}

				//add p(n,n)
				if (expMultCorr[n] > min) {
					min = expMultCorr[n];
				}
				if (expMultCorr[n] == 0) {
					puntProb[0]++;
					puntProb[puntProb[0]] = n;
				}

				System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
				System.arraycopy(expMultCorr, 0, expMultPrec, 0, n + 1);
			}//end of loop over all customers
			for (int c = LICount; c < m; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}
		}//end loop over all stations

		//calculation of other parameters
		totThroughput = X[customers] / visits[stations - 1];
		for (m = noLI ? 0 : LICount; m < stations; m++) {
			throughput[m] = totThroughput * visits[m];
			if (type[m] == Solver.DELAY) {
				utilization[m] = visits[m] * servTime[m][0];
			} else {
				utilization[m] = usPrec[m][customers];
			}
			queueLen[m] = nsPrec[m][customers];
			residenceTime[m] = queueLen[m] / totThroughput;
		}

		if (!noLI) {
			for (m = 0; m < LICount; m++) {
				throughput[m] = totThroughput * visits[m];
				utilization[m] = D[m] * totThroughput;
				queueLen[m] = 0.0;
				for (n = 1; n <= customers; n++) {
					if (type[m] == Solver.DELAY) {
						residenceTime[m] = D[m];
					} else {
						residenceTime[m] = D[m] * (1 + queueLen[m]);
					}
					queueLen[m] = (X[n] / visits[stations - 1]) * residenceTime[m];
				}
				residenceTime[m] = queueLen[m] / totThroughput;
			}
		}

		//NEW
		//@author Stefano Omini

		//other aggregate parameters
		totRespTime = totUser = 0;
		for (m = 0; m < stations; m++) {
			totRespTime += residenceTime[m];
			totUser += queueLen[m]; //of course should be equal to the number of customers
		}

		//end NEW

		//System.out.println("used LDErrCtrFaster");
	}

	/**
	 * Solves a model with LI and LD stations.
	 * To solve a closed model with N customers, all the models with
	 * 0, 1, 2, ... N-1 customers must be solved before.
	 * This method allows to save the results for these intermediate models.
	 * @param intermediate true to save results for intermediate populations
	 *
	 */
	private void solveSingleLDErrCtrFaster(boolean intermediate) {
		//TODO: ancora da sistemare!!! Non si hanno ancora i risultati intermedi...
		if (!intermediate) {
			//intermediate results mustn't be saved, then use
			//the base method
			solveSingleLDErrCtrFaster();
			return;
		}

		//create structures with intermediate results

		interm_throughput = new double[customers + 1][stations];
		interm_queueLen = new double[customers + 1][stations];
		interm_utilization = new double[customers + 1][stations];
		interm_residenceTime = new double[customers + 1][stations];

		interm_totThroughput = new double[customers + 1];
		interm_totRespTime = new double[customers + 1];
		interm_totUser = new double[customers + 1];

		int m;//index of station
		int n;//index of customer
		int k;//index of customers, in marginal probability
		double sum = 0.0;

		double[] X = new double[customers + 1];
		double MIN = 1.0E-290;
		double MAX = 1.0E290;

		int min = 0;
		byte[] expMultPrec = new byte[customers + 1];
		byte[] expMultCorr = new byte[customers + 1];
		int[] puntProb = new int[customers + 2];
		int num;// number of elements in vector puntProb[c]

		double[][] nsPrec;
		double[][] nsCorr;
		double[][] usPrec;
		double[][] usCorr;

		double[] pPrec = new double[customers + 1];
		double[] pCorr = new double[customers + 1];

		double[] Y = new double[customers + 1];//throughput of composite system
		double ws;//waiting time
		double wcomp;//composite waiting time
		int LICount = 0;
		double[] D;//D = visits * servTime
		boolean noLI = false;

		for (m = 0; m < stations; m++) {
			if (type[m] != Solver.LD) {
				LICount++;
			}
		}
		if (LICount == 0) {
			noLI = true; //there are only LD stations
		}
		D = new double[LICount];
		nsPrec = new double[stations][customers + 1];
		nsCorr = new double[stations][customers + 1];
		usPrec = new double[stations][customers + 1];
		usCorr = new double[stations][customers + 1];

		//put all queue length to 0 for n = 0 customers in the network
		for (k = 0; k < LICount; k++) {
			queueLen[k] = 0.0;

			//intermediate results: population = n = 0;
			interm_queueLen[0][k] = 0.0;
			interm_residenceTime[0][k] = 0.0;
			interm_throughput[0][k] = 0.0;
			interm_utilization[0][k] = 0.0;

			interm_totThroughput[0] = 0;
			interm_totRespTime[0] = 0;
			interm_totUser[0] = 0;

			D[k] = servTime[k][0] * visits[k];//D = S * V to speed up
		}//end loop through stations

		//calculation of throughput for all LI subnetwork, and queue length
		//recoursively starting from 0 customer in network to max customers

		if (!noLI) {
			//there are also LI stations
			for (n = 1; n <= customers; n++) {
				sum = 0.0;
				for (k = 0; k < LICount; k++) {
					switch (type[k]) {
						case Solver.DELAY:
							residenceTime[k] = interm_residenceTime[n][k] = D[k];
							break;
						case Solver.LI:
							residenceTime[k] = interm_residenceTime[n][k] = D[k] * (1 + interm_queueLen[n - 1][k]);
							break;
					}//end case

					sum += interm_residenceTime[n][k];
				}//end loop through stations
				totThroughput = interm_totThroughput[n] = n / sum;
				X[n] = interm_totThroughput[n];
				for (k = 0; k < stations; k++) {
					queueLen[k] = interm_queueLen[n][k] = interm_totThroughput[n] * interm_residenceTime[n][k];
				}
				//end loop through stations

			}//end loop through customers
		}

		//initialization
		for (n = 1; n <= customers; n++) {
			if (!noLI) {
				//throughput of composite system with population n
				Y[n] = 1 / (X[n] * visits[LICount]);

				nsPrec[LICount - 1][n] = n;
				usPrec[LICount - 1][n] = 1;
			} else {
				//throughput of composite system with population n
				X[n] = 1 / servTime[0][n];
				nsPrec[0][n] = n;
				usPrec[0][n] = 1;
			}

		}//end initialization

		//loop over all LD stations
		for (m = noLI ? 1 : LICount; m < stations; m++) {
			pPrec[0] = 1.0;
			expMultPrec[0] = 0;
			puntProb[0] = 1;
			puntProb[1] = 0;

			if (m != LICount) {
				for (n = 1; n <= customers; n++) {
					Y[n] = visits[m - 1] / (X[n] * visits[m]);
				}
			}

			for (n = 1; n <= customers; n++) {
				//calculation of waiting time at station m and of the aggregated m-1
				//stations.
				ws = 0.0;
				wcomp = 0.0;

				num = puntProb[0];
				for (k = 1; k <= num; k++) {
					ws += (puntProb[k] + 1) * servTime[m][puntProb[k] + 1] * pPrec[puntProb[k]];
					wcomp += (n - puntProb[k]) * pPrec[puntProb[k]] * Y[n - puntProb[k]];
				}

				//throughput of aggregated subsystem
				X[n] = n / (ws + wcomp);

				nsPrec[m][n] = X[n] * ws;
				puntProb[0] = 0;

				//calculation of marginal probabilities p(k,n) except k = 0, n
				for (k = 1; k < n; k++) {
					pCorr[k] = servTime[m][k] * X[n] * pPrec[k - 1];
					expMultCorr[k] = expMultPrec[k - 1];
					if (pCorr[k] < MIN) {
						pCorr[k] *= MAX;
						expMultCorr[k]++;
						if (expMultCorr[k] > min) {
							min = expMultCorr[k];
						}
					} else if (pCorr[k] > MAX) {
						pCorr[k] *= MIN;
						expMultCorr[k]--;
					}
					if (expMultCorr[k] == 0) {
						puntProb[0]++;
						puntProb[puntProb[0]] = k;
					}
				}

				//calculation of p(n,n)
				pCorr[n] = servTime[m][n] * X[n] * pPrec[n - 1];
				expMultCorr[n] = expMultPrec[n - 1];
				if (pCorr[n] < MIN) {
					pCorr[n] *= MAX;
					expMultCorr[n]++;
				} else if (pCorr[n] > MAX) {
					pCorr[n] *= MIN;
					expMultCorr[n]--;
				}
				//p(n,n) it is calculated, but not inserted for now....

				//utilization of subsystem m at n
				usPrec[m][n] = 0;

				num = puntProb[0];
				for (k = 1; k <= num; k++) {
					usPrec[m][n] += pCorr[puntProb[k]];
				}
				if (expMultCorr[n] == 0) {
					usPrec[m][n] += pCorr[n];
				}

				//calculation of p(0,n)
				pCorr[0] = pPrec[0] * X[n] * Y[n];
				expMultCorr[0] = expMultPrec[0];
				if (pCorr[0] < MIN) {
					pCorr[0] *= MAX;
					expMultCorr[0]++;
					if (expMultCorr[0] > min) {
						min = expMultCorr[0];
					}
				} else if (pCorr[0] > MAX) {
					pCorr[0] *= MIN;
					expMultCorr[0]--;
				}
				if (expMultCorr[0] == 0) {
					puntProb[0]++;
					puntProb[puntProb[0]] = 0;
				}

				//test of correctness sum(p(k,n)) = 1. only for debug
				//if(Math.abs(usPrec[m][n]+(pCorr[0]
				//*Math.pow(MIN, expMultCorr[0]))- 1) > 0.0000000000001)
				//    System.out.println(n+" ERROR   Sum of prob : "
				//    +(usPrec[m][n]+(pCorr[0]*Math.pow(MIN, expMultCorr[0]))));

				//calculation of utilization & queue lenght of al m subsystem
				//from n
				for (int i = noLI ? 0 : LICount; i < m; i++) {
					nsCorr[i][n] = interm_queueLen[n][i] = 0.0;
					usCorr[i][n] = interm_utilization[n][i] = 0.0;

					num = puntProb[0];
					for (k = 1; k <= num; k++) {
						nsCorr[i][n] = interm_queueLen[n][i] += pCorr[puntProb[k]] * nsPrec[i][n - puntProb[k]];
						usCorr[i][n] = interm_utilization[n][i] += pCorr[puntProb[k]] * usPrec[i][n - puntProb[k]];
					}

				}

				//add p(n,n)
				if (expMultCorr[n] > min) {
					min = expMultCorr[n];
				}
				if (expMultCorr[n] == 0) {
					puntProb[0]++;
					puntProb[puntProb[0]] = n;
				}

				System.arraycopy(pCorr, 0, pPrec, 0, n + 1);
				System.arraycopy(expMultCorr, 0, expMultPrec, 0, n + 1);
			}//end of loop over all customers
			for (int c = LICount; c < m; c++) {
				System.arraycopy(nsCorr[c], 1, nsPrec[c], 1, customers);
				System.arraycopy(usCorr[c], 1, usPrec[c], 1, customers);
			}
		}//end loop over all stations

		/*
		//calculation of other parameters
		totThroughput = interm_totThroughput[customers] = X[customers] / visits[stations - 1];
		for (m = noLI ? 0 : LICount; m < stations; m++) {
		throughput[m] = totThroughput * visits[m];
		if (type[m] == Solver.DELAY)
		utilization[m] = visits[m] * servTime[m][0];
		else
		utilization[m] = usPrec[m][customers];
		queueLen[m] = nsPrec[m][customers];
		residenceTime[m] = queueLen[m] / totThroughput;
		}
		*/

		//calculation of other parameters
		totThroughput = interm_totThroughput[customers] = X[customers] / visits[stations - 1];
		for (m = noLI ? 0 : LICount; m < stations; m++) {
			throughput[m] = interm_throughput[customers][m] = totThroughput * visits[m];
			if (type[m] == Solver.DELAY) {
				utilization[m] = interm_utilization[customers][m] = visits[m] * servTime[m][0];
			} else {
				utilization[m] = interm_utilization[customers][m] = usPrec[m][customers];
			}
			queueLen[m] = interm_queueLen[customers][m] = nsPrec[m][customers];
			residenceTime[m] = interm_residenceTime[customers][m] = queueLen[m] / totThroughput;
		}

		if (!noLI) {
			for (m = 0; m < LICount; m++) {
				throughput[m] = interm_throughput[customers][m] = totThroughput * visits[m];
				utilization[m] = interm_utilization[customers][m] = D[m] * totThroughput;
				queueLen[m] = interm_queueLen[customers][m] = 0.0;
				for (n = 1; n <= customers; n++) {
					if (type[m] == Solver.DELAY) {
						residenceTime[m] = interm_residenceTime[customers][m] = D[m];
					} else {
						residenceTime[m] = interm_residenceTime[customers][m] = D[m] * (1 + queueLen[m]);
					}
					queueLen[m] = interm_queueLen[customers][m] = (X[n] / visits[stations - 1]) * residenceTime[m];
				}
				residenceTime[m] = interm_residenceTime[customers][m] = queueLen[m] / totThroughput;
			}
		}

		//NEW
		//@author Stefano Omini

		//other aggregate parameters
		totRespTime = totUser = 0;
		for (m = 0; m < stations; m++) {
			totRespTime += residenceTime[m];
			totUser += queueLen[m]; //of course should be equal to the number of customers
		}

		//end NEW

		//System.out.println("used LDErrCtrFaster");
	}

	/**
	 * Restores the original positions of stations (?)
	 */
	private void reorder() {
		String tempName;
		int tempInt;
		double tempDouble;
		for (int i = 0; i < position.length; i++) {
			tempName = name[position[i]];
			name[position[i]] = name[i];
			name[i] = tempName;

			tempInt = type[position[i]];
			type[position[i]] = type[i];
			type[i] = tempInt;

			tempDouble = queueLen[position[i]];
			queueLen[position[i]] = queueLen[i];
			queueLen[i] = tempDouble;

			tempDouble = residenceTime[position[i]];
			residenceTime[position[i]] = residenceTime[i];
			residenceTime[i] = tempDouble;

			tempDouble = throughput[position[i]];
			throughput[position[i]] = throughput[i];
			throughput[i] = tempDouble;

			tempDouble = utilization[position[i]];
			utilization[position[i]] = utilization[i];
			utilization[i] = tempDouble;

			if (intermediate_results) {

				for (int n = 0; n <= customers; n++) {
					//also intermediate results have to be reordered
					tempDouble = interm_queueLen[n][position[i]];
					interm_queueLen[n][position[i]] = interm_queueLen[n][i];
					interm_queueLen[n][i] = tempDouble;

					tempDouble = interm_residenceTime[n][position[i]];
					interm_residenceTime[n][position[i]] = interm_residenceTime[n][i];
					interm_residenceTime[n][i] = tempDouble;

					tempDouble = interm_throughput[n][position[i]];
					interm_throughput[n][position[i]] = interm_throughput[n][i];
					interm_throughput[n][i] = tempDouble;

					tempDouble = interm_utilization[n][position[i]];
					interm_utilization[n][position[i]] = interm_utilization[n][i];
					interm_utilization[n][i] = tempDouble;

				}

			}
			//at the end "resets" the position array
			tempInt = position[position[i]];
			position[position[i]] = position[i];
			position[i] = tempInt;
		}
	}

	//NEW
	//@author Stefano Omini

	/** generates a string with all the calculated indexes.
	 * @return the string
	 */

	@Override
	public String toString() {
		if (!intermediate_results) {
			//if intermediate results haven't been saved, don't write them!
			return super.toString();
		}
		StringBuffer buf = new StringBuffer();
		buf.append("\n------------------------------------");
		buf.append("\nAnalysis with MVA Singleclass: intermediate results");
		buf.append("\n\n");

		for (int n = 0; n <= customers; n++) {
			buf.append("\n\nPopulation = " + n + " customers\n\n");
			buf.append("Queue Length (Q)");
			buf.append("\n------------------------------------\n");
			for (int m = 0; m < stations; m++) {
				buf.append("[   ");
				buf.append(Printer.print(interm_queueLen[n][m], 6));
				buf.append("   ]\n");
			}

			buf.append("\n\n");
			buf.append("Residence Time (R)");
			buf.append("\n------------------------------------\n");
			for (int m = 0; m < stations; m++) {
				buf.append("[   ");
				buf.append(Printer.print(interm_residenceTime[n][m], 6));
				buf.append("   ]\n");
			}

			buf.append("\n\n");
			buf.append("Throughput (X)");
			buf.append("\n------------------------------------\n");
			for (int m = 0; m < stations; m++) {
				buf.append("[   ");
				buf.append(Printer.print(interm_throughput[n][m], 6));
				buf.append("   ]\n");
			}

			buf.append("\n\n");
			buf.append("Utilization (U)");
			buf.append("\n------------------------------------\n");
			for (int m = 0; m < stations; m++) {
				buf.append("[   ");
				buf.append(Printer.print(interm_utilization[n][m], 6));
				buf.append("   ]\n");
			}

		}

		return buf.toString();
	}

	//end NEW

}
