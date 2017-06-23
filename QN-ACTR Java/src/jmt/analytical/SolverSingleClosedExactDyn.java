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
 * SolverSingleClosedExactDyn.java
 *
 * Created on 21 novembre 2002, 17.47
 */

package jmt.analytical;

import java.io.PrintWriter;

/**
 * Solves a single class closed model, using the normalization constant algorithm.
 * <br>
 * NOT USED
 * @author  Federico Granata
 */
public class SolverSingleClosedExactDyn extends jmt.analytical.SolverSingleClosedExact {

	private double[] scalPrec;
	private double[] scalCorr;

	public SolverSingleClosedExactDyn(int customers, int stations) {
		super(customers, stations);
	}

	/** Solves the system
	 */
	@Override
	public void solve() {
		scalPrec = new double[customers + 1];
		scalCorr = new double[customers + 1];
		PrintWriter pw = new PrintWriter(System.out, true);
		double Y = 0;
		double[] FM = new double[customers + 1];
		double temp;
		double sum = 0;
		double MAX = 1.0E250;
		double MIN = Double.parseDouble("1.0e-250");
		long start; // used in time elapsed calculating
		long end; // used in time elapsed calculating
		int center = 0; // center index
		int cust = 0; // customer index
		int auxCust = 0; // an other customer index

		double beta = 1;

		start = System.currentTimeMillis();
		pw.println("solving");

		/* Start calculation of Norm Const*/
		/* first service center */
		G[0] = 1;
		scalPrec[0] = 1;

		if (type[0] == Solver.LI) {
			Y = visits[0] * servTime[0][0];
			for (int n = 1; n < G.length; n++) {
				G[n] = Y * G[n - 1];
				if (G[n] >= MAX) {
					beta = Math.pow(MAX, -1.0 / n);
					scalPrec[n] = scalPrec[n - 1] * beta;
					G[n] /= MAX;
					Y *= beta;
					for (int m = 0; m < stations; m++) {
						visits[m] *= beta;
					}
				} else if (G[n] <= MIN) {
					beta = Math.pow(MIN, -1.0 / n);
					scalPrec[n] = scalPrec[n - 1] * beta;
					G[n] /= MIN;
					Y *= beta;
					for (int m = 0; m < stations; m++) {
						visits[m] *= beta;
					}
				} else {
					scalPrec[n] = scalPrec[n - 1];
				}
			}
			//pw.println("G of center 0 : " + G[customers]);
		} else if (type[0] == Solver.LD) {
			for (int n = 1; n < G.length; n++) {
				G[n] = visits[0] * G[n - 1] * servTime[0][n];
				if (G[n] >= MAX) {
					beta = Math.pow(MAX, -1.0 / n);
					scalPrec[n] *= beta;
					G[n] /= MAX;
					for (int m = 0; m < stations; m++) {
						visits[m] *= beta;
					}
				} else if (G[n] <= MIN) {
					beta = Math.pow(MIN, -1.0 / n);
					scalPrec[n] *= beta;
					G[n] /= MIN;
					for (int m = 0; m < stations; m++) {
						visits[m] *= beta;
					}
				} else {
					scalPrec[n] = scalPrec[n - 1];
				}
			}
			//pw.println("G of center 0 : " + G[customers]);
		}

		/* all others service center */
		for (center = 1; center < stations; center++) {
			if (type[center] == Solver.LI) {
				Y = visits[center] * servTime[center][0];
				for (cust = 1; cust < G.length; cust++) {
					if (scalPrec[cust] == scalCorr[cust - 1]) {
						G[cust] = G[cust] + Y * G[cust - 1];
					} else {
						G[cust] = G[cust] * Math.pow(scalCorr[cust - 1] / scalPrec[cust], cust) + Y * G[cust - 1];
					}
					if (G[cust] >= MAX) {
						beta = Math.pow(MAX, -1.0 / cust);
						scalCorr[cust] = scalCorr[cust - 1] * beta;
						G[cust] /= MAX;
						Y *= beta;
						for (int m = 0; m < stations; m++) {
							visits[m] *= beta;
						}
					} else if (G[cust] <= MIN) {
						beta = Math.pow(MIN, -1.0 / cust);
						scalCorr[cust] = scalCorr[cust - 1] * beta;
						G[cust] /= MIN;
						Y *= beta;
						for (int m = 0; m < stations; m++) {
							visits[m] *= beta;
						}
					} else {
						scalCorr[cust] = scalCorr[cust - 1];
					}
				}
				//pw.println(" G of center " + center + " : " + G[customers]);
			}

			if (type[center] == Solver.LD) {
				FM[0] = 1;
				Y = visits[center];
				for (cust = 1; cust < G.length; cust++) {
					FM[cust] = FM[cust - 1] * Y * servTime[center][cust];
				}
				for (cust = (G.length - 1); cust > 0; cust--) {
					sum = 0;
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
			System.arraycopy(scalCorr, 0, scalPrec, 0, customers + 1);
		}
		pw.println("End solving");
		end = System.currentTimeMillis();
		pw.println("Time elapsed in milliseconds : " + (end - start));
		return;
	}

	/** Calculates the indexes of interest for the system.<br>
	 *  For a description of the algorithm see:<br>
	 * <em>
	 *  S.C. Bruell, G. Balbo,<br>
	 * "Computational Algorithms for closed Queueing Networks"<br>
	 * 1980, Elsevier North Holland
	 */
	@Override
	public void indexes() {
		//TODO: da implementare??
	}

}
