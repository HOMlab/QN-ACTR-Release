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

/**
 * Solves a single class open model. <br>
 * To read about the formulas used to solve the system, see:<br>
 * <em>
 * E.D. Lazowska, J. Zahorjan, G.S. Graham, K. Sevcik<br>
 * Quantitative System Performance,<br>
 * Prentice Hall, 1984<br>
 * </em>
 *
 * NOT USED
 * @author Federico Granata, Stefano Omini
 *
 */
public class SolverSingleOpen extends Solver {

	/**
	 * arrival rate
	 */
	protected double lambda;

	/**
	 * Constructor
	 * @param lambda the arrival rate
	 * @param stations the number of stations
	 *
	 */

	public SolverSingleOpen(double lambda, int stations) {
		this.lambda = lambda;
		this.stations = stations;

		name = new String[stations];
		type = new int[stations];
		//the matrix turns into an array, because for SolverSingleOpen LD centers are not allowed
		servTime = new double[stations][1];
		visits = new double[stations];

		throughput = new double[stations];
		queueLen = new double[stations];
		utilization = new double[stations];
		residenceTime = new double[stations];
	}

	//TODO: bisognerebbe far ritornare un boolean, per coprire il caso di soluzione non possibile (LD)

	//NEW
	//the old method "solve" has been renamed "solveLI", since LD stations weren't
	//allowed
	//a new method "solve" has been introduced to check that no LD stations are contained
	//in the system.
	//
	//@author Stefano Omini

	/**
	 * Solves the system, after checking that the system contains no LD station
	 * (only LI and DELAY stations are allowed).
	 <br>* "input(...)" method must have been called before solving the model!!
	 */
	@Override
	public void solve() {

		for (int i = 0; i < stations; i++) {
			//checks if LD stations are present in the system
			if (type[i] == Solver.LD) {
				return;
			}
		}
		//no LD stations present: solve the system
		solveLI();
		return;
	}

	/**
	 * Solves the system with only LI and DELAY stations.
	 */
	public void solveLI() {
		//initialize system aggregate measures
		totRespTime = totUser = 0;

		for (int i = 0; i < throughput.length; i++) {
			//station utilization
			utilization[i] = lambda * visits[i] * servTime[i][0];

			//station residence time
			if (type[i] == Solver.DELAY) {
				residenceTime[i] = visits[i] * servTime[i][0];
			} else {
				residenceTime[i] = visits[i] * servTime[i][0] / (1 - utilization[i]);
			}

			//station queue length
			queueLen[i] = residenceTime[i] * lambda;

			//system response time
			totRespTime += residenceTime[i];

			//average number in system
			totUser += queueLen[i];

			//station throughput
			//OLD
			//throughput[i] = queueLen[i] / residenceTime[i];
			throughput[i] = lambda * visits[i];
		}
		//system throughput
		totThroughput = lambda;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	//TODO aggiungere controllo su processing capacity
	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of such a load.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 */
	@Override
	public boolean hasSufficientProcessingCapacity() {

		//load lambda must be < 1/Dmax (that is Uj < 1 for all stations)
		//otherwise the system has no sufficient processing capacity

		for (int j = 0; j < stations; j++) {
			//utilization for station j
			//Uj = lambda * Vj * Sj = lambda * Dj
			double utiliz_j = lambda * visits[j] * servTime[j][0];
			if (utiliz_j >= 1) {
				return false;
			}
		}
		//there are no stations with utilization >= 1
		return true;
	}

	//end NEW

}
