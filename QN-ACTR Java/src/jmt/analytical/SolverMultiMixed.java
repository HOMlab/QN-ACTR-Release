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
 * Solves a multiclass model, with both open and closed classes.
 * @author Federico Granata, Stefano Omini
 */
public class SolverMultiMixed extends SolverMulti {
	//TODO aggiungere controllo su processing capacity (guardare solo le classi aperte)

	//TODO: aggiungere calcolo del response time di ogni stazione

	/** array with class types */
	int[] classType;

	/** matrix with service demands */
	double[][] demands;

	/** array describing the classes: each element can be either an
	 *  arrival rate (for open classes) or a population (for closed ones),
	 *  according to the class type  */
	double[] popPar;

	/**
	 * Constructor
	 * @param classes number of classes
	 * @param stations number of stations
	 */
	public SolverMultiMixed(int classes, int stations) {
		super(classes, stations);
		demands = new double[stations][classes];
	}

	/** initializes the Multi class solver with the system parameters.
	 *  @param  n   array of names of service centers.
	 *  @param  t   array of the types (LD or LI) of service centers.
	 *  @param  s   matrix of service time of the service centers.
	 *  @param  v   matrix of visits to the service centers.
	 *  @param popPar array describing the classes: each element can be either an
	 *  arrival rate (for open classes) or a population (for closed ones),
	 *  according to the class type
	 *  @param classType array of class types (open or closed)
	 *  @return true if the operation has been completed with success
	 */
	public boolean input(String[] n, int[] t, double[][][] s, double[][] v, double[] popPar, int[] classType) {
		if (super.input(n, t, s, v)) {
			this.popPar = popPar;
			this.classType = classType;
			return true;
		}
		return false;
	}

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
			//System.out.println(this);
		} else {
			System.out.println("Load dependent solver is not available for MultiMixed");
		}
	}

	/**
	 * Solves the system.
	 * <br><br>
	 * See:
	 * <br>
	 * <em>
	 * E.D. Lazowska, J. Zahorjan, G.S. Graham, K. Sevcik<br>
	 * Quantitative System Performance,<br>
	 * Prentice Hall, 1984<br>
	 * </em>
	 */
	public void solveLI() {
		//TODO: ci sono problemi nel caso di stazioni delay???!!!
		//class parameters: population (for closed classes), arrival rate (for open classes)
		int[] closedClasses;
		double[] openClasses;

		//take in consideraton the presence of the open classes only.
		//we solve easily the problem as an open system.
		//the new utilization will reduce the power of the system
		//considering the effect of the open classes.
		//then we solve the close system
		for (int i = 0; i < stations; i++) {
			//NEW
			//@author Stefano Omini
			//initializes service center solution values
			scUtilization[i] = 0;
			scQueueLen[i] = 0;
			scResidTime[i] = 0;
			scThroughput[i] = 0;
			//end NEW

			for (int j = 0; j < classes; j++) {
				demands[i][j] = visits[i][j] * servTime[i][j][0];
				if (classType[j] == OPEN_CLASS) {
					utilization[i][j] = demands[i][j] * popPar[j];
					scUtilization[i] += utilization[i][j];
				}
			}
		}

		//counts open classes
		int oCounter = 0;
		for (int j = 0; j < classes; j++) {
			if (classType[j] == OPEN_CLASS) {
				oCounter++;
			}
		}

		//counts closed classes
		int cCounter = classes - oCounter;

		//allocates an array with the appropriate size for each kind of class
		openClasses = new double[oCounter];
		closedClasses = new int[cCounter];

		//fills the arrays containing only open parameters (i.e. lambda)
		//and only closed parameters (i.e. population)
		oCounter = 0;
		cCounter = 0;

		//TODO: non crea strutture analoghe anche per open,perchè le D delle classi open non cambiano
		double[][][] closedServTime = new double[stations][closedClasses.length][1];
		double[][] closedVisits = new double[stations][closedClasses.length];

		for (int j = 0; j < classes; j++) {
			if (classType[j] == OPEN_CLASS) {
				//this parameter is a lambda
				openClasses[oCounter] = popPar[j];
				oCounter++;
			} else {
				//this parameter is a population
				closedClasses[cCounter] = (int) Math.ceil(popPar[j]);
				for (int i = 0; i < stations; i++) {
					/*
					the effect of the open classes on closed class performance is
					represented by "inflating" the service demands of the closed class
					at all devices.
					        (D*)c,k = (D)c,k / ( 1 - (U)open,k )
					The inflaction factor used is ( 1 - (U)open,k ) which is the
					percentage of time that the processor is not in use by the open classes.
					This technique allows to reduce model complexity by eliminating
					open classes while still incorporating their effects on performance.
					*/

					//these are the S* and V* of the inflated service demands
					//OLD
					//closedVisits[i][cCounter] = visits[i][j] / (1 - scUtilization[i]);
					//closedServTime[i][cCounter] = servTime[i][j];
					closedVisits[i][cCounter] = visits[i][j];
					closedServTime[i][cCounter][0] = servTime[i][j][0] / (1 - scUtilization[i]);
				}
				cCounter++;
			}
		}

		if (cCounter > 1) {
			//more than 1 closed class

			//solves the model consisting of only closed classes (with inflated D*)
			//X, Q and R obtained are valid also for the mixed model
			//U can be computed by applying the utilization law with the original set of D
			SolverMultiClosedMVA clSolver = new SolverMultiClosedMVA(cCounter, stations);
			clSolver.input(name, type, closedServTime, closedVisits, closedClasses);
			clSolver.solve();

			oCounter = 0;
			cCounter = 0;

			for (int i = 0; i < stations; i++) {
				//this value of the closed model is used to compute open classes parameters
				//see Lazowska
				scQueueLen[i] = clSolver.getAggrQueueLen(i);

				cCounter = 0;
				for (int j = 0; j < classes; j++) {
					if (classType[j] == CLOSED_CLASS) {
						//closed class
						queueLen[i][j] = clSolver.getQueueLen(i, cCounter);
						residenceTime[i][j] = clSolver.getResTime(i, cCounter);

						//OLD
						//throughput[i][j] = queueLen[i][j] / residenceTime[i][j];
						//utilization[i][j] = throughput[i][j] * closedServTime[i][cCounter][0];

						//NEW
						//@author Stefano Omini
						throughput[i][j] = clSolver.getThroughput(i, cCounter);

						//utilization must be computed (using the utilization law) with
						//the original service demand
						utilization[i][j] = throughput[i][j] * servTime[i][j][0];

						//end NEW

						clsRespTime[j] += residenceTime[i][j];
						cCounter++;
					} else {
						//open class
						//utilizations have been already computed
						residenceTime[i][j] = demands[i][j] * (1 + scQueueLen[i]) / (1 - scUtilization[i]);
						queueLen[i][j] = popPar[j] * residenceTime[i][j];
						//OLD
						//throughput[i][j] = queueLen[i][j] / residenceTime[i][j];
						//NEW
						//@author Stefano Omini
						throughput[i][j] = popPar[j] * visits[i][j];
						//end NEW
						clsRespTime[j] += residenceTime[i][j];

					}
					scThroughput[i] += throughput[i][j];
					clsNumJobs[j] += queueLen[i][j];
				}
			}

			for (int j = 0; j < classes; j++) {
				clsThroughput[j] = clsNumJobs[j] / clsRespTime[j];
				sysNumJobs += clsNumJobs[j];
			}

			sysResponseTime = 0;

			for (int i = 0; i < stations; i++) {
				scResidTime[i] = 0;
				scUtilization[i] = 0;
				for (int j = 0; j < classes; j++) {

					scUtilization[i] += utilization[i][j];
					if (classType[j] != CLOSED_CLASS) {
						//queue length of closed classes has been alredy considered
						scQueueLen[i] += queueLen[i][j];
					}
				}
				scResidTime[i] = scQueueLen[i] / scThroughput[i];
				sysResponseTime += scResidTime[i];

			}
			sysThroughput = sysNumJobs / sysResponseTime;

		} else {
			//only 1 closed class
			SolverSingleClosedMVA clSolver = new SolverSingleClosedMVA(closedClasses[0], stations);
			double[][] tempServTime = new double[closedServTime.length][];
			double[] tempVisits = new double[closedServTime.length];

			for (int i = 0; i < tempVisits.length; i++) {
				/*
				the effect of the open classes on closed class performance is
				represented by "inflating" the service demands of the closed class
				at all devices.
				(D*)c,k = (D)c,k / ( 1 - (U)open,k )
				The inflaction factor used is ( 1 - (U)open,k ) which is the
				percentage of time that the processor is not in use by the open classes.
				This technique allows to reduce model complexity by eliminating
				open classes while still incorporating their effects on performance.
				*/

				//these are the S* and V* of the inflated service demands
				tempVisits[i] = closedVisits[i][0];
				tempServTime[i] = closedServTime[i][0];
			}
			//solves the model consisting of only the closed class (with inflated D*)
			//X, Q and R obtained are valid also for the mixed model
			//U can be computed by applying the utilization law with the original set of D
			clSolver.input(name, type, tempServTime, tempVisits);
			clSolver.solve();

			oCounter = 0;
			cCounter = 0;

			for (int i = 0; i < stations; i++) {
				scQueueLen[i] = clSolver.getQueueLen(i);

				cCounter = 0;
				for (int j = 0; j < classes; j++) {
					if (classType[j] == CLOSED_CLASS) {
						//closed class
						queueLen[i][j] = clSolver.getQueueLen(i);
						residenceTime[i][j] = clSolver.getResTime(i);
						//OLD
						//throughput[i][j] = queueLen[i][j] / residenceTime[i][j];
						//utilization[i][j] = throughput[i][j] * closedServTime[i][cCounter][0];
						//must be multiplicated for visits number!!

						//OLD
						//throughput[i][j] = queueLen[i][j] * closedVisits[i][cCounter] / residenceTime[i][j];
						//NEW
						//@author Stefano Omini
						//TODO: verificare correttezza modifiche
						throughput[i][j] = clSolver.getThroughput(i);
						//utilization must be computed (using the utilization law) with the original
						//service demand
						utilization[i][j] = throughput[i][j] * servTime[i][j][0];
						//end NEW

						clsRespTime[j] += residenceTime[i][j];
						cCounter++;
					} else {
						//open class
						residenceTime[i][j] = demands[i][j] * (1 + scQueueLen[i]) / (1 - scUtilization[i]);
						queueLen[i][j] = popPar[j] * residenceTime[i][j];
						//OLD
						//throughput[i][j] = queueLen[i][j] / residenceTime[i][j];
						//NEW
						//@author Stefano Omini
						throughput[i][j] = popPar[j] * visits[i][j];
						//end NEW
						clsRespTime[j] += residenceTime[i][j];

					}
					scThroughput[i] += throughput[i][j];
					clsNumJobs[j] += queueLen[i][j];
				}
			}

			for (int j = 0; j < classes; j++) {
				clsThroughput[j] = clsNumJobs[j] / clsRespTime[j];
				sysNumJobs += clsNumJobs[j];
			}

			sysResponseTime = 0;

			for (int i = 0; i < stations; i++) {
				scResidTime[i] = 0;
				scUtilization[i] = 0;
				for (int j = 0; j < classes; j++) {

					scUtilization[i] += utilization[i][j];
					if (classType[j] != CLOSED_CLASS) {
						scQueueLen[i] += queueLen[i][j];
					}
				}
				scResidTime[i] = scQueueLen[i] / scThroughput[i];
				sysResponseTime += scResidTime[i];

			}
			sysThroughput = sysNumJobs / sysResponseTime;
		}
	}

	//NEW
	//@author Stefano Omini
	//TODO aggiungere controllo su processing capacity
	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of the combined loads
	 * of all the open classes.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 */
	@Override
	public boolean hasSufficientProcessingCapacity() {

		//the maximum aggregate utilization between all the stations must be < 1
		//otherwise the system has no sufficient processing capacity
		for (int i = 0; i < stations; i++) {

			if (type[i] == SolverMulti.DELAY) {
				//delay station: don't check saturation
				continue;
			}

			//utiliz is the aggregate utilization for station j
			double utiliz = 0;
			for (int j = 0; j < classes; j++) {
				//consider only open classes
				if (classType[j] == OPEN_CLASS) {
					utiliz += popPar[j] * visits[i][j] * servTime[i][j][0];
				}
			}
			if (utiliz >= 1) {
				return false;
			}
		}
		//there are no stations with aggregate utilization >= 1
		return true;
	}

	//end NEW

}
