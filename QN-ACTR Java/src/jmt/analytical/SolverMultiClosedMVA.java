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
 * SolverMultiClosedMVA.java
 * Created on 11 dicembre 2002, 9.37
 */

package jmt.analytical;

/**
 * Solves a multiclass closed model, using MVA algorithm.
 * @author  Federico Granata, Stefano Omini
 */
public class SolverMultiClosedMVA extends SolverMulti {

	public final boolean DEBUG = false;

	//array of population for every class
	private int[] population;

	//an array used to reduce the computation every time
	//needed the product of n class population
	private int[] popMult;

	//status of the computation
	private int[] status;

	//sum af all populations
	private int totPop = 0;

	//queuelength vector
	private double[][] n;

	//private double sysResponseTime = 0;

	//private double sysThroughput = 0;

	private int maxIndex = -1;

	//number of states
	private int memory = 1;

	//NEW
	//@author Stefano Omini

	//---INTERMEDIATE RESULTS (0,1,2 ...N customers)------------//

	/** Intermediate results: throughput for each service station, class<br>
	 * [customers] [station] [class] */
	protected double[][][] interm_throughput;

	/** Intermediate results: utilization for each service station, class<br>
	 * [customers] [station] [class] */
	protected double[][][] interm_utilization;

	/** Intermediate results: queue lenght for each service station, class<br>
	 * [customers] [station] [class] */
	protected double[][][] interm_queueLen;

	/** Intermediate results: residence time for each service station, class<br>
	 * [customers] [station] [class] */
	protected double[][][] interm_residenceTime;

	/** Intermediate results: throughputs of the service centers<br>
	 *  [customers] [station] */
	protected double[][] interm_scThroughput;

	/** Intermediate results: utilization of the service centers<br>
	 *  [customers] [station] */
	protected double[][] interm_scUtilization;

	/** Intermediate results: queue lenght of the service centers<br>
	 *  [customers] [station] */
	protected double[][] interm_scQueueLen;

	/** Intermediate results: residence time of the service centers<br>
	 *  [customers] [station] */
	protected double[][] interm_scResidTime;

	/** Intermediate results: response time for each class<br>
	 * [customers] [class] */
	protected double[][] interm_clsRespTime;

	/** Intermediate results: throughput for each class<br>
	 * [customers] [class] */
	protected double[][] interm_clsThroughput;

	/** Intermediate results: average number of users for each class<br>
	 * [customers] [class] */
	protected double[][] interm_clsNumJobs;

	/** Intermediate results: System response time
	 * [customers]   */
	protected double[] interm_sysResponseTime;

	/** Intermediate results: System throughput
	 * [customers]  */
	protected double[] interm_sysThroughput;

	/** Intermediate results: Number of jobs in the system
	 * [customers]  */
	protected double[] interm_sysNumJobs;

	/**
	 * Tells whether intermediate results have been computed or not
	 */
	protected boolean intermediate_results = false;

	//---------------------------------------------------//

	//end NEW

	/** Creates new SolverMultiClosedMVA
	 *  @param  stations    number of service centers
	 *  @param  classes     number of classes of customers
	 */
	public SolverMultiClosedMVA(int classes, int stations) {
		super(classes, stations);
		population = new int[classes];
		popMult = new int[classes];
		status = new int[classes];

		//	    intZeros = new int[cls];

		//NEW
		//@author Stefano Omini

		interm_throughput = null;
		interm_utilization = null;
		interm_queueLen = null;
		interm_residenceTime = null;
		interm_scThroughput = null;
		interm_scUtilization = null;
		interm_scQueueLen = null;
		interm_scResidTime = null;
		interm_clsRespTime = null;
		interm_clsThroughput = null;
		interm_clsNumJobs = null;
		interm_sysResponseTime = null;
		interm_sysThroughput = null;
		interm_sysNumJobs = null;

		//end NEW

	}

	//TODO: attenzione, non è in grado di gestire classi con popolazioni nulle
	/** initializes the Multi class solver with the system parameters.
	 * @param  n   array of names of the service centers.
	 * @param  t   array of types (LD or LI) of the service centers.
	 * @param  s   matrix of service time of the service centers.
	 * @param  v   matrix of visits to the service centers.
	 * @param pop  array of populations of each class
	 * @return true if the operation is completed with success
	 */
	public boolean input(String[] n, int[] t, double[][][] s, double[][] v, int[] pop) {
		int maxPop = 0;
		int lastClass = classes - 1;

		if ((n.length != stations) || (t.length != stations) || (s.length != stations) || (v.length != stations)) {
			return false; // wrong input.
		}

		System.arraycopy(n, 0, name, 0, stations);
		System.arraycopy(t, 0, type, 0, stations);

		//OLD
		//for (int c = 0; c < classes; c++)
		//	totPop += pop[c];

		//NEW
		//@author Stefano Omini
		for (int c = 0; c < classes; c++) {
			if (pop[c] > 0) {
				totPop += pop[c];
			} else {
				System.out.println("Error: class population must be greater than 0.");
				return false;
			}

		}
		//end NEW

		for (int i = 0; i < stations; i++) {
			//DEK (Federico Granata) 26-09-2003
			/*			for (int j = 0; j < classes; j++)
							servTime[i][j] = new double[totPop];*/
			if (t[i] == LD) {
				for (int j = 0; j < classes; j++) {
					servTime[i][j] = new double[totPop];
				}
			} else {
				for (int j = 0; j < classes; j++) {
					servTime[i][j] = new double[1];
					//end
				}
			}
		}

		System.arraycopy(pop, 0, population, 0, pop.length);
		//finds out the class with maximum population
		for (int c = 0; c < classes; c++) {
			//OLD
			//if (population[c] > maxPop) {
			if (population[c] >= maxPop) {
				maxPop = population[c];
				maxIndex = c;
			}
		}

		//moves in the last position the class with maximum population
		//todo: perchè? (per ottimizzare consumo di memoria muovendosi intelligentemente nello spazio degli stati)
		population[lastClass] = pop[maxIndex];
		population[maxIndex] = pop[lastClass];

		//computes the number of states
		for (int c = 0; c < classes; c++) {
			if (c != maxIndex) {
				memory *= (pop[c] + 1);
			}
		}

		for (int i = 0; i < stations; i++) {
			System.arraycopy(v[i], 0, visits[i], 0, lastClass);
			visits[i][lastClass] = v[i][maxIndex];
			visits[i][maxIndex] = v[i][lastClass];

			for (int j = 0; j < lastClass; j++) {
				System.arraycopy(s[i][j], 0, servTime[i][j], 0, s[i][j].length);
			}
			System.arraycopy(s[i][maxIndex], 0, servTime[i][lastClass], 0, s[i][maxIndex].length);
			//todo: perchè copia s[i][0].length e non s[i][lastClass].length ?
			//OLD
			//System.arraycopy(s[i][lastClass], 0, servTime[i][maxIndex], 0, s[i][0].length);
			//NEW
			//@author Stefano Omini
			System.arraycopy(s[i][lastClass], 0, servTime[i][maxIndex], 0, s[i][lastClass].length);

		}

		popMult[0] = 1;
		if (DEBUG) {
			System.out.println("Pop mult, elemento " + 0 + " pari a: " + 1);
		}
		for (int i = 1; i < popMult.length; i++) {
			popMult[i] = popMult[i - 1] * (population[i - 1] + 1);
			if (DEBUG) {
				System.out.println("Pop mult, elemento " + i + " pari a: " + popMult[i]);
			}
		}
		return true;
	}

	//NEW
	//@author Stefano Omini

	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of the combined loads
	 * of all the classes.
	 * <br>
	 * Must be implemented to create a multi class model solver.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 *
	 *
	 */
	@Override
	public boolean hasSufficientProcessingCapacity() {
		//only closed class: no saturation
		return true;
	}

	//end NEW

	/**
	 * Solves the model, using an appropriate technique (LI or LD model).
	 * <br>
	 * "input(...)" method must have been called before solving the model!!<br><br>
	 *
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
			//System.out.println(this);
		} else {
			//TODO: non funziona
			//solveLD();
		}
	}

	/**
	 * Solves the model, using an appropriate technique (LI or LD model).
	 * <br>
	 * To solve a closed model with N customers, all the models with
	 * 0, 1, 2, ... N-1 customers must be solved before.
	 * This method allows to save the results for these intermediate models.
	 *
	 * @param intermediate true to save intermediate results
	 *
	 */
	public void solve(boolean intermediate) {
		//tests if all the resources, stations, are load independent
		boolean loadIndep = true;
		for (int i = 0; i < stations && loadIndep; i++) {
			if (type[i] == LD) {
				loadIndep = false;
			}
		}

		if (loadIndep) {
			solveLI(intermediate);
			//System.out.println(this);
		} else {
			//TODO: ancora da implementare
			//solveLD(intermediate);
		}
	}

	/**
	 * Solves a model with only load independent stations
	 */
	private void solveLI() {
		int m = 0;//index of service center
		int c = 0;//index of class
		int lastClass = classes - 1;
		int index = 0;
		boolean flag;
		double sum;
		status[0] = 1;
		index = 1;
		//service demands
		double[][] demand = new double[stations][classes];
		//queue length\\TODO: memory????
		n = new double[stations][memory];

		//compute demands vector
		for (m = 0; m < stations; m++) {
			for (c = 0; c < classes; c++) {
				demand[m][c] = visits[m][c] * servTime[m][c][0];
			}
		}

		do {
			//TODO: tolta per controllare se serve, probabilmente è solo per debug
			//printStatus();
			//compute residence time
			for (m = 0; m < stations; m++) {
				for (c = 0; c < lastClass; c++) {
					if (status[c] > 0) {
						if (type[m] == LI) {
							residenceTime[m][c] = demand[m][c] * (1.0 + n[m][index - popMult[c]]);
						} else {
							residenceTime[m][c] = demand[m][c];
						}
					} else {
						residenceTime[m][c] = 0;
					}
				}
				if (status[lastClass] > 0) {
					if (type[m] == LI) {
						residenceTime[m][lastClass] = demand[m][lastClass] * (1.0 + n[m][index]);
					} else {
						residenceTime[m][lastClass] = demand[m][lastClass];
					}
				} else {
					residenceTime[m][lastClass] = 0;
				}
			}

			//compute throughput
			for (c = 0; c < classes; c++) {
				sum = 0;
				for (m = 0; m < stations; m++) {
					sum += residenceTime[m][c];
				}
				if (status[c] > 0) {
					//OLD
					//clsThroughput[c] = status[c] / sum;
					//NEW
					//@author Stefano Omini
					if (sum == 0) {
						//all D=0 for this class
						clsThroughput[c] = 0;
					} else {
						clsThroughput[c] = status[c] / sum;
					}
					//end NEW
				} else {
					clsThroughput[c] = 0;
				}
			}

			//compute queue length
			for (m = 0; m < stations; m++) {
				n[m][index] = 0;
				for (c = 0; c < classes; c++) {
					n[m][index] += clsThroughput[c] * residenceTime[m][c];
				}
			}

			index++;
			c = 0;
			do {
				status[c] += 1;
				if (c == lastClass) {
					index = 0;
				}
				flag = true;
				if (status[c] > population[c]) {
					status[c] = 0;
					c++;
					flag = false;
				}
			} while (c < classes && !flag);
		} while (c < classes);

		for (m = 0; m < stations; m++) {
			for (c = 0; c < classes; c++) {
				throughput[m][c] = clsThroughput[c] * visits[m][c];
				utilization[m][c] = throughput[m][c] * servTime[m][c][0]; // Umc=Xmc*Smc
				queueLen[m][c] = clsThroughput[c] * residenceTime[m][c];
				scThroughput[m] += throughput[m][c];
				scUtilization[m] += utilization[m][c];
				scResidTime[m] += residenceTime[m][c];
				scQueueLen[m] += queueLen[m][c];
			}
			//scQueueLen[m] = n[m][n.length - 1];
		}

		//adjusting the correct order of classes....
		double temp;
		for (m = 0; m < stations; m++) {
			temp = throughput[m][maxIndex];
			throughput[m][maxIndex] = throughput[m][lastClass];
			throughput[m][lastClass] = temp;
			temp = utilization[m][maxIndex];
			utilization[m][maxIndex] = utilization[m][lastClass];
			utilization[m][lastClass] = temp;
			temp = queueLen[m][maxIndex];
			queueLen[m][maxIndex] = queueLen[m][lastClass];
			queueLen[m][lastClass] = temp;
			temp = residenceTime[m][maxIndex];
			residenceTime[m][maxIndex] = residenceTime[m][lastClass];
			residenceTime[m][lastClass] = temp;
		}

		//NEW
		//@author Stefano Omini
		//compute system parameters
		sysResponseTime = 0;
		sysNumJobs = 0;

		for (c = 0; c < classes; c++) {
			for (m = 0; m < stations; m++) {
				clsRespTime[c] += residenceTime[m][c];
				sysNumJobs += queueLen[m][c];
			}
			sysResponseTime += clsRespTime[c];
		}

		sysThroughput = sysNumJobs / sysResponseTime;
		//end NEW
	}

	/**
	 * Solves a model with only load independent stations
	 */
	private void solveLI(boolean intermediate) {

		if (!intermediate) {
			//intermediate results mustn't be saved, then use
			//the base method
			solveLI();
			return;
		}

		//intermediate results must be saved
		intermediate_results = true;

		//TODO: ci interessano i risultati intermedi anche per i multiclass??

		int m = 0;//index of service center
		int c = 0;//index of class
		int lastClass = classes - 1;
		int index = 0;
		boolean flag;
		double sum;
		status[0] = 1;
		index = 1;
		//service demands
		double[][] demand = new double[stations][classes];
		//queue length
		n = new double[stations][memory];

		//compute demands vector
		for (m = 0; m < stations; m++) {
			for (c = 0; c < classes; c++) {
				demand[m][c] = visits[m][c] * servTime[m][c][0];
			}
		}

		do {
			//TODO: tolta per controllare se serve, probabilmente è solo per debug
			//printStatus();
			//compute residence time
			for (m = 0; m < stations; m++) {
				for (c = 0; c < lastClass; c++) {
					if (status[c] > 0) {
						if (type[m] == LI) {
							residenceTime[m][c] = demand[m][c] * (1.0 + n[m][index - popMult[c]]);
						} else {
							residenceTime[m][c] = demand[m][c];
						}
					} else {
						residenceTime[m][c] = 0;
					}
				}
				if (status[lastClass] > 0) {
					if (type[m] == LI) {
						residenceTime[m][lastClass] = demand[m][lastClass] * (1.0 + n[m][index]);
					} else {
						residenceTime[m][lastClass] = demand[m][lastClass];
					}
				} else {
					residenceTime[m][lastClass] = 0;
				}
			}

			//compute throughput
			for (c = 0; c < classes; c++) {
				sum = 0;
				for (m = 0; m < stations; m++) {
					sum += residenceTime[m][c];
				}
				if (status[c] > 0) {
					clsThroughput[c] = status[c] / sum;
				} else {
					clsThroughput[c] = 0;
				}
			}

			//compute queue length
			for (m = 0; m < stations; m++) {
				n[m][index] = 0;
				for (c = 0; c < classes; c++) {
					n[m][index] += clsThroughput[c] * residenceTime[m][c];
				}
			}

			index++;
			c = 0;
			do {
				status[c] += 1;
				if (c == lastClass) {
					index = 0;
				}
				flag = true;
				if (status[c] > population[c]) {
					status[c] = 0;
					c++;
					flag = false;
				}
			} while (c < classes && !flag);
		} while (c < classes);

		for (m = 0; m < stations; m++) {
			for (c = 0; c < classes; c++) {
				throughput[m][c] = clsThroughput[c] * visits[m][c];
				utilization[m][c] = throughput[m][c] * servTime[m][c][0]; // Umc=Xmc*Smc
				queueLen[m][c] = clsThroughput[c] * residenceTime[m][c];
				scThroughput[m] += throughput[m][c];
				scUtilization[m] += utilization[m][c];
				scResidTime[m] += residenceTime[m][c];
				scQueueLen[m] += queueLen[m][c];
			}
			//scQueueLen[m] = n[m][n.length - 1];
		}

		//adjusting the correct order of classes....
		double temp;
		for (m = 0; m < stations; m++) {
			temp = throughput[m][maxIndex];
			throughput[m][maxIndex] = throughput[m][lastClass];
			throughput[m][lastClass] = temp;
			temp = utilization[m][maxIndex];
			utilization[m][maxIndex] = utilization[m][lastClass];
			utilization[m][lastClass] = temp;
			temp = queueLen[m][maxIndex];
			queueLen[m][maxIndex] = queueLen[m][lastClass];
			queueLen[m][lastClass] = temp;
			temp = residenceTime[m][maxIndex];
			residenceTime[m][maxIndex] = residenceTime[m][lastClass];
			residenceTime[m][lastClass] = temp;
		}

		//NEW
		//@author Stefano Omini
		//compute system parameters
		sysResponseTime = 0;
		sysNumJobs = 0;

		for (c = 0; c < classes; c++) {
			for (m = 0; m < stations; m++) {
				clsRespTime[c] += residenceTime[m][c];
				sysNumJobs += queueLen[m][c];
			}
			sysResponseTime += clsRespTime[c];
		}

		sysThroughput = sysNumJobs / sysResponseTime;
		//end NEW
	}

	/**
	 * See:
	 * <br>
	 * G.Balbo, S.C.Bruell, L.Cerchio, D.Chiaberto, L.Molinatti
	 * "Mean Value Analysis of Closed Load Dependent Networks".
	 */
	private void solveLD() {
		//TODO da fare tutto.....
		n = new double[stations][memory];
		int sum = 0;
		int c;
		int index = 0;
		boolean flag;
		int last = classes - 1;

		for (c = 0; c < classes; c++) {
			sum += population[c];
		}
		double[][][] P = new double[stations + 1][sum + 1][memory];
		for (int s = 0; s < stations; s++) {
			P[s][0][0] = 1.0;
		}
		status = new int[classes];
		status[classes - 1] = 1;
		do {

			for (c = 0; c < classes; c++) {
				if (status[c] == 0) {
					for (int s = 0; s < stations; s++) {
						residenceTime[s + 1][c + 1] = 0.0;
					}
				} else {
					for (int s = 0; s < stations; s++) {
						if (type[s] == Solver.DELAY) {
							residenceTime[s + 1][c + 1] = servTime[s][c][0];
						} else if (type[s] == Solver.LI) {
							/* ix[c]--;
							x = linear( NC - 1 , ix , N ) ;
							residenceTime[s+1][c+1] = servTime[s][c][0] * ( 1 + n[s + 1][x]);
							ix[c]++;*/
						}
					}
				}
			}

			index++;
			c = 0;
			do {
				status[c] += 1;
				if (c == last) {
					index = 0;
				}
				flag = true;
				if (status[c] > population[c]) {
					status[c] = 0;
					c++;
					flag = false;
				}
			} while (c < classes && !flag);
			//            printStatus();
		} while (c < classes);

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
		buf.append("\nAnalysis with MVA Multiclass: intermediate results");
		buf.append("\n\n");

		/*

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

		*/

		return buf.toString();
	}

	//end NEW

	/** print the actual status of the system (a debug function)
	 */
	private void printStatus() {
		if (status[1] % 3 == 0 && status[1] > 0 && ((status[0] / status[1]) == 1 / 3)) {
			for (int i = 0; i < classes; i++) {
				System.out.print(status[i] + " ");
			}
			System.out.print(getClsThroughput(0) + " " + getClsThroughput(1) + "\n");
		}
	}

	public static void testLI() {
		int nCust = 3;
		int nCent = 4;
		String[] name = new String[nCent];
		int[] type = new int[nCent];
		double[][][] timeMulti = new double[nCent][nCust][1];
		double[][] visitMulti = new double[nCent][nCust];
		int[] popVect = new int[nCust];

		for (int i = 0; i < nCent; i++) {
			name[i] = "Service Center " + i;
			type[i] = LI;
		}

		popVect[0] = 10;
		popVect[1] = 15;
		popVect[2] = 13;

		//not a very general test but I don't want to write a lot of data..
		for (int i = 0; i < nCent; i++) {
			double local = (i + 1) * 0.5;
			for (int j = 0; j < nCust; j++) {
				timeMulti[i][j][0] = local * (j + 1) * 0.1;
			}
		}

		visitMulti[0][0] = 1;
		visitMulti[0][1] = 0.6;
		visitMulti[0][2] = 5;
		visitMulti[1][0] = 0.12;
		visitMulti[1][1] = 0.81;
		visitMulti[1][2] = 0.61;
		visitMulti[2][0] = 0.4;
		visitMulti[2][1] = 0.6;
		visitMulti[2][2] = 0.49;
		visitMulti[3][0] = 0.86;
		visitMulti[3][1] = 0.38;
		visitMulti[3][2] = 0.15;

		SolverMultiClosedMVA solver = new SolverMultiClosedMVA(nCust, nCent);
		if (!solver.input(name, type, timeMulti, visitMulti, popVect)) {
			System.out.println("Vector Lengths not exact");
			return;
		}
		solver.solve();
		System.out.println("solved");
		System.out.print(solver);
		if ((Math.abs(2.667 - solver.getThroughput(0, 0)) >= 0.001) || (Math.abs(1.794 - solver.getThroughput(0, 1)) >= 0.001)
				|| (Math.abs(4.576 - solver.getThroughput(0, 2)) >= 0.001) || (Math.abs(0.320 - solver.getThroughput(1, 0)) >= 0.001)
				|| (Math.abs(2.421 - solver.getThroughput(1, 1)) >= 0.001) || (Math.abs(0.558 - solver.getThroughput(1, 2)) >= 0.001)
				|| (Math.abs(1.067 - solver.getThroughput(2, 0)) >= 0.001) || (Math.abs(1.794 - solver.getThroughput(2, 1)) >= 0.001)
				|| (Math.abs(0.448 - solver.getThroughput(2, 2)) >= 0.001) || (Math.abs(2.294 - solver.getThroughput(3, 0)) >= 0.001)
				|| (Math.abs(1.136 - solver.getThroughput(3, 1)) >= 0.001) || (Math.abs(0.137 - solver.getThroughput(3, 2)) >= 0.001)
				|| (Math.abs(0.133 - solver.getUtilization(0, 0)) >= 0.001) || (Math.abs(0.179 - solver.getUtilization(0, 1)) >= 0.001)
				|| (Math.abs(0.686 - solver.getUtilization(0, 2)) >= 0.001) || (Math.abs(0.032 - solver.getUtilization(1, 0)) >= 0.001)
				|| (Math.abs(0.484 - solver.getUtilization(1, 1)) >= 0.001) || (Math.abs(0.167 - solver.getUtilization(1, 2)) >= 0.001)
				|| (Math.abs(0.160 - solver.getUtilization(2, 0)) >= 0.001) || (Math.abs(0.538 - solver.getUtilization(2, 1)) >= 0.001)
				|| (Math.abs(0.201 - solver.getUtilization(2, 2)) >= 0.001) || (Math.abs(0.458 - solver.getUtilization(3, 0)) >= 0.001)
				|| (Math.abs(0.454 - solver.getUtilization(3, 1)) >= 0.001) || (Math.abs(0.082 - solver.getUtilization(3, 2)) >= 0.001)) {
			System.out.println("\nTest failed");
		} else {
			System.out.println("\nTest succeded");
		}
	}

	public static void testLI_2C_2S() {
		int nCust = 3;
		int nCent = 4;
		String[] name = new String[nCent];
		int[] type = new int[nCent];
		double[][][] timeMulti = new double[nCent][nCust][1];
		double[][] visitMulti = new double[nCent][nCust];
		int[] popVect = new int[nCust];

		for (int i = 0; i < nCent; i++) {
			name[i] = "Service Center " + i;
			type[i] = LI;
		}

		popVect[0] = 10;
		popVect[1] = 15;
		popVect[2] = 13;

		//not a very general test but I don't want to write a lot of data..
		for (int i = 0; i < nCent; i++) {
			double local = (i + 1) * 0.5;
			for (int j = 0; j < nCust; j++) {
				timeMulti[i][j][0] = local * (j + 1) * 0.1;
			}
		}

		visitMulti[0][0] = 1;
		visitMulti[0][1] = 0.6;
		visitMulti[0][2] = 5;
		visitMulti[1][0] = 0.12;
		visitMulti[1][1] = 0.81;
		visitMulti[1][2] = 0.61;
		visitMulti[2][0] = 0.4;
		visitMulti[2][1] = 0.6;
		visitMulti[2][2] = 0.49;
		visitMulti[3][0] = 0.86;
		visitMulti[3][1] = 0.38;
		visitMulti[3][2] = 0.15;

		SolverMultiClosedMVA solver = new SolverMultiClosedMVA(nCust, nCent);
		if (!solver.input(name, type, timeMulti, visitMulti, popVect)) {
			System.out.println("Vector Lengths not exact");
			return;
		}
		solver.solve();
		System.out.println("solved");
		System.out.print(solver);
		if ((Math.abs(2.667 - solver.getThroughput(0, 0)) >= 0.001) || (Math.abs(1.794 - solver.getThroughput(0, 1)) >= 0.001)
				|| (Math.abs(4.576 - solver.getThroughput(0, 2)) >= 0.001) || (Math.abs(0.320 - solver.getThroughput(1, 0)) >= 0.001)
				|| (Math.abs(2.421 - solver.getThroughput(1, 1)) >= 0.001) || (Math.abs(0.558 - solver.getThroughput(1, 2)) >= 0.001)
				|| (Math.abs(1.067 - solver.getThroughput(2, 0)) >= 0.001) || (Math.abs(1.794 - solver.getThroughput(2, 1)) >= 0.001)
				|| (Math.abs(0.448 - solver.getThroughput(2, 2)) >= 0.001) || (Math.abs(2.294 - solver.getThroughput(3, 0)) >= 0.001)
				|| (Math.abs(1.136 - solver.getThroughput(3, 1)) >= 0.001) || (Math.abs(0.137 - solver.getThroughput(3, 2)) >= 0.001)
				|| (Math.abs(0.133 - solver.getUtilization(0, 0)) >= 0.001) || (Math.abs(0.179 - solver.getUtilization(0, 1)) >= 0.001)
				|| (Math.abs(0.686 - solver.getUtilization(0, 2)) >= 0.001) || (Math.abs(0.032 - solver.getUtilization(1, 0)) >= 0.001)
				|| (Math.abs(0.484 - solver.getUtilization(1, 1)) >= 0.001) || (Math.abs(0.167 - solver.getUtilization(1, 2)) >= 0.001)
				|| (Math.abs(0.160 - solver.getUtilization(2, 0)) >= 0.001) || (Math.abs(0.538 - solver.getUtilization(2, 1)) >= 0.001)
				|| (Math.abs(0.201 - solver.getUtilization(2, 2)) >= 0.001) || (Math.abs(0.458 - solver.getUtilization(3, 0)) >= 0.001)
				|| (Math.abs(0.454 - solver.getUtilization(3, 1)) >= 0.001) || (Math.abs(0.082 - solver.getUtilization(3, 2)) >= 0.001)) {
			System.out.println("\nTest failed");
		} else {
			System.out.println("\nTest succeded");
		}
	}

	/**

	 OLD

	public String toString() {
	    StringBuffer buf = new StringBuffer();
	    buf.append("\n------------------------------------");
	    buf.append("\nAnalysis with MVA Multiclass");
	    buf.append("\n");
	    buf.append("Throughput  ");
	    for (int c = 0; c < classes; c++)
	        buf.append("class " + (c + 1) + "    ");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("Resource " + m + "  ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(throughput[m][c], 6) + "   ");
	        buf.append("\n");
	    }

	    buf.append("\n\n");
	    buf.append("Queue Length  ");
	    for (int c = 0; c < classes; c++)
	        buf.append("class " + (c + 1) + "    ");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("Resource " + m + "  ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(queueLen[m][c], 6) + "   ");
	        buf.append("\n");
	    }

	    buf.append("\n\n");
	    buf.append("Res  Time  ");
	    for (int c = 0; c < classes; c++)
	        buf.append("class " + (c + 1) + "    ");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("Resource " + m + "  ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(residenceTime[m][c], 6) + "   ");
	        buf.append("\n");
	    }

	    buf.append("\n\n");
	    buf.append("Utilization ");
	    for (int c = 0; c < classes; c++)
	        buf.append("class " + (c + 1) + "    ");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("Resource " + m + "  ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(utilization[m][c], 6) + "   ");
	        buf.append("\n");
	    }
	    return buf.toString();
	}
	*/

	/*
	OLD

	NEW: Use MultiSolver toString


	public String toString() {
	    StringBuffer buf = new StringBuffer();
	    buf.append("\n------------------------------------");
	    buf.append("\nAnalysis with MVA Multiclass");

	    buf.append("\n\n");
	    buf.append("Throughput (X)");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("[   ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(throughput[m][c], 6) + "   ");
	        buf.append("]\n");
	    }

	    buf.append("\n\n");
	    buf.append("Queue Length (Q)");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("[   ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(queueLen[m][c], 6) + "   ");
	        buf.append("]\n");
	    }

	    buf.append("\n\n");
	    buf.append("Residence Time (R)");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("[   ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(residenceTime[m][c], 6) + "   ");
	        buf.append("]\n");
	    }

	    buf.append("\n\n");
	    buf.append("Utilization (U)");
	    buf.append("\n------------------------------------\n");
	    for (int m = 0; m < stations; m++) {
	        buf.append("[   ");
	        for (int c = 0; c < classes; c++)
	            buf.append(Printer.print(utilization[m][c], 6) + "   ");
	        buf.append("]\n");
	    }
	    return buf.toString();
	}
	*/

	/**
	 *
	 *
	 */
	private static void miotest(int N1, int N2) {
		int nCust = 2;
		int nCent = 3;
		String[] name = new String[nCent];
		int[] type = new int[nCent];
		double[][][] timeMulti = new double[nCent][nCust][1];
		double[][] visitMulti = new double[nCent][nCust];
		int[] popVect = new int[nCust];

		for (int i = 0; i < nCent; i++) {
			name[i] = "Service Center " + i;
			type[i] = LI;
		}

		popVect[0] = N1;
		popVect[1] = N2;

		timeMulti[0][0][0] = 100;
		timeMulti[0][1][0] = 50;
		timeMulti[1][0][0] = 50;
		timeMulti[1][1][0] = 90;
		//timeMulti[2][0][0] = 90;
		//timeMulti[2][1][0] = 70;

		visitMulti[0][0] = 1;
		visitMulti[0][1] = 1;
		visitMulti[1][0] = 1;
		visitMulti[1][1] = 1;
		visitMulti[2][0] = 1;
		visitMulti[2][1] = 1;

		SolverMultiClosedMVA model = new SolverMultiClosedMVA(nCust, nCent);
		if (!model.input(name, type, timeMulti, visitMulti, popVect)) {
			System.out.println("Vector Lengths not exact");
			return;
		}
		model.solve();
		/*System.out.print(N1+","+N2+",");
		  */
		for (int i = nCent - 1; i < nCent; i++) {
			/* System.out.print(model.getClsThroughput(0)+","+model.getClsThroughput(1)+","+model.getUtilization(0,0)+","+model.getUtilization(0,1)+","+(model.getUtilization(0,1)+model.getUtilization(0,0))+","+model.getUtilization(1,0)+","+
			         model.getUtilization(1,1)+","+(model.getUtilization(1,1)+model.getUtilization(1,0))+","+model.getQueueLen(0,0)+","+model.getQueueLen(0,1)+","+model.getAggrQueueLen(0)+","+
			         model.getQueueLen(1,0)+","+model.getQueueLen(1,1)+","+model.getAggrQueueLen(1));
			         */
			System.out.println(model.getClsThroughput(0) + " " + model.getClsThroughput(1));

		}
	}

}
