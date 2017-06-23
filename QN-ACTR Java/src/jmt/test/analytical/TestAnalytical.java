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

package jmt.test.analytical;

import java.util.Random;

import jmt.analytical.Solver;
import jmt.analytical.SolverMulti;
import jmt.analytical.SolverMultiClosedMVA;
import jmt.analytical.SolverMultiMixed;
import jmt.analytical.SolverMultiOpen;
import jmt.analytical.SolverSingleClosedMVA;

/**
 * @author Stefano
 * @version 7-giu-2004 10.01.14
 */
public class TestAnalytical {

	/**
	 * Test
	 */
	public static void testMultiOpenLI() {

		System.out.println("Sistema aperto con 2 classi");
		long start = System.currentTimeMillis();

		double[] lambda = new double[2];
		lambda[0] = 0.05;
		lambda[1] = 0.05;

		SolverMultiOpen mOpen = new SolverMultiOpen(2, 3, lambda);

		String[] name = { "Delay", "Disk1", "CPU" };
		int[] type = { Solver.DELAY, Solver.LI, Solver.LI, };
		double[][][] servt = new double[3][2][1];

		//station 1
		servt[0][0][0] = 5;
		servt[0][1][0] = 5;

		//station 2
		servt[1][0][0] = 0.005;
		servt[1][1][0] = 0.002;

		//station 3
		servt[2][0][0] = 0.015;
		servt[2][1][0] = 0.012;

		double[][] visits = new double[3][2];

		//station 1
		visits[0][0] = 1;
		visits[0][1] = 1;

		//station 2
		visits[1][0] = 70;
		visits[1][1] = 120;

		//station 3
		visits[2][0] = 35;
		visits[2][1] = 20;

		if (mOpen.input(name, type, servt, visits)) {
			if (mOpen.hasSufficientProcessingCapacity()) {
				mOpen.solve();
				System.out.println(mOpen);
				System.out.println("End Multi LI");
				long stop = System.currentTimeMillis();
				System.out.print("Tempo trascorso: " + (stop - start));
				System.out.println();
			} else {
				System.out.println("No sufficient processing capacity!");
			}
		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	 * Test
	 */
	public static void testMultiOpenLI2() {

		System.out.println("Sistema aperto con 3 classi");
		long start = System.currentTimeMillis();

		double[] lambda = new double[3];
		lambda[0] = 0.05;
		lambda[1] = 0.05;
		lambda[2] = 0.05;

		SolverMultiOpen mOpen = new SolverMultiOpen(3, 2, lambda);

		String[] name = { "Disk1", "CPU" };
		int[] type = { Solver.LI, Solver.LI, };
		double[][][] servt = new double[2][3][1];

		//station 1
		servt[0][0][0] = 0.005;
		servt[0][1][0] = 0.005;
		servt[0][2][0] = 0.005;

		//station 2
		servt[1][0][0] = 0.005;
		servt[1][1][0] = 0.002;
		servt[1][2][0] = 0.002;

		double[][] visits = new double[2][3];

		//station 1
		visits[0][0] = 1;
		visits[0][1] = 1;
		visits[0][2] = 1;

		//station 2
		visits[1][0] = 70;
		visits[1][1] = 120;
		visits[1][2] = 120;

		if (mOpen.input(name, type, servt, visits)) {
			if (mOpen.hasSufficientProcessingCapacity()) {
				mOpen.solve();
				System.out.println(mOpen);
				System.out.println("End Multi LI");
				long stop = System.currentTimeMillis();
				System.out.print("Tempo trascorso: " + (stop - start));
				System.out.println();
			} else {
				System.out.println("No sufficient processing capacity!");
			}
		} else {
			System.out.println("Wrong input!!");
		}

	}

	public static void testMultiClosedLI() {

		System.out.println("Sistema chiuso con 2 classi");
		long start = System.currentTimeMillis();

		SolverMultiClosedMVA mClosed = new SolverMultiClosedMVA(3, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI, };
		double[][][] servt = new double[2][3][1];

		int[] pop = new int[3];
		pop[0] = 10;
		pop[1] = 10;
		pop[2] = 10;

		//station 1
		servt[0][0][0] = 10;
		servt[0][1][0] = 5;
		servt[0][2][0] = 1;

		//station 2
		servt[1][0][0] = 5;
		servt[1][1][0] = 9;
		servt[1][2][0] = 1;

		double[][] visits = new double[2][3];

		//station 1
		visits[0][0] = 1;
		visits[0][1] = 1;
		visits[0][2] = 1;

		//station 2
		visits[1][0] = 1;
		visits[1][1] = 1;
		visits[1][2] = 1;

		if (mClosed.input(name, type, servt, visits, pop)) {
			mClosed.solve();
			System.out.println(mClosed);
			System.out.println("End Multi Closed LI");
			long stop = System.currentTimeMillis();
			System.out.print("Tempo trascorso: " + (stop - start));
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	public static void testMultiClosedLI_visits_not1() {

		System.out.println("Sistema chiuso con 2 classi");
		long start = System.currentTimeMillis();

		SolverMultiClosedMVA mClosed = new SolverMultiClosedMVA(2, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI, };
		double[][][] servt = new double[2][2][1];

		int[] pop = new int[2];
		pop[0] = 100;
		pop[1] = 50;

		//station 1
		servt[0][0][0] = 10;
		servt[0][1][0] = 5;

		//station 2
		servt[1][0][0] = 5;
		servt[1][1][0] = 9;

		double[][] visits = new double[2][2];

		//station 1
		visits[0][0] = 3;
		visits[0][1] = 4;

		//station 2
		visits[1][0] = 2;
		visits[1][1] = 5;

		if (mClosed.input(name, type, servt, visits, pop)) {
			mClosed.solve();
			System.out.println(mClosed);
			System.out.println("End Multi Closed LI");
			long stop = System.currentTimeMillis();
			System.out.print("Tempo trascorso: " + (stop - start));
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	public static void testMultiClosedLI_big() {

		System.out.println("Sistema chiuso con 4 classi");
		long start = System.currentTimeMillis();

		SolverMultiClosedMVA mClosed = new SolverMultiClosedMVA(4, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI, };
		double[][][] servt = new double[2][4][1];

		int[] pop = new int[4];
		pop[0] = 100;
		pop[1] = 100;
		pop[2] = 100;
		pop[3] = 100;

		//station 1
		servt[0][0][0] = 10;
		servt[0][1][0] = 5;
		servt[0][2][0] = 10;
		servt[0][3][0] = 5;

		//station 2
		servt[1][0][0] = 5;
		servt[1][1][0] = 9;
		servt[1][2][0] = 4;
		servt[1][3][0] = 5;

		double[][] visits = new double[2][4];

		//station 1
		visits[0][0] = 1;
		visits[0][1] = 1;
		visits[0][2] = 1;
		visits[0][3] = 1;

		//station 2
		visits[1][0] = 1;
		visits[1][1] = 1;
		visits[1][2] = 1;
		visits[1][3] = 1;

		if (mClosed.input(name, type, servt, visits, pop)) {
			mClosed.solve();
			System.out.println(mClosed);
			System.out.println("End Multi Closed LI");
			long stop = System.currentTimeMillis();
			System.out.print("Tempo trascorso: " + (stop - start));
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	 * Test
	 */
	public static void testMultiMixed() {

		System.out.println("Sistema misto con 2 classi");
		long start = System.currentTimeMillis();

		int[] classTypes = new int[2];
		classTypes[0] = SolverMulti.OPEN_CLASS;
		classTypes[1] = SolverMulti.CLOSED_CLASS;

		double[] classData = new double[2];
		classData[0] = 0.005;
		classData[1] = 10;

		SolverMultiMixed multiMixed = new SolverMultiMixed(2, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI };
		double[][][] servt = new double[2][2][1];

		//station 1
		servt[0][0][0] = 0.002;
		servt[0][1][0] = 0.005;

		//station 2
		servt[1][0][0] = 0.003;
		servt[1][1][0] = 0.004;

		double[][] visits = new double[2][2];

		//station 1
		visits[0][0] = 5;
		visits[0][1] = 5;

		//station 2
		visits[1][0] = 1;
		visits[1][1] = 3;

		if (multiMixed.input(name, type, servt, visits, classData, classTypes)) {

			multiMixed.solve();
			System.out.println(multiMixed);
			System.out.println("End Multi mixed");
			long stop = System.currentTimeMillis();
			long elapsed = stop - start;
			double totTime = elapsed / 1000;
			System.out.print("Tempo trascorso: ");
			System.out.print(totTime);
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	 * Test
	 */
	public static void testMultiMixed2() {

		System.out.println("Sistema misto con 2 classi");
		long start = System.currentTimeMillis();

		int[] classTypes = new int[2];
		classTypes[0] = SolverMulti.CLOSED_CLASS;
		classTypes[1] = SolverMulti.CLOSED_CLASS;

		double[] classData = new double[2];
		classData[0] = 5;
		classData[1] = 10;

		SolverMultiMixed multiMixed = new SolverMultiMixed(2, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI };
		double[][][] servt = new double[2][2][1];

		//station 1
		servt[0][0][0] = 0.002;
		servt[0][1][0] = 0.005;

		//station 2
		servt[1][0][0] = 0.003;
		servt[1][1][0] = 0.004;

		double[][] visits = new double[2][2];

		//station 1
		visits[0][0] = 5;
		visits[0][1] = 5;

		//station 2
		visits[1][0] = 1;
		visits[1][1] = 3;

		if (multiMixed.input(name, type, servt, visits, classData, classTypes)) {

			multiMixed.solve();
			System.out.println(multiMixed);
			System.out.println("End Multi mixed");
			long stop = System.currentTimeMillis();
			long elapsed = stop - start;
			double totTime = elapsed / 1000;
			System.out.print("Tempo trascorso: ");
			System.out.print(totTime);
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	 * Test
	 */
	public static void testMultiMixed3() {

		System.out.println("Sistema misto con 2 classi");
		long start = System.currentTimeMillis();

		int[] classTypes = new int[2];
		classTypes[0] = SolverMulti.CLOSED_CLASS;
		classTypes[1] = SolverMulti.OPEN_CLASS;

		double[] classData = new double[2];
		classData[0] = 5;
		classData[1] = 0.04;

		SolverMultiMixed multiMixed = new SolverMultiMixed(2, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.DELAY, Solver.DELAY };
		double[][][] servt = new double[2][2][1];

		//station 1
		servt[0][0][0] = 2;
		servt[0][1][0] = 3;

		//station 2
		servt[1][0][0] = 1;
		servt[1][1][0] = 8;

		double[][] visits = new double[2][2];

		//station 1
		visits[0][0] = 1;
		visits[0][1] = 1;

		//station 2
		visits[1][0] = 1;
		visits[1][1] = 1;

		if (multiMixed.input(name, type, servt, visits, classData, classTypes)) {

			multiMixed.solve();
			System.out.println(multiMixed);
			System.out.println("End Multi mixed");
			long stop = System.currentTimeMillis();
			long elapsed = stop - start;
			double totTime = elapsed / 1000;
			System.out.print("Tempo trascorso: ");
			System.out.print(totTime);
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	  * Test
	  */
	public static void testMultiMixed_4C_2S() {

		System.out.println("Sistema misto con 4 classi");
		long start = System.currentTimeMillis();

		int[] classTypes = new int[4];
		classTypes[0] = SolverMulti.OPEN_CLASS;
		classTypes[1] = SolverMulti.OPEN_CLASS;
		classTypes[2] = SolverMulti.CLOSED_CLASS;
		classTypes[3] = SolverMulti.CLOSED_CLASS;

		double[] classData = new double[4];
		classData[0] = 0.005;
		classData[1] = 0.008;
		classData[2] = 20;
		classData[3] = 10;

		SolverMultiMixed multiMixed = new SolverMultiMixed(4, 2);

		String[] name = { "Station1", "Station2" };
		int[] type = { Solver.LI, Solver.LI };
		double[][][] servt = new double[2][4][1];

		//station 1
		servt[0][0][0] = 5;
		servt[0][1][0] = 10;
		servt[0][2][0] = 10;
		servt[0][3][0] = 3;

		//station 2
		servt[1][0][0] = 4;
		servt[1][1][0] = 3;
		servt[1][2][0] = 5;
		servt[1][3][0] = 6;

		double[][] visits = new double[2][4];

		//station 1
		visits[0][0] = 2;
		visits[0][1] = 1;
		visits[0][2] = 4;
		visits[0][3] = 1;

		//station 2
		visits[1][0] = 3;
		visits[1][1] = 1;
		visits[1][2] = 3;
		visits[1][3] = 1;

		if (multiMixed.input(name, type, servt, visits, classData, classTypes)) {

			multiMixed.solve();
			System.out.println(multiMixed);
			System.out.println("End Multi mixed");
			long stop = System.currentTimeMillis();
			long elapsed = stop - start;
			double totTime = elapsed / 1000;
			System.out.print("Tempo trascorso: ");
			System.out.print(totTime);
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	/**
	 * Method used for testing LI models
	 */
	public static void testSingleClosedLI_1() {
		int n = 10;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 4);
		String[] name = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LI, Solver.LI, Solver.LI, };
		double[][] servt = { { 15 }, { 0.03 }, { 0.005 }, { 0.027 } };
		double[] visit = { 1, 70, 121, 50 };

		if (mva.input(name, type, servt, visit)) {
			//mva.solve();
			mva.solve(true);
		}
		//System.out.println(mva);
		System.out.println(mva.toString());
		System.out.println("End LI");
		long stop = System.currentTimeMillis();
		System.out.print("Tempo trascorso: " + (stop - start));

	}

	/**
	 * Method used for testing LI models
	 */
	public static void testSingleClosedLI_2() {
		int n = 100;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 4);
		String[] name = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LI, Solver.LI, Solver.LI, };
		double[][] servt = { { 15 }, { 0.03 }, { 0.005 }, { 0.027 } };
		double[] visit = { 1, 70, 121, 50 };

		if (mva.input(name, type, servt, visit)) {
			mva.solve();
		}
		System.out.println(mva);
		System.out.println("End LI");
		System.out.print("Tempo trascorso: " + (System.currentTimeMillis() - start) / 1000);

	}

	/**
	 * Method used for testing LI models
	 */
	public static void testSingleClosedLI_3() {
		int n = 5000000;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 4);
		String[] name = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LI, Solver.LI, Solver.LI, };
		//double[][] servt = {{15}, {0.03}, {0.005}, {0.027}};
		double[][] servt = { { 0.03 }, { 0.03 }, { 0.2 }, { 0.2 } };
		double[] visit = { 1, 1, 1, 1 };

		if (mva.input(name, type, servt, visit)) {
			mva.solve();
		}
		System.out.println(mva);
		System.out.println("End LI");
		System.out.print("Tempo trascorso: ");
		long elapsed = (System.currentTimeMillis() - start);
		System.out.println(elapsed);

	}

	/**
	 * Method used for testing LI models with intermediate results of mva
	 */
	public static void testSingleClosedLI_4() {
		int n = 3;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 4);
		String[] name = { "Delay", "CPU", "Disk1", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LI, Solver.LI, Solver.LI, };
		double[][] servt = { { 15 }, { 0.005 }, { 0.03 }, { 0.027 } };
		double[] visit = { 1, 121, 70, 50 };

		if (mva.input(name, type, servt, visit)) {
			//mva.solve();
			mva.solve(true);
			System.out.println(mva.toString());
			//mva.solve(false);
			//System.out.println(mva.toString());
		}

		System.out.println("End LI");
		long stop = System.currentTimeMillis();
		System.out.print("Tempo trascorso: " + (stop - start));

	}

	/**
	 * Method used for testing LI models with intermediate results of mva
	 */
	public static void testSingleClosedLI_scalability() {

		for (int pop = 20000; pop < 100000; pop = pop + 10000) {
			int n = pop;
			System.out.println("Sistema con: " + n + " customers");
			long start = System.currentTimeMillis();

			SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 4);
			String[] name = { "Delay", "CPU", "Disk1", "Disk2" };
			int[] type = { Solver.DELAY, Solver.LI, Solver.LI, Solver.LI, };
			double[][] servt = { { 15 }, { 0.005 }, { 0.03 }, { 0.027 } };
			double[] visit = { 1, 121, 70, 50 };

			if (mva.input(name, type, servt, visit)) {
				//mva.solve();
				mva.solve(true);
			}
			System.out.println(mva);
			//System.out.println(mva.intermediateToString());
			System.out.println("End LI");
			long stop = System.currentTimeMillis();
			System.out.print("Tempo trascorso: " + (stop - start));
			System.out.println();
		}

	}

	/**
	 * Method used for testing LD models
	 */
	public static void testSingleClosedLD() {

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(3, 4);
		String[] name = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LD, Solver.LI, Solver.LD };
		double[][] servt = { { 15 }, { 0, 0.03, 0.03, 0.03 }, { 0.005 }, { 0, 0.027, 0.027, 0.027 } };
		double[] visit = { 1, 70, 121, 50 };

		if (mva.input(name, type, servt, visit)) {
			long start = System.currentTimeMillis();
			mva.solve(false);
			long stop = System.currentTimeMillis();

			long elapsed = stop - start;
			System.out.println("Tempo trascorso...");
			System.out.println(Long.toString(elapsed));
			System.out.println(mva);
		}
		System.out.println("End LD");

	}

	/**
	 * Method used for testing LD models
	 */
	public static void testSingleClosedLD2() {
		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(3, 4);
		String[] name = { "Disk1", "Delay", "CPU", "Disk2" };
		int[] type = { Solver.LD, Solver.DELAY, Solver.LI, Solver.LD };
		double[][] servt = { { 0, 0.03, 0.03, 0.03 }, { 15 }, { 0.005 }, { 0, 0.027, 0.027, 0.027 } };
		double[] visit = { 70, 1, 121, 50 };

		if (mva.input(name, type, servt, visit)) {
			mva.solve(false);
		}

		System.out.println(mva);
		System.out.println("End LD");

	}

	/**
	 * Method used for testing LD models
	 */
	public static void testSingleClosedLD_step() {

		System.out.println("Customers: 1");

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(1, 4);
		String[] name = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type = { Solver.DELAY, Solver.LD, Solver.LI, Solver.LD };
		double[][] servt = { { 15 }, { 0, 0.03 }, { 0.005 }, { 0, 0.027 } };
		double[] visit = { 1, 70, 121, 50 };

		if (mva.input(name, type, servt, visit)) {
			mva.solve(false);
		}

		System.out.println(mva);

		System.out.println("Customers: 2");

		mva = new SolverSingleClosedMVA(2, 4);
		String[] name2 = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type2 = { Solver.DELAY, Solver.LD, Solver.LI, Solver.LD };
		double[][] servt2 = { { 15 }, { 0, 0.03, 0.03 }, { 0.005 }, { 0, 0.027, 0.027 } };
		double[] visit2 = { 1, 70, 121, 50 };

		if (mva.input(name2, type2, servt2, visit2)) {
			mva.solve(false);
		}

		System.out.println(mva);

		System.out.println("Customers: 3");

		mva = new SolverSingleClosedMVA(3, 4);
		String[] name3 = { "Delay", "Disk1", "CPU", "Disk2" };
		int[] type3 = { Solver.DELAY, Solver.LD, Solver.LI, Solver.LD };
		double[][] servt3 = { { 15 }, { 0, 0.03, 0.03, 0.03 }, { 0.005 }, { 0, 0.027, 0.027, 0.027 } };
		double[] visit3 = { 1, 70, 121, 50 };

		if (mva.input(name3, type3, servt3, visit3)) {
			mva.solve(false);
		}

		System.out.println(mva);
		System.out.println("End LD");

	}

	/**
	 * Method used for testing LD models
	 */
	public static void testSingleClosedLD_big() {
		int n = 50000; //customers

		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 2);
		String[] name = { "Delay", "Disk1" };
		int[] type = { Solver.DELAY, Solver.LD };

		double[] servLD = new double[n + 1];
		servLD[0] = 0;

		int i = 1;
		for (i = 1; i <= n; i++) {
			servLD[i] = 0.03;
		}

		double[][] servt = { { 15 }, servLD };
		double[] visit = { 1, 70 };

		long start2 = 0;
		if (mva.input(name, type, servt, visit)) {
			start2 = System.currentTimeMillis();
		}
		mva.solve(false);
		System.out.println(mva);

		long stop = System.currentTimeMillis();

		double elapsed = (double) (stop - start) / 1000;
		System.out.println("Tempo trascorso (allocazione+calcolo)...");
		System.out.println(elapsed);

		double elapsed2 = (double) (stop - start2) / 1000;
		System.out.println("Tempo trascorso (solo calcolo)...");
		System.out.println(elapsed2);

		System.out.println("End LD");

	}

	/**
	 * Method used for testing
	 */
	public static void testSingleClosedStability(int n) {
		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 3);
		String[] name = { "1", "2", "3" };
		int[] type = { Solver.LD, Solver.LD, Solver.LD };
		double[][] servt = new double[3][n + 1];
		double[] visit = { 1.0, 1.3, 2.5 };
		double p;

		for (int i = 1; i <= n; i++) {
			p = 1.0 / i;
			servt[0][i] = 0.020 * p;
			servt[1][i] = 0.024 * p;
			servt[2][i] = 0.007 * p;
		}

		if (mva.input(name, type, servt, visit)) {
			mva.solve();
		}
		System.out.println(mva);

		System.out.println("End Stability");
	}

	/**
	 * Method used for testing
	 */
	public static void testSingleClosedSpeed(int n) {
		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 5);
		String[] name = { "1", "2", "3", "4", "5" };
		int[] type = { Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD };
		double[][] servt = new double[5][n + 1];
		double[] visit = { 1.0, 1.3, 2.5, 1, 1 };
		double p;

		for (int i = 1; i <= n; i++) {
			p = 1.0 / i;
			servt[0][i] = 0.020 * p;
			servt[1][i] = 0.024 * p;
			servt[2][i] = 0.007 * p;
			servt[3][i] = 0.0001 * p;
			servt[4][i] = 0.0015 * p;
		}

		if (mva.input(name, type, servt, visit)) {
			mva.solve();
		}
		System.out.println(mva);

		System.out.println("End Stability");
	}

	/**
	 * Method used for testing
	 */
	public static void testSingleClosedSpeed2(int n) {
		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 10);
		String[] name = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" };
		int[] type = { Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD, Solver.LD };
		double[][] servt = new double[10][n + 1];
		double[] visit = { 1.0, 1.3, 2.5, 1, 1, 2, 4, 5, 2, 1.2 };
		double p;

		for (int i = 1; i <= n; i++) {
			p = 1.0 / i;
			servt[0][i] = 0.020 * p;
			servt[1][i] = 0.024 * p;
			servt[2][i] = 0.007 * p;
			servt[3][i] = 0.0001 * p;
			servt[4][i] = 0.0015 * p;
			servt[5][i] = 0.2 * p;
			servt[6][i] = 0.0214 * p;
			servt[7][i] = 0.074 * p;
			servt[8][i] = 0.0144 * p;
			servt[9][i] = 0.0213 * p;
		}

		if (mva.input(name, type, servt, visit)) {
			mva.solve();
		}
		System.out.println(mva);

		System.out.println("End Stability");
	}

	/**
	 * Method used for testing LI models
	 */
	public static void testSingleClosedLI_5() {
		int n = 5;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 1);
		String[] name = { "Disk1" };
		int[] type = { Solver.LI };
		double[][] servt = { { 2.65 } };
		double[] visit = { 1 };

		if (mva.input(name, type, servt, visit)) {
			//mva.solve();
			mva.solve(true);
		}
		//System.out.println(mva);
		System.out.println(mva.toString());
		System.out.println("End LI");
		long stop = System.currentTimeMillis();
		System.out.print("Tempo trascorso: " + (stop - start));

	}

	/**
	 * Method used for testing LI models
	 */
	public static void testSingleClosedLI_6() {
		int n = 5;
		System.out.println("Sistema con: " + n + " customers");
		long start = System.currentTimeMillis();

		SolverSingleClosedMVA mva = new SolverSingleClosedMVA(n, 2);
		String[] name = { "Disk1", "Disk2" };
		int[] type = { Solver.DELAY, Solver.DELAY };
		double[][] servt = new double[2][1];
		servt[0][0] = 2 / (1 - 0.12);
		servt[1][0] = 1 / (1 - 0.32);
		double[] visit = { 1, 1 };

		if (mva.input(name, type, servt, visit)) {
			//mva.solve();
			mva.solve(false);
		}
		//System.out.println(mva);
		System.out.println(mva.toString());
		System.out.println("End LI");
		long stop = System.currentTimeMillis();
		System.out.print("Tempo trascorso: " + (stop - start));

	}

	//---------------------@author Giuliano-------------------------------------//

	/**
	 * Generates random numbers to fill the matrix of service times (LI case)
	 * @param M number of stations
	 * @param R number of classes
	 * @return the matrix with service times
	 */
	public static double[][][] randL(int M, int R) {
		Random randgen = new Random();
		double[][][] servicetimes = new double[M][R][1];
		for (int i = 1; i <= M; i++) {
			for (int r = 1; r <= R; r++) {
				servicetimes[i - 1][r - 1][0] = 1 + Math.abs(randgen.nextInt(99));
			}
		}
		return servicetimes;
	}

	/**
	 * solves a multiclosed model
	 * @param M number of stations
	 * @param R number of classes
	 * @param L matrix of service times
	 * @param N array of class populations
	 */
	public static void solveClosedPFQN(int M, int R, double[][][] L, int[] N) {
		long tic = System.currentTimeMillis();
		SolverMultiClosedMVA mClosed = new SolverMultiClosedMVA(R, M);

		String[] name = new String[M];
		for (int i = 1; i <= M; i++) {
			name[i - 1] = "";
		}

		int[] type = new int[M];

		for (int i = 1; i <= M; i++) {
			type[i - 1] = Solver.LI;
		}

		int n = N.length;
		double[][] visits = new double[M][n];
		for (int i = 1; i <= M; i++) {
			for (int j = 1; j <= n; j++) {
				visits[i - 1][j - 1] = 1;
			}
		}

		if (mClosed.input(name, type, L, visits, N)) {
			mClosed.solve();
			System.out.println(mClosed);
			System.out.println("End Multi Closed LI");
			long toc = System.currentTimeMillis();
			System.out.print("Tempo trascorso: " + (toc - tic) + "ms");
			System.out.println();

		} else {
			System.out.println("Wrong input!!");
		}

	}

	public static void testJu() {

		int[] N = { 500, 500, 50 };
		int M = 2;
		int R = 3;
		solveClosedPFQN(M, R, randL(M, R), N);
		return;

	}

	//---------------------end @author Giuliano-------------------------------------//

	public static void main(String[] args) {

		//testMultiClosedLI();
		//testSingleClosedLI_6();
		//testSingleClosedLD();

		testSingleClosedLD_big();

		//testMultiOpenLI();
		//testMultiOpenLI2();
		//testMultiClosedLI();
		//testMultiClosedLI_big();
		//testMultiClosedLI_visits_not1();
		//testMultiMixed3();
		//testMultiMixed_4C_2S();
		return;

	}

}
