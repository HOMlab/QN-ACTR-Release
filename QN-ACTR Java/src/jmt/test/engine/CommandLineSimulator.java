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

package jmt.test.engine;

import java.io.FileNotFoundException;
import java.io.IOException;

import jmt.common.exception.LoadException;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.simEngine.SimLoader;
import jmt.engine.simEngine.Simulation;

/**
 * @author Stefano
 * @version 4-ago-2004 16.15.19
 */
public class CommandLineSimulator {

	public static void testFixedPath(String path) {
		long start, stop, elapsed;

		try {
			//File model = new File(path);
			//FileInputStream is = new FileInputStream(model);
			SimLoader loader = new SimLoader(path);
			Simulation sim = loader.getSim();
			sim.initialize();

			start = System.currentTimeMillis();
			sim.run();
			stop = System.currentTimeMillis();
			elapsed = stop - start;
			//System.out.println("Duration execution "+i+": ");
			//System.out.println(Long.toString(elapsed));
		} catch (FileNotFoundException e) {
			System.out.println("Errore, file non trovato...");
			return;
		} catch (LoadException e) {
			System.out.println("Errore, file non caricato...");
			e.printStackTrace();
			return;
		} catch (IOException e) {
			System.out.println("Errore, modello non inizializzato...");
			return;
		} catch (Exception e) {

			System.out.println("Errore, simulazione fallita...");
			e.printStackTrace();
			return;
		}

	}

	public static void testFixedPathAndSeed(String path, long seed) {
		long start, stop, elapsed;

		try {
			//File model = new File(path);
			//FileInputStream is = new FileInputStream(model);
			SimLoader loader = new SimLoader(path);
			Simulation sim = loader.getSim();

			//TODO: fixed seed
			sim.setRandomEngineSeed(seed);

			sim.initialize();

			start = System.currentTimeMillis();
			sim.run();
			stop = System.currentTimeMillis();
			elapsed = stop - start;
			//System.out.println("Duration execution "+i+": ");
			//System.out.println(Long.toString(elapsed));
		} catch (FileNotFoundException e) {
			System.out.println("Errore, file non trovato...");
			return;
		} catch (LoadException e) {
			System.out.println("Errore, file non caricato...");
			e.printStackTrace();
			return;
		} catch (IOException e) {
			System.out.println("Errore, modello non inizializzato...");
			return;
		} catch (Exception e) {

			System.out.println("Errore, simulazione fallita...");
			e.printStackTrace();
			return;
		}

	}

	public static void test1() {
		long start, stop, elapsed;

		String path = "D://JMTtest//sim_solverstep0.xml";
		//String path = "D://JMTtest//sim_model_with_blocking_open_drop.xml";
		//String path = "D://JMTtest//sim_jsim_randomModel_open_1.xml";

		try {
			//File model = new File(path);
			//FileInputStream is = new FileInputStream(model);
			SimLoader loader = new SimLoader(path);
			Simulation sim = loader.getSim();
			sim.initialize();
			start = System.currentTimeMillis();
			sim.run();
			stop = System.currentTimeMillis();
			elapsed = stop - start;
			//System.out.println("Duration execution "+i+": ");
			System.out.println(Long.toString(elapsed));
		} catch (FileNotFoundException e) {
			System.out.println("Errore, file non trovato...");
			return;
		} catch (LoadException e) {
			System.out.println("Errore, file non caricato...");
			e.printStackTrace();
			return;
		} catch (IOException e) {
			System.out.println("Errore, modello non inizializzato...");
			return;
		} catch (Exception e) {

			System.out.println("Errore, simulazione fallita...");
			e.printStackTrace();
			return;
		}

	}

	public static void test2() {

		try {

			SimLoader loader = new SimLoader("D://sim_test_2open.xml");
			Simulation sim = loader.getSim();
			sim.initialize();
			sim.run();
			double elapsed = NetSystem.getElapsedTime();
			System.out.println(elapsed);

		} catch (FileNotFoundException e) {
			System.out.println("Errore, file non trovato...");
			e.printStackTrace();
			return;
		} catch (LoadException e) {
			System.out.println("Errore, file non caricato...");
			e.printStackTrace();
			return;
		} catch (IOException e) {
			System.out.println("Errore, modello non inizializzato...");
			e.printStackTrace();
			return;
		} catch (Exception e) {
			System.out.println("Errore, simulazione fallita...");
			e.printStackTrace();
			return;
		}

	}

	public static void testSimulationTime() {

		int N = 5;
		long[] duration = new long[N];
		long start, stop, elapsed;
		long tot = 0;

		for (int i = 0; i < N; i++) {

			try {

				SimLoader loader = new SimLoader("D://sim_prova.xml");
				Simulation sim = loader.getSim();
				sim.initialize();
				start = System.currentTimeMillis();

				sim.run();

				stop = System.currentTimeMillis();
				elapsed = stop - start;
				//System.out.println("Duration execution "+i+": ");
				System.out.println(Long.toString(elapsed));

				duration[i] = elapsed;
				tot += elapsed;

			} catch (FileNotFoundException e) {
				System.out.println("Errore, file non trovato...");
				e.printStackTrace();
				return;
			} catch (LoadException e) {
				System.out.println("Errore, file non caricato...");
				e.printStackTrace();
				return;
			} catch (IOException e) {
				System.out.println("Errore, modello non inizializzato...");
				e.printStackTrace();
				return;
			} catch (Exception e) {
				System.out.println("Errore, simulazione fallita...");
				e.printStackTrace();
				return;
			}
		}

		long mean = tot / N;
		System.out.println("Mean: ");
		System.out.println(Long.toString(mean));

	}

	public static void main(String[] argsv) {
		//test1();
		//test2();
		//testSimulationTime();

		//String path = "D://JMTtest//simulatore_systemR_closed.xml";
		//String path = "D://JMTtest//simulatore_systemR_open.xml";
		//String path = "D://JMTtest//simulatore_closed_par_.xml";

		/*
		for (int i = 1; i < 4; i++) {
		    //String path = "D://JMTtest//simulatore_mm1k_" + i +  ".xml";
		    String path = "D://JMTtest//regione_mm1k_" + i +  ".xml";
		    testFixedPathAndSeed(path, 232323);
		}
		*/

		//String path = "D://JMTtest//misure_aggreg.xml";
		String path = "D://JMTtest//verbose.xml";
		testFixedPathAndSeed(path, 232323);

	}

}
