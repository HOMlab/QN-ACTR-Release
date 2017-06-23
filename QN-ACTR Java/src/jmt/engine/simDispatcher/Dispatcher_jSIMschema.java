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

package jmt.engine.simDispatcher;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.Vector;

import javax.swing.JOptionPane;

import jmt.common.exception.LoadException;
import jmt.common.exception.NetException;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.QueueNetwork;
import jmt.engine.dataAnalysis.Measure;
import jmt.engine.dataAnalysis.TempMeasure;
import jmt.engine.log.JSimLogger;
import jmt.engine.random.engine.RandomEngine;
import jmt.engine.simEngine.SimLoader;
import jmt.engine.simEngine.Simulation;

/**
 * Receives the absolute path of a xml file which describes the model using the
 * SIMmodeldefinition.xsd schema.
 * The simulation results, at the end, are converted and
 * are inserted in the original file.
 *
 * @author Stefano Omini
 * Modified by Bertoli Marco
 *
 * Modified by Francesco D'Aquino
 */
public class Dispatcher_jSIMschema {

	//xml files containing model definition, simmodel definition and sim results
	File simModelDefinition;
	File simResults;

	//path of the xml files
	String simModelDefinitionPath;
	String simResultsPath;

	//true if the simulation results have been already obtained, false otherwise
	boolean resultsObtained = false;

	//if true, the simulation seed
	private boolean automaticSeed = true;
	//if true, the simulation seed
	private long simulationSeed;

	private JSimLogger logger = JSimLogger.getLogger(JSimLogger.STD_LOGGER);

	//used to compute the progress of the simulation
	private boolean simStarted = false;
	private boolean simFinished = false;
	private boolean simPaused = false;
	private QueueNetwork net;

	//used to store temp measures info
	private TempMeasure[] tempMeasures = null;

	// used to store progress time - Bertoli Marco
	private double progressTime;
	private Vector<Integer> measuresToAbort = new Vector<Integer>(); // Vector with measures to abort at refresh

	private long maxDuration = -1;

	private Simulation sim;

	/**
	 * This constructor receives the absolute path of the xml file containing
	 * the model to be solved.
	 * The model must be described using the SIMmodeldefinition.xsd schema
	 *
	 * @param absolutePath absolute path of the xml file containing the
	 * model to be solved
	 */
	public Dispatcher_jSIMschema(String absolutePath) {
		simModelDefinitionPath = absolutePath;
		simModelDefinition = new File(absolutePath);
	}

	/**
	 * This constructor receives the absolute path of the xml file containing
	 * the model to be solved.
	 * The model must be described using the SIMmodeldefinition.xsd schema
	 *
	 * @param model the xml file containing the
	 * model to be solved
	 */
	public Dispatcher_jSIMschema(File model) {
		simModelDefinition = model;
		simModelDefinitionPath = model.getAbsolutePath();
	}

	/**
	 * By invoking this method, simulation will generate a random simulation seed
	 */
	public void automaticSimulationSeed() {
		automaticSeed = true;
	}

	/**
	 * Specifies the seed to be used in the simulation.
	 * @param seed the simulation seed
	 */
	public void setSimulationSeed(long seed) {
		simulationSeed = seed;
		automaticSeed = false;
	}

	/**
	 * Sets simulation max duration time
	 * @param durationMillis maximum time in milliseconds
	 */
	public void setSimulationMaxDuration(long durationMillis) {
		maxDuration = durationMillis;
	}

	public boolean solveModel() throws IOException, LoadException, NetException, Exception {

		/*********************SIM DEFINITION MODEL*********************/

		// does model file exist??
		if (!simModelDefinition.exists()) {
			//the passed file does not exist
			logger.error("The sim model file " + simModelDefinitionPath + " does not exist...");
			return false;
		} else {
			logger.debug("Sim model definition path: " + simModelDefinitionPath);
		}

		/*********************SIMULATION LOADING AND RUNNING*********************/

		//now prepare simulation
		//try {
		
		
		SimLoader simLoader = new SimLoader(simModelDefinitionPath);
		sim = simLoader.getSim();
		// Sets simulation max duration (if specified)
		if (maxDuration > 0) {
			sim.setMaxSimulationTime(maxDuration);
		}

		//sets in the Simulation object the path of the
		//xml model definition
		sim.setXmlSimModelDefPath(simModelDefinitionPath);

		if (automaticSeed) {
			// Generate only positive integers (to facilitate replay in the gui)
			simulationSeed = RandomEngine.makeDefault().nextInt();
			if (simulationSeed < 0) {
				simulationSeed = -simulationSeed;
			}
		}

		sim.setRandomEngineSeed(simulationSeed);
		sim.initialize();

		logger.debug("jSIM correctly initialized with simulation seed = " + simulationSeed);

		//find QueueNetwork reference
		net = sim.getNetwork();

		long start, stop, elapsed;

		simStarted = true;
		//Start time
		start = System.currentTimeMillis();

		//run simulation
		sim.run();

		// /stop time
		stop = System.currentTimeMillis();
		elapsed = (stop - start) / 1000;

		simPaused = false;
		simFinished = true;

		logger.info("Model " + simModelDefinitionPath + " solved by jSIM in " + Double.toString(elapsed) + " seconds");
		/*
		        } catch (IOException e) {
		            e.printStackTrace();
		            logger.error(e.getMessage());
		            return false;
		        } catch (LoadException e) {
		            e.printStackTrace();
		            logger.error(e.getMessage());
		            return false;
		        } catch (Exception e) {
		            e.printStackTrace();
		            logger.error(e.getMessage());
		            return false;
		        }   */

		return true;
	}

	public boolean solveHandlingExceptions() {
		try {
			return solveModel();
		} catch (Exception ex) {
			ex.printStackTrace();
			logger.error(ex.getMessage());
			return false;
		}
	}

	/**
	 * Tells if simulation was started
	 * @return true iff simulation was started
	 */
	public boolean isStarted() {
		return simStarted;
	}

	/**
	 * Tells if simulation was finished
	 * @return true iff simulation was finished
	 */
	public boolean isFinished() {
		return simFinished;
	}

	/**
	 * Tells if simulation was paused
	 * @return true iff simulation was paused
	 */
	public boolean isPaused() {
		return simPaused;
	}

	/**
	 * allows to know the state of simulation. Value is calculated at every
	 * <code>refreshTempMeasures()</code> to avoid pausing simulation two times.
	 * @return the percentage of progress (= number of computed confidence intervals /
	 * total number of required confidence intervals)
	 */
	public double checkSimProgress() {
		if (!simStarted) {
			//not started yet
			return 0.0;
		}
		if (simFinished) {
			//already finished
			return 1.0;
		}
		return progressTime;
	}

	/**
	 * Pauses the simulation, refreshes temp measures and then restarts simulation
	 */
	public synchronized void refreshTempMeasures() {

		//simulation not started yet
		if (!simStarted) {
			return;
		}

		//pauses the computation
		if (pauseSim()) {
			// Aborts measures (if any)
			while (!measuresToAbort.isEmpty()) {
				int index = measuresToAbort.remove(0).intValue();
				tempMeasures[index].abort();
			}

			//at the first execution, measures must be retrieved
			if (tempMeasures == null) {
				if (net == null) {
					return;
				} else {
					//gets the measures defined for this queue network
					LinkedList measures = net.getMeasures();

					//creates the array of temp measures
					tempMeasures = new TempMeasure[measures.size()];

					for (int m = 0; m < measures.size(); m++) {
						tempMeasures[m] = new TempMeasure((Measure) measures.get(m));
						tempMeasures[m].refreshMeasure();
					}
				}
			} else {
				//temp measures have already been retrieved
				//only refresh temp values
				for (TempMeasure tempMeasure : tempMeasures) {
					tempMeasure.refreshMeasure();
				}
			}
			// Updates simulation progress time
			try {
				progressTime = NetSystem.checkProgress(net);
			} catch (NetException e) {
				progressTime = 0.0;
			}

			//restart computation
			restartSim();

		} else if (simFinished) { // Gets last value for each measure
			for (TempMeasure tempMeasure : tempMeasures) {
				tempMeasure.refreshMeasure();
			}
		}
	}

	/**
	 * gets the temp measures
	 * @return the temp measures
	 */
	public TempMeasure[] getTempMeasures() {
		return tempMeasures;
	}

	public synchronized boolean abortAllMeasures() {
		if (!simStarted) {
			return false;
		}

		if (pauseSim()) {
			refreshTempMeasures();

			logger.debug("All remaining measures will be aborted");
			for (int m = 0; m < tempMeasures.length; m++) {
				abortMeasure(m);
			}

			return true;

		} else {
			return false;
		}
	}

	public synchronized boolean killSimulation() {
		//simulation not started yet
		if (!simStarted) {
			return false;
		}
		try {
			NetSystem.terminate();
		} catch (NetException nex) {
			nex.printStackTrace();
		}
		return true;
	}

	public synchronized boolean abortMeasure(int index) {
		boolean success = false;

		if (pauseSim()) {
			refreshTempMeasures();

			if ((0 <= index) && (index <= tempMeasures.length)) {
				success = tempMeasures[index].abort();
			} else {
				success = false;
			}
			restartSim();
		}

		return success;
	}

	/**
	 * Aborts a measure at next measure refresh... Used to avoid to stop simulation too frequently
	 * Author: Bertoli Marco
	 */
	public synchronized void abortMeasureAtRefresh(int measureIndex) {
		measuresToAbort.add(new Integer(measureIndex));
	}

	/**
	 * Prints on System.out the temp values of measures that haven't finished yet
	 */
	public void printTempMeasures() {

		if (pauseSim()) {
			if (tempMeasures != null) {
				for (TempMeasure tempMeasure : tempMeasures) {
					TempMeasure temp = tempMeasure;
					if (!temp.isFinished()) {
						System.out.println(temp.getName() + ": " + Double.toString(temp.getTempMean()) + " " + temp.getNsamples());
					}
				}
			}
			restartSim();
		}

	}

	/**
	 * Pauses the simulation
	 * @return true if the operation was successful, false otherwise (for example
	 * simulation not started or already finished)
	 */
	public synchronized boolean pauseSim() {
		simPaused = true;
		return NetSystem.pause();
	}

	/**
	 * Resumes the simulation
	 * @return true if the operation was successful, false otherwise (for example
	 * simulation not started or already finished)
	 */
	public synchronized boolean restartSim() {
		simPaused = false;
		return NetSystem.restartFromPause();
	}

	//Francesco D'Aquino
	public Simulation getSimulation() {
		return sim;
	}

	//------------------------TEST-----------------------------------//

	public static void testSimulationTime() {

		int N = 5;
		long[] duration = new long[N];
		long tot = 0;

		for (int i = 0; i < N; i++) {

			String path = "D:\\JMTtest\\prova" + i + ".xml";

			Dispatcher_jSIMschema disp = new Dispatcher_jSIMschema(path);
			long start, stop, elapsed;

			start = System.currentTimeMillis();

			if (disp.solveHandlingExceptions()) {
				stop = System.currentTimeMillis();
				elapsed = stop - start;
				//System.out.println("Duration execution "+i+": ");
				System.out.println(Long.toString(elapsed));

				duration[i] = elapsed;
				tot += elapsed;

			} else {
				System.out.println("Error!!");
			}
		}

		long mean = tot / N;
		System.out.println("Mean: ");
		System.out.println(Long.toString(mean));

	}

	/**
	 * @return the output XML file with the results of the simulation.
	 */
	public File getOutputFile() {
		return sim.getOutputFile();
	}

	public static void testSolution() {
		//Dispatcher disp = new Dispatcher("D:\\test_2open.xml");
		Dispatcher_jSIMschema disp = new Dispatcher_jSIMschema("D:\\JMTtest\\solverstep0.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\solverstep10.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\prova.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\test_base.xml");

		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking_2.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking_open_drop.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\preload.xml");
		disp.setSimulationSeed(23000);

		long start, stop, elapsed;
		start = System.currentTimeMillis();

		if (disp.solveHandlingExceptions()) {
			stop = System.currentTimeMillis();
			elapsed = stop - start;
			System.out.println("OK - Simulation time: ");
			System.out.println(Long.toString(elapsed));
		} else {
			System.out.println("Error!!");
		}

	}

}
