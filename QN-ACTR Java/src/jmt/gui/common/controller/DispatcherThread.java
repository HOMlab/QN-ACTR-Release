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

package jmt.gui.common.controller;

import java.awt.Cursor;
import java.io.File;

import jmt.engine.simDispatcher.Dispatcher_jSIMschema;
import jmt.gui.common.definitions.AbortMeasure;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.ResultsModel;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.panels.ResultsWindow;
import jmt.gui.jmodel.controller.Mediator;
import jmt.gui.jmodel.controller.SimulationStateChecker;

/**
 * <p>Title: Dispatcher Thread</p>
 * <p>Description: This thread is responsable for dispatching simulation. If simulation max time has
 * been specified, an inner thread will be used to stop simulation after that time. We don't rely
 * on engine method as it doesn't appear to work properly.</p>
 * 
 * @author Bertoli Marco
 *         Date: 8-set-2005
 *         Time: 11.40.46
 *
 * Modified by Francesco D'Aquino 9/11/2005
 */
public class DispatcherThread extends Thread implements AbortMeasure {
	private SimulationStateChecker simStateChecker;
	private Dispatcher_jSIMschema simulator;
	private SimulationDefinition sd;
	private GuiInterface gui;
	private TimerThread timer = null;
	private PollerThread poller = null;
	ResultsModel results;

	/**
	 * Construct a new Dispatcher Thread
	 * @param gui reference to current gui object (can be JSIMMain or Mediator)
	 * @param sd Reference to Simulation Definition data structure
	 * @param results data structure where simulation results should be saved
	 */
	public DispatcherThread(GuiInterface gui, SimulationDefinition sd, ResultsModel results) {
		this.sd = sd;
		this.gui = gui;
		this.results = results;
		poller = new PollerThread(sd.getPollingInterval(), this);
		// Avoid hanging the system during execution
		this.setPriority(Thread.MIN_PRIORITY);
	}

	/**
	 * Run method. This method will simply start simualtion, it is designed to be called only
	 * after startSimulation has been called.
	 */
	@Override
	public void run() {
		if (timer != null) {
			timer.start();
		}
		poller.start();
		try {
			simulator.solveModel();
		} catch (OutOfMemoryError err) {
			simulator.abortAllMeasures();
			simulator.killSimulation();
			gui.showErrorMessage("Out of memory error. Try to run Java Virtual Machine with more heap size (-Xmx<num>m)");
		} catch (Exception ex) {
			simulator.abortAllMeasures();
			simulator.killSimulation();
			gui.handleException(ex);
		}
		if (timer != null) {
			timer.kill();
		}
		gui.changeSimActionsState(true, false, false);
		results.refresh(1);

		// Removes output file, if it was created.
		if (simulator.getOutputFile() != null) {
			if (!simulator.getOutputFile().delete()) {
				simulator.getOutputFile().deleteOnExit();
			}
		}
	}

	/**
	 * This method starts simualtion, given simulation XML file
	 * @param simulationFile file where simulation model is stored
	 */
	public void startSimulation(File simulationFile) {
		simulator = new Dispatcher_jSIMschema(simulationFile);
		// ----------- Francesco D'Aquino ---------------
		if (sd.isAnimationEnabled()) {
			//if it is the first time create a new simStateChecker
			simStateChecker = new SimulationStateChecker((Mediator) gui, simulator);

		}
		// ----------- end Francesco D'Aquino -----------
		// If needed sets simulation seed
		if (!sd.getUseRandomSeed()) {
			simulator.setSimulationSeed(sd.getSimulationSeed().longValue());
		}
		// If a time maximum time is specified, initialize all stuff related
		if (sd.getMaximumDuration().longValue() > 0) {
			timer = new TimerThread(this, sd.getMaximumDuration().doubleValue());
		}
		gui.changeSimActionsState(false, true, true);
		start();
	}

	/**
	 * Pauses current simulation only if it was already started
	 */
	public void pauseSimulation() {
		if (simulator != null) {
			if (timer != null) {
				timer.pause();
			}
			gui.changeSimActionsState(true, false, true);
			simulator.pauseSim();
		}
	}

	/**
	 * Stops current simulation, forcing abort of all measures,
	 * only if it was already started
	 */
	public void stopSimulation() {
		if (simulator != null) {
			if (timer != null) {
				timer.kill();
			}
			gui.changeSimActionsState(true, false, false);
			simulator.abortAllMeasures();
		}
	}

	/**
	 * Restarts current simulation, forcing abort of all measures,
	 * only if it was already started
	 */
	public void restartSimulation() {
		if (simulator != null) {
			if (timer != null) {
				timer.restart();
			}
			gui.changeSimActionsState(false, true, true);
			simulator.restartSim();
		}
	}

	/**
	 * Aborts a measure, given its index
	 * @param index index of the measure to be aborted
	 */
	public void abortMeasure(int index) {
		simulator.abortMeasureAtRefresh(index);
	}

	/**
	 * Inner thread used to poll
	 */
	protected class PollerThread extends Thread {
		protected long interval;
		protected DispatcherThread dispatcher;
		protected boolean initialized = false;

		public PollerThread(double seconds, DispatcherThread dispatcher) {
			interval = Math.round(seconds * 1000);
			this.dispatcher = dispatcher;
		}

		@Override
		public void run() {
			if (gui instanceof Mediator) {
				((Mediator) gui).setGraphCursor(new Cursor(Cursor.WAIT_CURSOR));
			}
			// While simulation is not finished, polls at given intervals
			while (!dispatcher.simulator.isFinished()) {
				// Wait to collect results until simulation is really started
				while (!dispatcher.simulator.isStarted() && !dispatcher.simulator.isFinished()) {
					try {
						sleep(500);
					} catch (InterruptedException ex) {
						System.err.println("Unexpected InterruptException in PollerThread");
					}
				}
				// If it's first poll, initialize Results data structure
				if (!initialized) {
					if (gui instanceof Mediator) {
						((Mediator) gui).setGraphCursor(new Cursor(Cursor.DEFAULT_CURSOR));
					}
					dispatcher.simulator.refreshTempMeasures();
					dispatcher.results.setTempMeasures(dispatcher.simulator.getTempMeasures(), dispatcher.simulator.checkSimProgress());
					initialized = true;
					// Sets ResultsWindow
					ResultsWindow rw = new ResultsWindow(results, dispatcher);
					gui.setResultsWindow(rw);
					// Moves it to lower right corner if animation is enabled
					if (sd.isAnimationEnabled()) {
						rw.moveToLowerRightCorner();
					}
					// Shows it
					gui.showResultsWindow();
				}
				// If it's next one, refresh stored tempMeasures (if simulation is started and not paused)
				else if (!dispatcher.simulator.isPaused()) {
					dispatcher.simulator.refreshTempMeasures();
					double progress = dispatcher.simulator.checkSimProgress();
					// Progress is the biggest between simulation progress and timer progress
					if (timer != null && timer.getElapsedPercentage() > progress) {
						progress = timer.getElapsedPercentage();
					}
					dispatcher.results.refresh(progress);
					// ------------ Francesco D'Aquino -------------------
					if (sd.isAnimationEnabled()) {
						if (!simStateChecker.isInitialized()) {
							simStateChecker.initialize();
						}
						simStateChecker.getModelState();
						//simStateChecker.print();
						simStateChecker.forceDraw();
					}
					// -------------end Francesco D'Aquino ---------------
				}

				// Wait for polling interval
				try {
					sleep(interval);
				} catch (InterruptedException ex) {
					System.out.println("Error: Poller thread interrupted unexpectedly...");
				}
			}
			// Simulation is finished
			if (sd.isAnimationEnabled()) {
				simStateChecker.forceRepaint();
				//gui.showResultsWindow();
			}
		}
	}

	/**
	 * An inner thread used to stop simulation after timeout elapsed
	 */
	protected class TimerThread extends Thread {
		protected long residualTime, totalTime;
		protected DispatcherThread dispatcher;
		protected long initialTime;
		// End means normal end of measures, kill forced one
		protected boolean end = false;
		protected boolean killed = false;
		protected boolean paused = false;
		protected final Object lock = new Object();

		public TimerThread(DispatcherThread dispatcher, double maxDuration) {
			this.dispatcher = dispatcher;
			this.residualTime = this.totalTime = Math.round(maxDuration * 1000);
		}

		/**
		 * Thread run's method. It will wait until maxDuration has elapsed, then stops
		 * simulation.
		 */
		@Override
		public void run() {
			initialTime = System.currentTimeMillis();
			// Wait for residual time. This is true unless pause button is pressed.
			while (!end && !killed) {
				initialTime = System.currentTimeMillis();
				try {
					synchronized (this) {
						wait(residualTime);
					}
				} catch (InterruptedException e) {
					System.out.println("Error: Timer thread interrupted unexpectedly...");
				}

				residualTime = residualTime - (System.currentTimeMillis() - initialTime);
				if (residualTime <= 100) {
					end = true;
				}

				synchronized (lock) {
					try {
						// If end=false we have to wait until restart or kill is told us
						if (!end && !killed) {
							lock.wait();
						}
					} catch (InterruptedException e) {
						System.out.println("Error: Timer thread interrupted unexpectedly...");
					}
				}
			}
			while (!dispatcher.simulator.isFinished()) {
				dispatcher.stopSimulation();
				// Try to terminate simulation more than once if required.
				try {
					sleep(500);
				} catch (InterruptedException e) {
					// Never thrown
					e.printStackTrace();
				}
			}
		}

		/**
		 * This method have to be called when simulation is paused
		 */
		public synchronized void pause() {
			// Unblocks first wait in run method, so residualTime is updated
			paused = true;
			notifyAll();
		}

		/**
		 * This method have to be called when simulation is restarted after pause
		 */
		public synchronized void restart() {
			// Unblocks second wait in run method, so it will restart from the beginning
			synchronized (lock) {
				lock.notifyAll();
				paused = false;
			}
		}

		/**
		 * This method have to be called if simulation is stopped or if it ends before timeout has
		 * elapsed.
		 */
		public synchronized void kill() {
			end = true;
			killed = true;
			synchronized (lock) {
				lock.notifyAll();
			}
			notifyAll();
		}

		/**
		 * Returns elapsed time in percentage format
		 * @return elapsed time / total time
		 */
		public synchronized double getElapsedPercentage() {
			double elapsed;
			if (paused) {
				elapsed = 1 - (double) residualTime / (double) totalTime;
			} else {
				elapsed = 1 - (double) (residualTime - (System.currentTimeMillis() - initialTime)) / (double) totalTime;
			}
			// Fix possible problem when this funcion is called after simulation ends
			if (elapsed > 1) {
				elapsed = 1;
			}
			return elapsed;
		}
	}

}
