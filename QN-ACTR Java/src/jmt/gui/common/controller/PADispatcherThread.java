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

import java.io.File;
import java.util.Vector;

import javax.swing.JOptionPane;

import jmt.engine.simDispatcher.Dispatcher_jSIMschema;
import jmt.framework.gui.graph.MeasureValue;
import jmt.gui.common.definitions.AbortMeasure;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.MeasureDefinition;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StoredResultsModel;
import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;
import jmt.gui.common.panels.parametric.PAProgressWindow;
import jmt.gui.common.panels.parametric.PAResultsWindow;
import jmt.gui.common.xml.XMLReader;
import jmt.gui.common.xml.XMLResultsReader;
import jmt.gui.common.xml.XMLWriter;

import org.w3c.dom.Document;

/**
 * <p>Title: PADispatcherThread </p>
 * <p>Description: This thread is responsable for dispatching several simulations. If a simulation max
 * time has been specified, an inner thread will be used to stop simulation after that time. We don't rely
 * on engine method as it doesn't appear to work properly.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 29-dic-2005
 *         Time: 9.58.49
 *
 * Fixed by Bertoli Marco
 */
public class PADispatcherThread extends Thread implements AbortMeasure {
	private SimulationDefinition simd;
	//private CommonModel model;
	private GuiInterface gui;
	private Dispatcher_jSIMschema simulator;
	private PATimerThread timer = null;
	private PAProgressWindow progressWindow;
	private boolean stopped;
	private int currentStep;

	/**
	 * Construct a new Parametric Analysis Dispatcher Thread
	 * @param gui reference to current gui object (can be JSIMMain or Mediator)
	 * @param simd Reference to the simulation definition
	 */
	public PADispatcherThread(GuiInterface gui, SimulationDefinition simd, PAProgressWindow papw) {
		this.setName("PADispatcherThread");
		progressWindow = papw;
		this.simd = simd;
		this.gui = gui;
		// Avoid hanging the system during execution
		this.setPriority(Thread.MIN_PRIORITY);
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
			progressWindow.pause();
		}
	}

	/**
	 * Stops current simulation, forcing abort of all measures,
	 * only if it was already started
	 */
	public void stopSimulation() {
		this.currentStep = simd.getParametricAnalysisModel().getNumberOfSteps();
		if (simulator != null) {
			if (timer != null) {
				timer.kill();
			}
			gui.changeSimActionsState(true, false, false);
			simulator.abortAllMeasures();
			//simulator.killSimulation();
		}
		stopped = true;
		progressWindow.stop();
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
			progressWindow.restart();
		}
	}

	/**
	 * Aborts a measure, given its index
	 * @param index index of the measure to be aborted
	 */
	public void abortMeasure(int index) {
		simulator.abortMeasureAtRefresh(index);
	}

	/*public void startParametricAnalysis() {
	    // If a time maximum time is specified, initialize all stuff related
	    if (simd.getMaximumDuration().longValue() > 0) {
	        timer = new TimerThread(this, simd.getMaximumDuration().doubleValue());
	    }
	    gui.changeSimActionsState(false, true, true);
	    start();
	}*/

	@Override
	public void run() {
		StoredResultsModel results = null;
		File simulationFile = null;
		ParametricAnalysisDefinition pad = simd.getParametricAnalysisModel();
		pad.createValuesSet();
		PAResultsModel parametricAnalysisResultsModel = new PAResultsModel((CommonModel) simd);
		for (currentStep = 0; currentStep < pad.getNumberOfSteps(); currentStep++) {
			progressWindow.setStepNumber(currentStep + 1);
			pad.changeModel(currentStep);
			try {
				simulationFile = File.createTempFile("~JModelSimulation", ".xml");
				simulationFile.deleteOnExit();
			} catch (Exception e) {
				JOptionPane.showMessageDialog(null, "File exception", "Error", JOptionPane.ERROR_MESSAGE);
			}
			simulationFile.deleteOnExit();
			XMLWriter.writeXML(simulationFile, (CommonModel) simd);
			simulator = new Dispatcher_jSIMschema(simulationFile);
			if (simd.getMaximumDuration().longValue() > 0) {
				timer = new PATimerThread(simulator, simd.getMaximumDuration().doubleValue());
				timer.start();
			}
			try {
				simulator.solveModel();
			} catch (OutOfMemoryError err) {
				simulator.abortAllMeasures();
				simulator.killSimulation();
				progressWindow.stop();
				progressWindow.dispose();
				gui.showErrorMessage("Out of memory error. Try to run Java Virtual Machine with more heap size (-Xmx<num>m)");
				break;
			} catch (Exception ex) {
				simulator.abortAllMeasures();
				simulator.killSimulation();
				gui.handleException(ex);
				break;
			}
			File output = simulator.getSimulation().getOutputFile();
			output.deleteOnExit();
			Document doc = XMLReader.loadXML(output.getAbsolutePath());
			results = new StoredResultsModel();
			XMLResultsReader.parseXML(doc, results);
			for (int i = 0; i < results.getMeasureNumber(); i++) {
				Vector thisMes = results.getValues(i);
				int measureState = results.getMeasureState(i);
				Object value = thisMes.get(0);
				boolean valid;
				valid = measureState == MeasureDefinition.MEASURE_SUCCESS;
				//valueSet[i].add(value);
				//validityMap[i].add(Boolean.valueOf(valid));
				MeasureValue tempValue = (MeasureValue) value;
				parametricAnalysisResultsModel.addSample(i, tempValue.getLowerBound(), tempValue.getMeanValue(), tempValue.getUpperBound(), valid);
			}
			if (timer != null) {
				timer.kill();
				//simulator.killSimulation();
			}
		}
		pad.restoreOriginalValues();
		if (!stopped) {
			progressWindow.finished();
			//PAResultsWindow resWin = new PAResultsWindow(simd.getParametricAnalysisModel(),results,valueSet,validityMap);
			//progressWindow.hide();
			progressWindow.stopAnimation();
		} else {
			progressWindow.stop();
		}
		//set the parametric analysis model results
		simd.setSimulationResults(parametricAnalysisResultsModel);
		PAResultsWindow resWin = new PAResultsWindow(simd.getParametricAnalysisModel(), parametricAnalysisResultsModel);
		gui.setResultsWindow(resWin);
		gui.showResultsWindow();
		gui.changeSimActionsState(true, false, false);
		timer = null;
		simulator = null;
	}

}
