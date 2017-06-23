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

import java.io.File;

import jmt.common.exception.InputDataException;
import jmt.common.exception.SolverException;
import jmt.framework.data.ArrayUtils;
import jmt.framework.xml.XMLUtils;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;

import org.xml.sax.SAXException;

/**
 * Server side of the solver interface.<br>
 * This object takes a model, instantiates the correct solver, and solves it.<br>
 * Should probably be rewritten using a different data structure to hold model information
 *
 * @author unknown, Bertoli Marco (what-if analysis)
 */
public class SolverDispatcher {

	private static final boolean DEBUG = true;
	private static final boolean PRINTMODEL = false;

	private boolean stopped = false;

	private XMLUtils xmlUtils;
	/** Used to notify when a computation ends */
	private SolverListener listener;

	public SolverDispatcher() {
		xmlUtils = new XMLUtils();
	}

	/**
	 * Solves the model in file.
	 * @throws jmt.common.exception.SolverException if there were errors operation was not successful
	 */
	public void solve(File file) throws SolverException, InputDataException {
		ExactModel model = new ExactModel();
		try {
			if (!model.loadDocument(xmlUtils.loadXML(file))) {
				fail("Error loading model from tempfile", null);
			}
		} catch (SAXException e) {
			fail("XML parse error in tempfile", e);
		} catch (Exception e) {
			fail("Error loading model from tempfile", e);
		}

		if (PRINTMODEL) {
			System.out.println(model);
		}

		solve(model);

		if (PRINTMODEL) {
			System.out.println(model);
		}

		try {
			if (!xmlUtils.saveXML(model.createDocument(), file)) {
				fail("Error saving solved model to tempfile", null);
			}
		} catch (SAXException e) {
			fail("XML parse error in tempfile", e);
		} catch (Exception e) {
			fail("Error saving solved model to tempfile", e);
		}

	}

	/**
	 * Stops What-if analysis and invalidates results
	 */
	public void stop() {
		stopped = true;
	}

	/**
	 * Solves input model. This method will look for what-if analysis data and
	 * perform a what-if analysis if requested.
	 * <br>
	 * Author: Bertoli Marco
	 * @param model model to be solved
	 * @throws SolverException if something goes wrong during solution
	 * @throws InputDataException if some input data are malformed
	 */
	public void solve(ExactModel model) throws SolverException, InputDataException {
		stopped = false;
		model.resetResults();
		// Solves normal models
		if (!model.isWhatIf()) {
			finalDispatch(model, 0);
		}
		// Now solves what-if models
		else {
			// Arrival rates what-if analysis
			if (model.getWhatIfType().equalsIgnoreCase(ExactConstants.WHAT_IF_ARRIVAL)) {
				whatIfArrival(model);
			}
			// Customers number what-if analysis
			else if (model.getWhatIfType().equalsIgnoreCase(ExactConstants.WHAT_IF_CUSTOMERS)) {
				whatIfCustomers(model);
			}
			// Service demands what-if analysis
			else if (model.getWhatIfType().equalsIgnoreCase(ExactConstants.WHAT_IF_DEMANDS)) {
				whatIfDemands(model);
			}
			// Population mix what-if analysis
			else if (model.getWhatIfType().equalsIgnoreCase(ExactConstants.WHAT_IF_MIX)) {
				whatIfMix(model);
			}
		}
	}

	public void finalDispatch(ExactModel model, int iteration) throws SolverException, InputDataException {

		/* disable all change-checking */
		model.discardChanges();
		model.setChanged();
		ModelFESCApproximator fesc = new ModelFESCApproximator(model, iteration);
		try {
			if (model.isMultiClass() || model.isOpen()) {
				solveMulti(fesc.getModelToBeSolved(), iteration);
			} else {
				solveSingle(fesc.getModelToBeSolved(), iteration);
			}
			// Notify termination of current model solution
			if (listener != null) {
				listener.computationTerminated(iteration);
			}
		} catch (InputDataException rse) {
			throw rse;
		} catch (SolverException se) {
			throw se;
		} catch (Exception e) {
			fail("Unhandled exception", e);
		}
		fesc.processModelAfterSolution();
	}

	private void fail(String message, Throwable t) throws SolverException {
		if (DEBUG && t != null) {
			t.printStackTrace();
		}
		StringBuffer s = new StringBuffer(message);
		if (t != null) {
			s.append("\n");
			s.append(t.toString());
		}

		throw new SolverException(s.toString(), t);
	}

	private void solveSingle(ExactModel model, int iteration) throws Exception, InputDataException {

		int stations = model.getStations();
		Solver solver = null;

		//init
		String[] names = model.getStationNames();
		int[] types = mapStationTypes(model.getStationTypes());
		double[][] servicetimes = (ArrayUtils.extract13(model.getServiceTimes(), 0));

		//no supplemental copy here since extract13 already copies the first level of the array
		adjustLD(servicetimes, types);
		double[] visits = ArrayUtils.extract1(model.getVisits(), 0);

		try {
			if (model.isClosed()) {
				//single closed
				int pop = model.getMaxpop();
				//First of all controls that the closed class has population greater than 0.
				//Otherwise throws a InputDataException
				if (pop <= 0) {
					//error: population is not greater than 0.0
					throw new InputDataException("Population must be greater than zero");
				}
				solver = new SolverSingleClosedMVA(pop, stations);
				if (!solver.input(names, types, servicetimes, visits)) {
					fail("Error initializing MVASolver", null);
				}
			} else {
				//TODO this is not used any more (multi solver is used instead)
				/* single open */
				double lambda = (model.getClassData())[0];
				//First of all controls that the open class has rate greater than 0.
				//Otherwise throws a InputDataException
				if (lambda <= 0) {
					//error: rate is not greater than 0.0
					throw new InputDataException("Arrival rate must be greater than zero");

				}
				solver = new SolverSingleOpen(model.getClassData()[0], stations);
				if (!solver.input(names, types, servicetimes, visits)) {
					fail("Error initializing OpenSolver", null);
				}
			}
		} catch (Exception e) {
			fail("Error initializing SingleClass solver", e);
		}
		//controls processing capacity
		if (!solver.hasSufficientProcessingCapacity()) {
			throw new InputDataException("One or more resources are in saturation. Decrease arrival rates or service demands.");
		}

		/* solve */
		solver.solve();

		/* solution */
		double[][] ql = ArrayUtils.makeFilled(stations, 1, -1);
		ArrayUtils.insert1(ql, solver.getQueueLen(), 0);

		double[][] tp = ArrayUtils.makeFilled(stations, 1, -1);
		ArrayUtils.insert1(tp, solver.getThroughput(), 0);

		double[][] rt = ArrayUtils.makeFilled(stations, 1, -1);
		ArrayUtils.insert1(rt, solver.getResTime(), 0);

		double[][] util = ArrayUtils.makeFilled(stations, 1, -1);
		ArrayUtils.insert1(util, solver.getUtilization(), 0);

		model.setResults(ql, tp, rt, util, iteration);
	}

	private void solveMulti(ExactModel model, int iteration) throws SolverException, InputDataException {
		if (model.isLd()) {
			throw new SolverException("Multiclass solver does not support LD stations");
		}

		int classes = model.getClasses();
		int stations = model.getStations();

		String[] stationNames = model.getStationNames();
		int[] stationTypes = mapStationTypes(model.getStationTypes());
		double[] classData = model.getClassData();
		double[][][] serviceTimes = model.getServiceTimes();
		double[][] visits = model.getVisits();
		int[] classPop = ArrayUtils.toInt(classData);

		SolverMulti solver = null;

		//First of all controls that all classes have population or rate greater than 0.
		//Otherwise throws a InputDataException
		for (double element : classData) {
			if (element <= 0) {
				//error: population or rate not greater than 0.0
				//prepare message according to model type (mixed, open or closed)

				if (model.isMixed()) {
					//mixed model -> populations or rates
					throw new InputDataException("Populations and arrival rates must be greater than zero");
				} else if (model.isOpen()) {
					//open model -> rates
					throw new InputDataException("Arrival rates must be greater than zero");
				} else {
					//closed model -> populations
					throw new InputDataException("Populations must be greater than zero");
				}

			}
		}

		/* init */
		try {
			if (model.isOpen()) {
				solver = new SolverMultiOpen(classes, stations, model.getClassData(), model.getStationServers());
				if (!solver.input(stationNames, stationTypes, serviceTimes, visits)) {
					fail("Error initializing SolverMultiOpen", null);
				}
			} else {
				if (model.isClosed()) {
					SolverMultiClosedMVA closedsolver = new SolverMultiClosedMVA(classes, stations);
					if (!closedsolver.input(stationNames, stationTypes, serviceTimes, visits, classPop)) {
						fail("Error initializing MVAMultiSolver", null);
					}
					solver = closedsolver;
				} else {
					//model is multiclass mixed
					int[] classTypes = mapClassTypes(model.getClassTypes());

					SolverMultiMixed mixedsolver = new SolverMultiMixed(classes, stations);
					if (!mixedsolver.input(stationNames, stationTypes, serviceTimes, visits, classData, classTypes)) {
						fail("Error initializing SolverMultiMixed", null);
					}
					solver = mixedsolver;
				}
			}
		} catch (Exception e) {
			fail("Error initializing Multiclass solver", e);
		}
		if (!solver.hasSufficientProcessingCapacity()) {
			throw new InputDataException("One or more resources are in saturation. Decrease arrival rates or service demands.");
		}

		/* solution */
		solver.solve();

		double[][] ql = ArrayUtils.resize2(solver.getQueueLen(), stations, classes, 0);

		double[][] tp = ArrayUtils.resize2(solver.getThroughput(), stations, classes, 0);

		double[][] rt = ArrayUtils.resize2(solver.getResTime(), stations, classes, 0);

		double[][] util = ArrayUtils.resize2(solver.getUtilization(), stations, classes, 0);

		model.setResults(ql, tp, rt, util, iteration);
	}

	/**
	 * Map station types from model constants to solver constants
	 */
	private int[] mapStationTypes(int[] stationTypes) {
		int len = stationTypes.length;
		int[] res = new int[len];
		for (int i = 0; i < len; i++) {
			switch (stationTypes[i]) {
				case ExactConstants.STATION_LD:
					res[i] = Solver.LD;
					break;
				case ExactConstants.STATION_LI:
					res[i] = Solver.LI;
					break;
				case ExactConstants.STATION_DELAY:
					res[i] = Solver.DELAY;
					break;
				default:
					res[i] = -1;
			}
		}
		return res;
	}

	/**
	 * Map class types from model constants to solver constants
	 */
	private int[] mapClassTypes(int[] classTypes) {
		int len = classTypes.length;
		int[] res = new int[len];
		for (int i = 0; i < len; i++) {
			switch (classTypes[i]) {
				case ExactConstants.CLASS_OPEN:
					res[i] = SolverMulti.OPEN_CLASS;
					break;
				case ExactConstants.CLASS_CLOSED:
					res[i] = SolverMulti.CLOSED_CLASS;
					break;
				default:
					res[i] = -1;
			}
		}
		return res;
	}

	/*
	 * To conserve memory, service times arrays for LI stations are "compressed" to a single value. This
	 * method expands them back to full length (single class version)
	 */
	private void expand(double[][] st, int pop) {
		if (pop == 1) {
			return;
		}
		int len1 = st.length;
		for (int i = 0; i < len1; i++) {
			if (st[i].length < pop) {
				st[i] = ArrayUtils.resize(st[i], pop, st[i][0]);
			}
		}
	}

	/*
		 * To conserve memory, service times arrays for LI stations are "compressed" to a single value. This
		 * method expands them back to full length (multiclass version)
		 */
	private void expand(double[][][] st, int pop) {
		if (pop == 1) {
			return;
		}
		int len1 = st.length;
		for (int i = 0; i < len1; i++) {
			expand(st[i], pop);
		}
	}

	/** HACK: adds an itial zero to all LD stations */
	private void adjustLD(double[][] st, int[] types) {
		for (int i = 0; i < st.length; i++) {
			if (types[i] == Solver.LD) {
				st[i] = ArrayUtils.prepend0(st[i]);
			}
		}
	}

	/** HACK: adds an initial zero to all LD stations */
	private void adjustLD(double[][][] st, int[] types) {
		for (int i = 0; i < st.length; i++) {
			if (types[i] == Solver.LD) {
				for (int j = 0; j < st[i].length; j++) {
					st[i][j] = ArrayUtils.prepend0(st[i][j]);
				}
			}
		}
	}

	// --- What-if Analysis methods --- Bertoli Marco -------------------------------------
	/**
	 * Performs a what-if analysis by changing arrival rates
	 * @param model input model
	 */
	private void whatIfArrival(ExactModel model) throws SolverException, InputDataException {
		// Sanity checks on input model.
		if (model.getWhatIfClass() >= 0 && model.getClassTypes()[model.getWhatIfClass()] != ExactConstants.CLASS_OPEN) {
			throw new InputDataException("Cannot change arrival rate of a closed class.");
		}
		if (model.isClosed()) {
			throw new InputDataException("Cannot change arrival rates in a closed model.");
		}

		// Values for what-if
		double[] values = model.getWhatIfValues();

		// Backup class datas
		double[] initials = model.getClassData().clone();

		// Iterates for what-if executions
		for (int i = 0; i < model.getWhatIfValues().length && !stopped; i++) {
			double[] current = initials.clone();
			// If this is one class only
			if (model.getWhatIfClass() >= 0) {
				current[model.getWhatIfClass()] = values[i];
			}
			// If this is all open classes
			else {
				for (int j = 0; j < current.length; j++) {
					if (model.getClassTypes()[j] == ExactConstants.CLASS_OPEN) {
						current[j] = initials[j] * values[i];
					}
				}
			}
			model.setClassData(current);

			// Checks if stopped
			if (stopped) {
				break;
			}

			// Now solves current model - we cannot interrupt this as it's not designed to be done.
			finalDispatch(model, i);
		}
		// Resets initial model
		model.setClassData(initials);
		// Results are ok if the process was not stopped.
		model.setResultsOK(!stopped);
	}

	/**
	 * Performs a what-if analysis by changing number of customers
	 * @param model input model
	 */
	private void whatIfCustomers(ExactModel model) throws SolverException, InputDataException {
		// Sanity checks on input model.
		if (model.getWhatIfClass() >= 0 && model.getClassTypes()[model.getWhatIfClass()] != ExactConstants.CLASS_CLOSED) {
			throw new InputDataException("Cannot change number of customers of an open class.");
		}
		if (model.isOpen()) {
			throw new InputDataException("Cannot change number of customers in an open model.");
		}

		// Values for what-if
		double[] values = model.getWhatIfValues();

		// Backup class datas
		double[] initials = model.getClassData().clone();

		// Iterates for what-if executions
		int i;
		for (i = 0; i < model.getWhatIfValues().length && !stopped; i++) {
			double[] current = initials.clone();
			// If this is one class only
			if (model.getWhatIfClass() >= 0) {
				current[model.getWhatIfClass()] = values[i];
				// Check for not integer values
				if (Math.abs(current[model.getWhatIfClass()] - Math.rint(current[model.getWhatIfClass()])) > 1e-8) {
					throw new InputDataException("A fractional population value was assigned to class "
							+ model.getClassNames()[model.getWhatIfClass()] + " during step " + i);
				}
				// Rounds number to avoid truncation problems
				current[model.getWhatIfClass()] = Math.round(current[model.getWhatIfClass()]);

			}
			// If this is all closed classes
			else {
				for (int j = 0; j < current.length; j++) {
					if (model.getClassTypes()[j] == ExactConstants.CLASS_CLOSED) {
						current[j] = initials[j] * values[i];
						// Check for not integer values
						if (Math.abs(current[j] - Math.rint(current[j])) > 1e-8) {
							throw new InputDataException("A fractional population value was assigned to class " + model.getClassNames()[j]
									+ " during step " + i);
						}
						// Rounds number to avoid truncation problems
						current[j] = Math.round(current[j]);
					}
				}
			}
			model.setClassData(current);

			// Checks if stopped
			if (stopped) {
				break;
			}

			// Now solves current model - we cannot interrupt this as it's not designed to be done.
			finalDispatch(model, i);
		}
		// Resets initial model
		model.setClassData(initials);
		// Results are ok if the process was not stopped.
		model.setResultsOK(!stopped);
	}

	/**
	 * Performs a what-if analysis by changing service demands of a given station.
	 * @param model input model
	 */
	private void whatIfDemands(ExactModel model) throws SolverException, InputDataException {
		// Sanity checks on input model.
		if (model.getWhatIfStation() < 0 || model.getWhatIfStation() >= model.getStations()) {
			throw new InputDataException("Station for what-if analysis not specified.");
		}
		if (model.getStationTypes()[model.getWhatIfStation()] == ExactConstants.STATION_LD) {
			throw new InputDataException("Service Demands what-if analysis not supported on Load Dependent stations.");
		}

		// Values for what-if
		double[] values = model.getWhatIfValues();

		// Backup service times datas (note: we multiply only service times as it's the same of multiply service demands)
		double[][][] initials = ArrayUtils.copy3(model.getServiceTimes());

		// Saves what-if class and station indices
		int cl = model.getWhatIfClass();
		int st = model.getWhatIfStation();

		// Iterates for what-if executions
		int i;
		for (i = 0; i < values.length && !stopped; i++) {
			double[][][] current = ArrayUtils.copy3(initials);
			// If this is one class only
			if (cl >= 0) {
				if (model.getVisits()[st][cl] > 0) {
					current[st][cl][0] = values[i] / model.getVisits()[st][cl];
				} else {
					current[st][cl][0] = 0.0;
				}
			}
			// If this is all classes
			else {
				for (int j = 0; j < model.getClasses(); j++) {
					current[st][j][0] = initials[st][j][0] * values[i];
				}
			}
			model.setServiceTimes(current);

			// Checks if stopped
			if (stopped) {
				break;
			}

			// Now solves current model - we cannot interrupt this as it's not designed to be done.
			finalDispatch(model, i);
		}
		// Resets initial model
		model.setServiceTimes(initials);
		// Results are ok if the process was not stopped.
		model.setResultsOK(!stopped);
	}

	/**
	 * Performs a what-if analysis by changing population mix
	 * @param model input model
	 */
	private void whatIfMix(ExactModel model) throws SolverException, InputDataException {
		// First and second closed class for population mix what-if
		int class1, class2 = -1;
		class1 = model.getWhatIfClass();
		if (class1 < 0) {
			throw new InputDataException("Class not specified for population mix what-if analysis.");
		}
		// Find second class
		for (int i = 0; i < model.getClasses(); i++) {
			if (model.getClassTypes()[i] == ExactConstants.CLASS_CLOSED && i != class1) {
				if (class2 < 0) {
					class2 = i;
				} else {
					throw new InputDataException("Only models with two closed classes are supported. More than two classes detected.");
				}
			}
		}
		if (class2 < 0) {
			throw new InputDataException("Only models with two closed classes are supported. Only one classes detected.");
		}

		// Values for what-if
		double[] values = model.getWhatIfValues();

		// Backup class datas
		double[] initials = model.getClassData().clone();

		// Value for total number of customer
		double N = initials[class1] + initials[class2];

		// Iterates for what-if executions
		int i;
		for (i = 0; i < model.getWhatIfValues().length && !stopped; i++) {
			double[] current = initials.clone();
			current[class1] = values[i] * N;
			current[class2] = (1 - values[i]) * N;
			// Check for not integer values
			if (Math.abs(current[class1] - Math.rint(current[class1])) > 1e-8) {
				throw new InputDataException("A fractional population value was assigned to class " + model.getClassNames()[class1] + " during step "
						+ i);
			} else if (Math.abs(current[class2] - Math.rint(current[class2])) > 1e-8) {
				throw new InputDataException("A fractional population value was assigned to class " + model.getClassNames()[class2] + " during step "
						+ i);
			}
			// Rounds number to avoid truncation problems
			current[class1] = Math.round(current[class1]);
			current[class2] = Math.round(current[class2]);

			model.setClassData(current);

			// Checks if stopped
			if (stopped) {
				break;
			}

			// Now solves current model - we cannot interrupt this as it's not designed to be done.
			finalDispatch(model, i);
		}
		// Resets initial model
		model.setClassData(initials);
		// Results are ok if the process was not stopped.
		model.setResultsOK(!stopped);
	}

	// ------------------------------------------------------------------------------------

	// ---- Callbacks ---------------------------------------------------------------------
	/**
	 * Adds a solver listener to be notified when computation of an iteration terminates.
	 * This is useful for notification of a progress window. Only one listener is allowed.
	 * @param listener listener to be added or null to remove previous one.
	 */
	public void addSolverListener(SolverListener listener) {
		this.listener = listener;
	}

	/**
	 * Listener used to notify when computation of a model is terminated
	 */
	public interface SolverListener {
		/**
		 * This method is called each time the computation of a model is terminated
		 * @param num number of computated model (used for iterated solutions)
		 */
		public void computationTerminated(int num);
	}
	// ------------------------------------------------------------------------------------

}
