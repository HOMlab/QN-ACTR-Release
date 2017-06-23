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

package jmt.gui.exact.link;

import java.awt.Frame;
import java.io.File;

import jmt.analytical.SolverDispatcher;
import jmt.common.exception.InputDataException;
import jmt.common.exception.SolverException;
import jmt.framework.xml.XMLUtils;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.panels.ProgressWindow;

import org.xml.sax.SAXException;

/**

 * @author alyf (Andrea Conti)
 * Date: 19-set-2003
 * Time: 15.43.51

 * @author Bertoli Marco (what-if analysis)

 */

/**
 * Client side of the solver interface. This implementation uses a file in the user's temp directory as the transport between client and server.
 * Be aware that the file is not deleted if something goes wrong: while this might leave around a lot of files, it makes post-mortem inspection of transferred data possible :)
 */
public class SolverClient {

	public static final boolean DEBUG = false;

	private SolverDispatcher solver;
	private XMLUtils xmlUtils;
	private Frame owner;
	private ProgressWindow progress;

	public SolverClient(Frame owner) {
		solver = new SolverDispatcher();
		xmlUtils = new XMLUtils();
		this.owner = owner;
	}

	public File solve(ExactModel model) throws SolverException, InputDataException {
		File temp = null;

		try {
			temp = File.createTempFile("~jmt_solverClient", ".xml", null);
			temp.deleteOnExit();
			if (!xmlUtils.saveXML(model.createDocument(), temp)) {
				fail("Error saving model to temp file", null);
			}
		} catch (SAXException e) {
			fail("XML parse error", e);
		} catch (Exception e) {
			fail("Error saving model to temp file", e);
		}

		// Creates a solver thread and a solver progress window
		SolverThread solverThread = new SolverThread(solver, temp);
		progress = new ProgressWindow(this, model, owner);
		// Adds a listener that is notified each time the analytical solver terminates
		solver.addSolverListener(new SolverDispatcher.SolverListener() {

			/**
			 * This method is called each time the computation of a model is terminated
			 *
			 * @param num number of computated model (used for iterated solutions)
			 */
			public void computationTerminated(int num) {
				progress.terminateAnalysis(num);
			}
		});
		progress.hide();
		// Starts solution and shows progress window
		solverThread.start();
		progress.show();

		// Waits for computation to be terminated
		try {
			solverThread.join();
		} catch (InterruptedException e) {
			fail("Interrupted while waiting for results", null);
		}

		// Checks for exceptions
		if (solverThread.getOutOfMemoryError() != null) {
			solver = null;
			System.gc();
			solver = new SolverDispatcher();
			throw solverThread.getOutOfMemoryError();
		}
		if (solverThread.getInputException() != null) {
			progress.kill();
			throw solverThread.getInputException();
		}
		if (solverThread.getSolverException() != null) {
			progress.kill();
			throw solverThread.getSolverException();
		}

		try {
			if (!model.loadDocument(xmlUtils.loadXML(temp))) {
				fail("Error loading solved model from tempfile", null);
			}
		} catch (SAXException e) {
			fail("XML parse error", e);
		} catch (Exception e) {
			fail("Error loading solved model from tempfile", e);
		}
		return temp;
	}

	private void fail(String message, Throwable t) throws SolverException {
		if (DEBUG) {
			t.printStackTrace();
		}
		StringBuffer s = new StringBuffer(message);
		if (t != null) {
			s.append("\n");
			s.append(t.toString());
		}

		throw new SolverException(s.toString(), t);
	}

	/**
	 * Stops What-if analysis
	 */
	public void stop() {
		solver.stop();
	}

	/**
	 * Thread used to solve the model.
	 */
	protected class SolverThread extends Thread {
		private SolverDispatcher solver;
		private File model;
		private SolverException solverException;
		private InputDataException inputDataException;
		private OutOfMemoryError outOfMemoryError;

		/**
		 * Creates a solver thread
		 * @param solver reference to a SolverDispatcher object
		 * @param model file to be opened
		 */
		public SolverThread(SolverDispatcher solver, File model) {
			this.solver = solver;
			this.model = model;
			// Avoid blocking the system during solution...
			this.setPriority(Thread.MIN_PRIORITY);
		}

		/**
		 * Starts SolverDispatcher
		 */
		@Override
		public void run() {
			try {
				solver.solve(model);
			} catch (SolverException e) {
				solverException = e;
				progress.kill();
			} catch (InputDataException e) {
				inputDataException = e;
				progress.kill();
			} catch (OutOfMemoryError e) {
				solver = null;
				outOfMemoryError = e;
				progress.kill();
			}
		}

		/**
		 * Tells if a SolverException was thrown
		 * @return thrown SolverException or null if none was raised
		 */
		public SolverException getSolverException() {
			return solverException;
		}

		/**
		 * Tells if an InputDataException was thrown
		 * @return thrown InputDataException or null if none was raised
		 */
		public InputDataException getInputException() {
			return inputDataException;
		}

		/**
		 * Tells if an OutOfMemory error was thrown
		 * @return thrown OutOfMemory or null if none was raised
		 */
		public OutOfMemoryError getOutOfMemoryError() {
			return outOfMemoryError;
		}

	}
}
