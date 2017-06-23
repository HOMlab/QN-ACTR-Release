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

/**
 *
 * This class allows to solve a model contained in a ".xml" File from command line,
 * without using the graphic user interface of Java Modelling Tools.<br>
 * The model is validated and solved.
 *
 * @author Stefano Omini
 * @version 16-lug-2004
 */
public class CommandLineSolver {

	private SolverDispatcher solver = null;
	/** the extension of files which contain the models */
	public static final String MODEL_EXTENSION = ".xml";

	/**
	 * Initializes the command line solver.
	 */
	public CommandLineSolver() {
		solver = new SolverDispatcher();
	}

	/**
	 * Solves the model contained in the file. The results are saved into the file itself.
	 * @param file the XML file containing the model
	 * @return true if the model has been correctly solved
	 */
	public boolean solve(File file) {
		if (!(file.getName().toLowerCase().endsWith(MODEL_EXTENSION))) {
			//the passed file is not a xml file
			System.out.println("error: only " + MODEL_EXTENSION + " files are allowed...");
			return false;
		}
		try {
			//solves the model
			solver.solve(file);
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Error in solving the model...");
			return false;
		}
		return true;

	}

	/**
	 * Solves the model contained in the file specified using its path.
	 * The results are saved into the file itself.
	 * @param path the path of the XML file containing the model
	 */
	public boolean solve(String path) {
		File file;
		try {
			file = new File(path);
		} catch (Exception e) {
			System.out.println("Error: file not found...");
			return false;
		}
		return solve(file);
	}

	/**
	 * Used only for TESTING and DEBUGGING.
	 */
	public static void test1() {
		CommandLineSolver solver = new CommandLineSolver();
		String path = "D:\\prova.xml";
		File file = new File(path);
		solver.solve(file);
	}

}
