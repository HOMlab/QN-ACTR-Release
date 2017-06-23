/**    
  * Copyright (C) 2010, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.commandline;

import java.io.File;

import jmt.analytical.SolverDispatcher;

/**
 * <p><b>Name:</b> JMVASolver</p> 
 * <p><b>Description:</b> 
 * This class solves a model using the analytic engine from the command-line.
 * </p>
 * <p><b>Date:</b> 18/mar/2010
 * <b>Time:</b> 12.08.08</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMVASolver {
	
	
	/**
	 * The main method
	 * @param args arguments
	 */
	public static void main(String[] args) throws Exception {
		if (args.length == 0) {
			System.out.println("JMVASolver Solves a Queueing Network model using JMVA engine");
			System.out.println();
			System.out.println("Usage: JMVASolver modelfile");
			System.out.println("  modelfile  : name of the input XML file describing the model");
			System.exit(0);
		}
		File model = new File(args[0]);
		if (!model.isFile()) {
			System.err.print("Invalid model file: " + model.getAbsolutePath());
			System.exit(1);
		}

		SolverDispatcher dispatcher = new SolverDispatcher();
		
		// Starts the solution
		dispatcher.solve(model);
		
		System.out.println("Solution results stored in input file: " + model.getAbsolutePath());
	}

}
