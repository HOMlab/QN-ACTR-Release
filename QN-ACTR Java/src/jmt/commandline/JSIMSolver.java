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

import jmt.engine.simDispatcher.Dispatcher_jSIMschema;

/**
 * <p><b>Name:</b> JSIMSolver</p> 
 * <p><b>Description:</b> 
 * This class solves a model using the simulator from the command-line.
 * </p>
 * <p><b>Date:</b> 18/mar/2010
 * <b>Time:</b> 11.49.09</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JSIMSolver {
	
	
	/**
	 * The main method
	 * @param args arguments
	 */
	public static void main(String[] args) throws Exception {
		if (args.length == 0) {
			System.out.println("JSIMSolver Solves a Queueing Network model using JSIM simulator");
			System.out.println();
			System.out.println("Usage: JSIMSolver modelfile [seed]");
			System.out.println("  modelfile  : name of the input XML file describing the model");
			System.out.println("  [seed]     : optional simulation seed. An integer number");
			System.exit(0);
		}
		File model = new File(args[0]);
		if (!model.isFile()) {
			System.err.print("Invalid model file: " + model.getAbsolutePath());
			System.exit(1);
		}
		
		Dispatcher_jSIMschema dispatcher = new Dispatcher_jSIMschema(model);
		// Sets simulation seed if required
		if (args.length > 1) {
			try {
				dispatcher.setSimulationSeed(Long.parseLong(args[1]));
			} catch (NumberFormatException ex) {
				System.err.println("Invalid simulation seed");
				System.exit(1);
			}
		}
		
		// Starts the simulation
		dispatcher.solveModel();
		
		File output = dispatcher.getOutputFile();
		
		System.out.println("Output file stored in path: " + output.getAbsolutePath());
	}

}
