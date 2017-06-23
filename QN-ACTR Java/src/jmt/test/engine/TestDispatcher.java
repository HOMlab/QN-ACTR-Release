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

import jmt.engine.simDispatcher.Dispatcher_jMVAschema;
import jmt.engine.simDispatcher.Dispatcher_jSIMschema;

/**
 * Created by:
 * User: Stefano
 * Date: 24-mag-2005
 * Time: 16.19.47
 */
public class TestDispatcher {

	public static void test_dispatcher_jMVAschema(String path, boolean delete) {

		final Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema(path);
		disp.setDeleteIntermediateFiles(delete);

		// Remove next line to initialize with random seed
		disp.setSimulationSeed(1200000);

		/* Decomment this to add a timer to stop simulation
		new Thread() {
		        public void run() {
		            try{
		                // Set maximum simulation time in milliseconds (here 15 seconds)
		                sleep(15000);
		            }
		            catch (InterruptedException ex) {
		                //Do nothing
		            }
		            disp.abortAllMeasures();
		        }
		}.start();    */

		disp.solveModel();
	}

	public static void test_dispatcher_jSIMschema(String path) {

		Dispatcher_jSIMschema disp = new Dispatcher_jSIMschema(path);
		disp.automaticSimulationSeed();
		disp.solveHandlingExceptions();

	}

	public static void main(String[] args) {
		String modelPath = "D:\\TEST\\chiuso.xml";
		//test_dispatcher_jMVAschema(modelPath,false);
		test_dispatcher_jSIMschema(modelPath);
	}

}
