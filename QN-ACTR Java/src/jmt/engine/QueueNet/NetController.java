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

package jmt.engine.QueueNet;

import javax.swing.JOptionPane;

import jmt.engine.simEngine.SimSystem;

/**
 * Controls the state of the simulation and determines when to stop the simulation.
 *
 * @author Federico Granata, Stefano Omini
 */

class NetController {
	private boolean running;

	private double startTime, stopTime;

	//number of system "ticks"
	private int n;
	//check measures every refreshPeriod system ticks
	private int refreshPeriod = 12000;

	//check if some measures have not receive any sample yet
	//WARNING: this samples number must be a multiple of refreshPeriod!!
	private int reachabilityTest = refreshPeriod * 10;

	private boolean blocked = false;

	NetController() {

		running = false;
		//initializes tick counter
		n = 0;
	}

	/** This is the run method of the NetController (thread). */
	public void run() {
		try {
			SimSystem.runStart();
			startTime = NetSystem.getElapsedTime();

			while (SimSystem.runTick()) {

			  synchronized (this) {
					//the presence of this "if" allows pause control
					if (blocked) {
						try {
							wait();
						} catch (InterruptedException e) {
							e.printStackTrace();
						}
					}
					n++;

					if (n % refreshPeriod == 0) {
						//User may have defined measures that will not receive any sample
						if (n % reachabilityTest == 0) {
							//stop measures which haven't collected samples yet
							NetSystem.stopNoSamplesMeasures();
						}
						//refresh measures
						NetSystem.checkMeasures();
					}
				}
				
			  //JOptionPane.showMessageDialog(null, "run() + clock: " + SimSystem.clock(), "NetController.java", JOptionPane.INFORMATION_MESSAGE); //CAO   
				//if ( n >= 5 ) break; //CAO test, just run a few events. // generator can be controlled in RandomSource.java
				
			}
			//sim is finished: get stop time
			stopTime = NetSystem.getElapsedTime();
			SimSystem.runStop();
			running = false;

		} catch (Exception Exc) {
			Exc.printStackTrace();
		}
		
	  //JOptionPane.showMessageDialog(null, "run() startTime (s): " + startTime + ", stopTime (s): " + stopTime, "NetController.java", JOptionPane.INFORMATION_MESSAGE); //CAO
		
	}

	public void start() {
		running = true;
	}

	/** Checks if the NetSystem Engine thread is running.
	 * @return True if NetSystem Engine thread is running.
	 */
	synchronized boolean isRunning() {
		return running;
	}

	/** Gets simulation time.
	 * @return Simulation time.
	 */
	synchronized double getSimulationTime() {
		return stopTime - startTime;
	}

	/**
	 * Blocks NetController for synchronized access to data.
	 */
	public synchronized void block() {
		blocked = true;
	}

	/**
	 * Unblocks the object.
	 */
	public synchronized void unblock() {
		blocked = false;
		notifyAll();
	}

}