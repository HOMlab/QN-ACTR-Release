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

import jmt.common.exception.NetException;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.QueueNetwork;
import jmt.engine.simDispatcher.Dispatcher_jMVAschema;
import jmt.engine.testSystem.BatchTest;

/**
 * @author Stefano Omini
 */
public class PauseThreadTest {

	private static void test1() {
		int n = 5;
		Thread1 t1 = new Thread1(n);
		Thread2 t2 = new Thread2(n);
		t1.start();
		t2.start();
	}

	private static void test2() {

		//Dispatcher disp = new Dispatcher("D://randomModel_open_1.xml");
		Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema("D://JMTtest//randomModel_open_1.xml");

		Thread3 t3 = new Thread3(disp);
		Thread4 t4 = new Thread4(disp);
		t3.start();
		t4.start();
	}

	private static void test3() {

		//Dispatcher disp = new Dispatcher("D://randomModel_open_1.xml");
		Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema("D://JMTtest//randomModel_open_1.xml");

		Thread3 t3 = new Thread3(disp);
		Thread5 t5 = new Thread5(disp);
		t3.start();
		t5.start();
	}

	public static void main(String[] args) {
		//test1();
		//test2();
		test3();
	}

}

class Thread1 extends Thread {

	int models;

	public Thread1(int n) {
		models = n;

	}

	@Override
	public void run() {

		BatchTest.comboTest(1, 1, models, 1, 1);

	}
}

class Thread2 extends Thread {

	int models;

	public Thread2(int n) {
		models = n;

	}

	@Override
	public void run() {

		//boolean finished = false;
		double progr = 0.0;

		do {
			try {
				sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

			if (NetSystem.pause()) {
				if (NetSystem.getNetworkList().size() != 0) {
					QueueNetwork net = NetSystem.getNetworkList().get(0);
					try {
						progr = NetSystem.checkProgress(net);
						System.out.println("PROGRESS: " + Double.toString(progr));
						if (progr == 1) {
							//finished = true;
							models--;

						}
					} catch (NetException e) {
						e.printStackTrace();
					}

				}

				NetSystem.restartFromPause();
			} else {
				if (progr > 0) {
					System.out.println("PROGRESS: finished");
					break;
				}
			}

		} while (models > 0);

	}
}

class Thread3 extends Thread {

	private Dispatcher_jMVAschema disp;

	public Thread3(Dispatcher_jMVAschema disp) {
		this.disp = disp;
	}

	@Override
	public void run() {

		disp.solveModel();

	}
}

class Thread4 extends Thread {

	private Dispatcher_jMVAschema disp;
	boolean finished = false;

	public Thread4(Dispatcher_jMVAschema disp) {
		this.disp = disp;
	}

	@Override
	public void run() {

		double progress = 0.0;

		while (!finished) {

			progress = disp.checkSimProgress();

			System.out.println("Progress: " + Double.toString(progress));
			disp.refreshTempMeasures();
			disp.printTempMeasures();

			if (progress == 1.0) {
				finished = true;
				break;
			}

			try {
				sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

		}

	}
}

class Thread5 extends Thread {

	private Dispatcher_jMVAschema disp;
	boolean finished = false;

	public Thread5(Dispatcher_jMVAschema disp) {
		this.disp = disp;
	}

	@Override
	public void run() {

		double progress = 0.0;

		while (!finished) {

			progress = disp.checkSimProgress();

			System.out.println("Progress: " + Double.toString(progress));
			disp.refreshTempMeasures();
			disp.printTempMeasures();

			try {
				sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

			if (disp.abortAllMeasures()) {
				finished = true;
			}

		}

	}
}
