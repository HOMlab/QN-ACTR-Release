package jmt.gui.common.controller;

import jmt.engine.simDispatcher.Dispatcher_jSIMschema;

/**
 * <p>Title:</p>
 * <p>Description: An inner thread used to stop simulation after timeout elapsed
 *
 *
 * @author Francesco D'Aquino, Bertoli Marco
 *         Date: 7-feb-2006
 *         Time: 23.53.25
 *
 */

public class PATimerThread extends Thread {
	protected final Object lock = new Object();
	protected long residualTime;
	protected Dispatcher_jSIMschema sim;
	protected long initialTime;
	// End means normal end of measures, kill forced one
	protected boolean end = false;
	protected boolean killed = false;

	public PATimerThread(Dispatcher_jSIMschema simulator, double maxDuration) {
		this.setName("PATimerThread");
		//this.setPriority(Thread.MAX_PRIORITY);
		this.sim = simulator;
		this.residualTime = Math.round(maxDuration * 1000);
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

			residualTime -= (System.currentTimeMillis() - initialTime);
			if (residualTime <= 100) {
				end = true;
				break;
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
		while (!sim.isFinished()) {
			sim.abortAllMeasures();
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
		notifyAll();
	}

	/**
	 * This method have to be called when simulation is restarted after pause
	 */
	public synchronized void restart() {
		// Unblocks second wait in run method, so it will restart from the beginning
		synchronized (lock) {
			lock.notifyAll();
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
			notifyAll();
		}
	}
}
