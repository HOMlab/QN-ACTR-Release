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

package jmt.gui.common.animation;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 *
 * This class is a simple thread which only purpose is to update an
 * {@link jmt.gui.common.animation.Animation} implementing class, which must be passed
 * as a construction parameter.
 *
 * Modified by Bertoli Marco to terminate correctly.
 */
public class Animator extends Thread {
	// Used to wait for termination
	private final Object mutex = new Object();

	//tells whether this animation is running.
	private boolean isWorking = false;

	//number of millisecs between two frames of the animation
	private long sleepTime;

	/**The {@link jmt.gui.common.animation.Animation} implementing class that has to be
	 * updated to show the animation.
	 */
	protected Animation animation = null;

	/**Creates a new instance of Animator.
	 * Frame rate is set by default at 30 frames per seconds.
	 * @param a: Animation to be updated by this animator.{@see animation}
	 */
	public Animator(Animation a) {
		this(30, a);
	}

	/**Creates a new instance of Animator with explicit definition of framerate.
	 * @param fps: framerate for this animation.
	 * @param a: Animation to be updated by this animator.{@see animation}
	 */
	public Animator(double fps, Animation a) {
		super();
		animation = a;
		sleepTime = (long) (1000 / fps);
	}

	/**Starts the handled animation*/
	@Override
	public void start() {
		super.start();
		animation.init();
		isWorking = true;
	}

	/**Performs update of the handled animation as far as terminate() method has not been called.*/
	@Override
	public void run() {
		synchronized (mutex) {
			while (isWorking) {
				try {
					mutex.wait(sleepTime);
				} catch (InterruptedException e) {
					this.terminate();
				}
				if (isWorking) {
					animation.refresh();
				}
			}
		}
	}

	/**Terminates current animation*/
	public void terminate() {
		isWorking = false;
		synchronized (mutex) {
			mutex.notifyAll();
		}
	}

}
