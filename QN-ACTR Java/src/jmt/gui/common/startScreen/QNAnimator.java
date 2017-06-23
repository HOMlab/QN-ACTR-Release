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

package jmt.gui.common.startScreen;

import java.awt.Graphics;
import java.awt.Image;
import java.util.Vector;

import javax.swing.JPanel;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 3-dic-2004
 * Time: 17.49.12
 * This component shows an animation representing a queue network.
 * Only for graphical purposes.
 */
public class QNAnimator extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//this inner class is a thread that must repaint every 0.033 seconds the qnanimator.
	class Mover extends Thread {
		//animation to be repainted
		QNAnimator qna;
		//flag representing execution state of this thread
		boolean fRunning = false;
		//time of this animation
		long time;

		public Mover(QNAnimator animator) {
			qna = animator;
		}

		@Override
		public void start() {
			fRunning = true;
		}

		@Override
		public void run() {
			while (fRunning) {
				//while this trhead is running, repaints the animation every 0.033 seconds
				try {
					time += 33;
					qna.repaint(time);
					sleep(33);
				} catch (InterruptedException e) {
					qna.repaint();
				}
			}
		}

		//pauses execution of this thread
		public void freeze() {
			fRunning = false;
		}

		//resumes execution
		public void wakeUp() {
			fRunning = true;
		}

		//stops this thread
		public void kill() {
			fRunning = false;
		}
	}

	//reference to the thread that must repaint this animation
	Mover mover;

	interface ConsumerProducer {
		public void addJob(Job j);

		public void addConsumer(ConsumerProducer cp);
	}

	//represents an edge of the queue network
	class Path {
		//time this instance was created, used set animation frames
		private long birthTime = System.currentTimeMillis();
		//speed for the jobs on this path
		private double jobSpeed;
		//angles of this path
		private int[] xPoints, yPoints;

		public Path(int[] xCoords, int[] yCoords, double speed) {
			xPoints = xCoords;
			yPoints = yCoords;
			jobSpeed = speed;
		}
	}

	//represents a station
	class Station {
		//jobs that are currently in this station
		Vector jobs = new Vector();

	}

	//represents a job
	class Job {
		Image i;

		public Job() {

		}

		public Job(Image jobImg) {
			i = jobImg;
		}

		public void paint(Graphics g, int x, int y) {

		}
	}

	public QNAnimator(java.awt.LayoutManager lm) {
		super(lm);
	}

}
