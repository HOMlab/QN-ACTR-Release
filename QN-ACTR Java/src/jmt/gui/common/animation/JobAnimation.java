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

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.ImageObserver;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 17-gen-2005
 * Time: 13.53.44
 * Represents a job to be routed through the queue net. Paths to be followed must be specified.
 * If more than one path is defined, this class randomly decides which path to follow next when
 * the current one is completed.
 */
public class JobAnimation implements Animation {

	//bounding box, used to repaint the job
	private Rectangle bounds;
	private Image icon;

	/**current symbolic position. Represents a coordinate calculated along the line of an edge.*/
	protected double position;

	/**timer that keeps trace of the millisecs passed by the time this job entered the
	element (e.g. edge or station) it's going through*/
	protected long timeOfEntrance;

	//speed this job must move at through the queue net.
	private double speed;

	//paths followed by this job
	JobPath[] jobPaths;
	//current path this job is running through
	int currentPath = 0;

	//tells wether this job must be repainted. For example, if this job enters a station,
	//it must be hidden.
	private boolean repaint = true;

	//icon toolkit to be used for renderization of the images.
	private IconsToolkit toolkit;

	/**Creates a new instance of JobAnimation initialized with specified default paths
	 * to be followed by the job and specified speed.
	 * @param speed: speed this job must keep following his paths. It is measured in
	 * pixels per second
	 * @param jps: paths this Job can go through
	 * @param bounds: default size for this job*/
	public JobAnimation(double speed, JobPath[] jps, Rectangle bounds) {
		this(speed, jps, bounds, new DefaultIconsToolkit());
	}

	/**Creates a new instance of JobAnimation initialized with specified values of another job.
	 * @param job: job wanted to be copied.*/
	public JobAnimation(JobAnimation job) {
		this(job.speed, job.jobPaths, job.bounds, new DefaultIconsToolkit());
	}

	/**Creates a new instance of JobAnimation initialized with specified default paths
	 * to be followed by the job and specified speed.
	 * @param speed: speed this job must keep following his paths. It is measured in
	 * pixels per second
	 * @param jps: paths this Job can go through
	 * @param bounds: default size for this job
	 * @param toolkit: Image toolkit for customized renderization of job image*/
	public JobAnimation(double speed, JobPath[] jps, Rectangle bounds, IconsToolkit toolkit) {
		timeOfEntrance = System.currentTimeMillis();
		this.speed = speed;
		jobPaths = jps;
		this.bounds = bounds;
		this.toolkit = toolkit;
		icon = toolkit.getJobIcon(bounds);
		currentPath = (int) (Math.random() * (jobPaths.length));
	}

	/**Adds a path to be followed by this job
	 * @param jp: new path to be added.*/
	public void addPath(JobPath jp) {
		JobPath[] newJps = new JobPath[jobPaths.length + 1];
		for (int i = 0; i < jobPaths.length; i++) {
			newJps[i] = jobPaths[i];
		}
		newJps[jobPaths.length] = jp;
	}

	public void refresh() {
	}

	public void init() {
	}

	public void paint(Graphics g, ImageObserver io) {
		if (repaint) {
			g.drawImage(icon, bounds.x, bounds.y, io);
		}
	}

	public Image getBGImage() {
		return icon;
	}

	public void setBGImage(Image img) {
		icon = img;
	}

	public void setBounds(Rectangle r) {
		if (bounds.width != r.width || bounds.height != r.height) {
			toolkit.getJobIcon(r);
		}
		bounds = r;
	}

	public Rectangle getBounds() {
		return bounds;
	}

	/**Tells this class to show or hide itself.
	 * @param repaint: true if this class must show itself, false otherwise.
	 * */
	public void setRepaint(boolean repaint) {
		this.repaint = repaint;
	}

	/**Tells wether this job was set to be hidden or to be shown.
	 * @return true if this job must be shown, false otherwise*/
	public boolean getRepaint() {
		return repaint;
	}

	public double getSpeed() {
		return speed;
	}

	public double getPosition() {
		return position;
	}

	public void setPosition(double newPosition) {
		position = newPosition;
	}

	/**Returns next queue net element this job must be routed to.*/
	public JobContainer getNextElement() {
		if (jobPaths[currentPath].isLast()) {
			currentPath = (int) (Math.random() * (jobPaths.length));
		}
		return jobPaths[currentPath].getNext();
	}

	/**Resets the time of entrance of this job in the element that currently contains it.
	 * every class that accepts jobs must call this method whenever a a new job is added,
	 * in order to calculate the correct position once the job speed is known.
	 * */
	public void resetTimeOfEntrance() {
		timeOfEntrance = System.currentTimeMillis();
	}

	/** returns system timestamp (in millisecs) for the time of entrance in an element of the
	 * queuenet.
	 * @return :number of system millisecs time.*/
	public long getTimeOfEntrance() {
		return timeOfEntrance;
	}
}
