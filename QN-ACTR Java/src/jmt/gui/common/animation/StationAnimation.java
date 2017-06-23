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
import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 17-gen-2005
 * Time: 13.56.04
 * This class provides a representation for a service station. No animation is provided by
 * default. Implementing classes must override refresh method to implement and customize
 * animation and job management.
 */
public class StationAnimation implements Animation, JobContainer {

	//type of this station
	private String typeOfStation;

	/**list of jobs currently passing through this station.*/
	protected Vector<JobAnimation> jobAnimations = new Vector<JobAnimation>();

	// current image to be rendered
	private Image img;

	//bounding box for this station to be rendered
	private Rectangle bounds;

	/**time a job must stay into this station*/
	protected long residencetime = 0;

	//icon toolkit to be used for renderization of the images.
	private IconsToolkit toolkit;

	/**Creates a new instance of a station animation and initializes it with given type, (which
	 * can be source, server, delay, terminal, sink, fork, join), bounding box to set the area
	 * the rendered image must fill, and the number of millisecs an entering job must stay into
	 * this station during animation.
	 * @param type: type of this station
	 * @param bounds: bounding box for this station
	 * @param residencetime: number of millisecs an entering job must stay in this station
	 */
	public StationAnimation(String type, Rectangle bounds, long residencetime) {
		this(type, bounds, residencetime, new DefaultIconsToolkit());
	}

	/**Creates a new instance of a station animation and initializes it with given type, (which
	 * can be source, server, delay, terminal, sink, fork, join), bounding box to set the area
	 * the rendered image must fill, and the number of millisecs an entering job must stay into
	 * this station during animation.
	 * @param type: type of this station
	 * @param bounds: bounding box for this station
	 * @param residencetime: number of millisecs an entering job must stay in this station
	 * @param toolkit: image toolkit for customized renderization of this station.
	 */
	public StationAnimation(String type, Rectangle bounds, long residencetime, IconsToolkit toolkit) {
		this.toolkit = toolkit;
		img = toolkit.getStationIcon(type, bounds);
		typeOfStation = type;
		this.bounds = bounds;
		this.residencetime = residencetime;
	}

	/**Refreshes current settings for animation. An extending class should override this method
	 * to implement a customized animation. This method also implements job treatement, so that
	 * overriding method must implement job routing or recall this method.*/
	public void refresh() {
		for (int i = 0; i < jobAnimations.size(); i++) {
			JobAnimation ja = jobAnimations.get(i);
			boolean exceededResidencetime = System.currentTimeMillis() - ja.getTimeOfEntrance() >= residencetime;
			if (exceededResidencetime) {
				routeJob(ja);
			}
		}
	}

	//routes a job to next element
	private void routeJob(JobAnimation ja) {
		jobAnimations.remove(ja);
		ja.setRepaint(true);
		ja.getNextElement().addJob(ja);
	}

	public void init() {
	}

	public void paint(Graphics g, ImageObserver io) {
		g.drawImage(img, bounds.x, bounds.y, io);
	}

	public Image getBGImage() {
		return img;
	}

	public void setBGImage(Image img) {
		this.img = img;
	}

	public void setBounds(Rectangle r) {
		img = toolkit.getStationIcon(typeOfStation, bounds);
		bounds = r;
	}

	public Rectangle getBounds() {
		return bounds;
	}

	/**Adds a job to this station.
	 * @param jobAnimation: job to be added to this station.
	 */
	public void addJob(JobAnimation jobAnimation) {
		jobAnimations.add(jobAnimation);
		jobAnimation.setRepaint(false);
		jobAnimation.setPosition(0);
		jobAnimation.resetTimeOfEntrance();
	}
}
