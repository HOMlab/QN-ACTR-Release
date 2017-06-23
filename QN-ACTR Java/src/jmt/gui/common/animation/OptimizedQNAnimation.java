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
import java.awt.Rectangle;
import java.awt.image.ImageObserver;

/**
 * Created by IntelliJ IDEA.
 * User: orsotroniii
 * Date: 4-mar-2005
 * Time: 9.41.35
 * To change this template use Options | File Templates.
 */
public class OptimizedQNAnimation extends QueueNetAnimation {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public void paint(Graphics g, ImageObserver io) {
		for (int i = 0; i < jobs.size(); i++) {
			Rectangle lastBounds = ((OptimizedJobAnimation) jobs.get(i)).getLastBounds();
			if (lastBounds != null) {
				this.repaint(lastBounds.x, lastBounds.y, lastBounds.width, lastBounds.height);
			}
		}
		for (int i = 0; i < edges.size(); i++) {
			((OptimizedEdgeAnimation) edges.get(i)).paint(g, io);
		}
		for (int i = 0; i < stations.size(); i++) {
			stations.get(i).paint(g, io);
		}
		for (int i = 0; i < jobs.size(); i++) {
			((OptimizedJobAnimation) jobs.get(i)).paint(g, io);
		}
	}

	@Override
	public void addJob(JobAnimation jobAnimation, JobContainer startingPoint) {
		super.addJob(new OptimizedJobAnimation(jobAnimation), startingPoint);
	}

	@Override
	public void addEdge(EdgeAnimation edge, StationAnimation station1, StationAnimation station2) {
		super.addEdge(new OptimizedEdgeAnimation(edge), station1, station2);
	}

}

class OptimizedJobAnimation extends JobAnimation {

	Rectangle lastBounds;

	public OptimizedJobAnimation(JobAnimation job) {
		super(job);
	}

	@Override
	public void setBounds(Rectangle r) {
		lastBounds = getBounds();
		super.setBounds(r);
	}

	public Rectangle getLastBounds() {
		return lastBounds;
	}
}

class OptimizedEdgeAnimation extends EdgeAnimation {

	public OptimizedEdgeAnimation(EdgeAnimation ea) {
		super(ea);
	}

	@Override
	public void paint(Graphics g, ImageObserver io) {
		if (jobAnimations.size() > 0) {
			super.paint(g, io);
		}
	}
}
