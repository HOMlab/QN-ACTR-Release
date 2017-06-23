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
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.JPanel;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 17-gen-2005
 * Time: 13.07.50
 * To change this template use Options | File Templates.
 */
public class QueueNetAnimation extends JPanel implements Animation {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**elements of this queue net animation*/
	protected Vector<StationAnimation> stations = null;
	/**elements of this queue net animation*/
	protected Vector<EdgeAnimation> edges = null;
	/**elements of this queue net animation*/
	protected Vector<JobAnimation> jobs = null;
	//hashmap containing links between stations and edges. Used by add job method to calculate a path
	private HashMap<Animation, Object> links = new HashMap<Animation, Object>();

	//background of the animation
	private Image bgImage;
	//Offline-painted image for optimization of the animation.
	private Image newFrame;

	//Icon toolkit for customized rendering of queue net background and elements
	private IconsToolkit iconToolkit;

	/**Creates a new instance of QueueNetAnimation.*/
	public QueueNetAnimation() {
		this(new DefaultIconsToolkit());
	}

	/**Creates a new instance of QueueNetAnimation with a customized toolkit for renderization
	 * of the queuenet background.*/
	public QueueNetAnimation(IconsToolkit toolkit) {
		stations = new Vector<StationAnimation>();
		edges = new Vector<EdgeAnimation>();
		jobs = new Vector<JobAnimation>();
		iconToolkit = toolkit;
	}

	//builds background image from tile images got from defaulticons class.
	private void buildBGImage() {
		int tileX = 30, tileY = 25;
		Image tile = iconToolkit.getBGTileIcon(new Rectangle(tileX, tileY));
		bgImage = new BufferedImage(getSize().width, getSize().height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics g = bgImage.getGraphics();
		for (int i = 0; i * tileX < getSize().width; i++) {
			for (int j = 0; j * tileY < getSize().height; j++) {
				g.drawImage(tile, i * tileX, j * tileY, this);
			}
		}
	}

	public void refresh() {
		for (int i = 0, j = 0, k = 0; i < edges.size() || j < stations.size() || k < jobs.size(); i++, j++, k++) {
			if (i < edges.size()) {
				edges.get(i).refresh();
			}
			if (j < stations.size()) {
				stations.get(j).refresh();
			}
			if (k < jobs.size()) {
				jobs.get(k).refresh();
			}
		}
		if (getGraphics() != null) {
			update(this.getGraphics());
		}
	}

	/**Updates the image shown during animation, e.g. calls paint() method in every queue net
	 * element's implementing class contained in this queue net.
	 * @param g: Graphics object used to repaint image.*/
	@Override
	public void update(Graphics g) {
		paint(g, this);
	}

	public void init() {
	}

	public void paint(Graphics g, ImageObserver io) {
		if (newFrame == null) {
			newFrame = new BufferedImage(getSize().width, getSize().height, BufferedImage.TYPE_4BYTE_ABGR);
		}
		g.drawImage(newFrame, 0, 0, io);
		Graphics f = newFrame.getGraphics();
		if (bgImage == null) {
			buildBGImage();
		}
		f.drawImage(bgImage, 0, 0, io);
		//first paint all of the edges, then all of the stations
		for (int i = 0; i < edges.size(); i++) {
			edges.get(i).paint(f, io);
		}
		for (int i = 0; i < jobs.size(); i++) {
			jobs.get(i).paint(f, io);
		}
		for (int i = 0; i < stations.size(); i++) {
			stations.get(i).paint(f, io);
		}
	}

	public Image getBGImage() {
		return bgImage;
	}

	public void setBGImage(Image img) {
		bgImage = img;
	}

	/**Adds a station to this queue net.
	 * @param station: station to be added to this queuenet*/
	public void addStation(StationAnimation station) {
		stations.add(station);
		/*adds a new vector to the hashmap containing links to the queuenet elements adjacent to
		this station*/
		links.put(station, new Vector());
	}

	/**Adds an edge between two stations of this queuenet.
	 * @param edge: new edge to be added.
	 * @param source: source station for this edge.
	 * @param target: target station for this edge.*/
	public void addEdge(EdgeAnimation edge, StationAnimation source, StationAnimation target) {
		edges.add(edge);
		((Vector<EdgeAnimation>) links.get(source)).add(edge);
		links.put(edge, target);
	}

	/**Adds a job to be routed through this queue net.
	 * @param jobAnimation: job to be added.
	 * @param startingPoint: queue net element from which this job should start its path.
	 * */
	public void addJob(JobAnimation jobAnimation, JobContainer startingPoint) {
		startingPoint.addJob(jobAnimation);
		jobs.add(jobAnimation);
	}

	/**Returns a list of successor nodes for the specified one.
	 * @param precedant: current node
	 * @return list of nodes which are successors of the current one.*/
	public JobContainer[] getSuccessors(JobContainer precedant) {
		Object obj = links.get(precedant);
		if (obj instanceof Vector) {
			JobContainer[] returnValue = new JobContainer[((Vector) obj).size()];
			((Vector) obj).toArray(returnValue);
			return returnValue;
		}
		if (obj instanceof StationAnimation) {
			return new JobContainer[] { ((StationAnimation) obj) };
		}
		//if I get here, no successor node is defined for this one.
		return new JobContainer[0];
	}

	/**Returns a possible path for this queue net, which must start from start station and end in
	 * end station. If there's no path that links this two stations, then null value is returned.
	 * This is useful in case a job is wanted to run on certain edges and stations and not on all.
	 * This method is not optimized to search for the shortest path, so it will find the first valid
	 * path available.
	 * @param start: starting point for this path.
	 * @param end: ending point for this path.
	 * @return a possible path between start and end stations, or null if does not exist.*/
	public JobPath getSinglePath(StationAnimation start, StationAnimation end) {
		//prepare path exploration...
		Vector<Object> pathVector = new Vector<Object>();
		pathVector.add(start);
		//recursively build path
		pathVector = addElementToPath(pathVector, end);
		JobContainer[] jobContainerArray = new JobContainer[pathVector.size()];
		pathVector.toArray(jobContainerArray);
		return new SinglePath(jobContainerArray);
	}

	//recursive method used to search for paths in this queue net.
	private Vector<Object> addElementToPath(Vector<Object> coveredPath, StationAnimation pathEnd) {
		Object lastElement = coveredPath.get(coveredPath.size() - 1);
		//If destination is reached, start building path in reverse order...
		if (lastElement.equals(pathEnd)) {
			//Don't accept degenerate (e.g. one step-) paths
			if (coveredPath.size() > 1) {
				return coveredPath;
			}
		}
		//Test if I'm on a cycle...
		if (coveredPath.indexOf(lastElement) != coveredPath.size() - 1) {
			return null;
		}
		//find which is successor node for lastelement
		Object obj = links.get(lastElement);
		//no successors, exploration failed!
		if (obj == null) {
			return null;
		}
		//if i am currently on a link, continue walking...
		if (lastElement instanceof EdgeAnimation) {
			coveredPath.add(obj);
			return addElementToPath(coveredPath, pathEnd);
		}
		/*until a dead end or a cycle (e.g. a station already contained in path) is not reached,
		keep exploring the net.*/
		if (obj instanceof Vector) {
			Vector v = (Vector) obj;
			//if lastElement is a dead end
			if (v.size() == 0) {
				return null;
			}
			//if lastElement has only one successor node, i can continue on that path, otherwise
			//I must fork execution and clone coveredPath to avoid aliasing.
			if (v.size() == 1) {
				coveredPath.add(v.get(v.size() - 1));
				return addElementToPath(coveredPath, pathEnd);
			} else {
				for (int i = 0; i < v.size(); i++) {
					Object newNode = v.get(i);
					//clone only when neccessary!
					if (i != 0) {
						coveredPath = new Vector<Object>(coveredPath);
					}
					coveredPath.add(newNode);
					Vector<Object> returnValue = addElementToPath(coveredPath, pathEnd);
					if (returnValue != null) {
						return returnValue;
					}
				}
				return null;
			}
		}
		return null;
	}
}
