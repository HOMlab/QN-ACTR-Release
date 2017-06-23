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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.ImageObserver;
import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 17-gen-2005
 * Time: 13.55.01
 * This class implements an animation for an edge of the graph that represents the queue net.
 * Once the coords of the angles (which may only be straight angles)of that edge are set, the
 * methods provide support for the output of this edge animation. This means that once the
 * paint method is called, the bounding box for this animation is refreshed with the new frame.
 */
public class EdgeAnimation implements Animation, JobContainer {

	//points this edge passes through
	private Point[] anglePoints;

	/**vector containing all of the jobs that currently are running on this edge of the
	queue net*/
	protected Vector<JobAnimation> jobAnimations = new Vector<JobAnimation>();

	//bounding box for this animation.
	private Rectangle bounds;

	//background image for this animation, tipically the trace of the edge
	private Image bgImage;

	//icon toolkit to be used for renderization of the images.
	private IconsToolkit toolkit;

	/**Creates a new instance of Edgeanimation. This must be initialized with a set of points
	 * which are coords of starting point of this edge, points where this edge turns with a
	 * straight angle (which may be omitted in case this edge doesn't turn), and finally
	 * ending point for this edge. All of the coords must be given in the real animation
	 * resolution scale.
	 * @param angles: coords of the angle points.
	 * @param margin: width of border of this edge. Used to calculate boundingbox for this
	 * animation from coords of the angle points
	 */
	public EdgeAnimation(Point[] angles, int margin) {
		this(angles, margin, new DefaultIconsToolkit());
	}

	/**Creates a new instance of Edgeanimation. This must be initialized with a set of points
	 * which are coords of starting point of this edge, points where this edge turns with a
	 * straight angle (which may be omitted in case this edge doesn't turn), and finally
	 * ending point for this edge. All of the coords must be given in the real animation
	 * resolution scale.
	 * @param angles: coords of the angle points.
	 * @param margin: width of border of this edge. Used to calculate boundingbox for this
	 * animation from coords of the angle points
	 * @param toolkit: toolkit for customized renderization of this edge.
	 */
	public EdgeAnimation(Point[] angles, int margin, IconsToolkit toolkit) {
		anglePoints = angles;
		calculateBounds(margin);
		this.toolkit = toolkit;
		bgImage = toolkit.getEdgeIcon(bounds, angles);
	}

	/**Creates a new instance of Edgeanimation from another edge animation.
	 * @param edge: edge to be copied.
	 */
	public EdgeAnimation(EdgeAnimation edge) {
		this(edge.anglePoints, 10, edge.toolkit);
		Point[] angles = edge.anglePoints;
		int margin = (edge.bounds.width - Math.abs(angles[0].x - angles[angles.length - 1].x)) / 2;
		calculateBounds(margin);
		bgImage = toolkit.getEdgeIcon(bounds, anglePoints);
	}

	//calculates bounding box position and size from angle points coordinates.
	private void calculateBounds(int margin) {
		int maxX = -1, maxY = -1, minX = -1, minY = -1;
		for (Point anglePoint : anglePoints) {
			if (maxX == -1) {
				maxX = anglePoint.x;
			}
			if (maxY == -1) {
				maxY = anglePoint.y;
			}
			if (minX == -1) {
				minX = anglePoint.x;
			}
			if (minY == -1) {
				minY = anglePoint.y;
			}
			if (anglePoint.x > maxX) {
				maxX = anglePoint.x;
			}
			if (anglePoint.y > maxY) {
				maxY = anglePoint.y;
			}
			if (anglePoint.x < minX) {
				minX = anglePoint.x;
			}
			if (anglePoint.y < minY) {
				minY = anglePoint.y;
			}
		}
		maxX += margin;
		minX -= margin;
		maxY += margin;
		minY -= margin;
		bounds = new Rectangle(minX, minY, maxX - minX, maxY - minY);
	}

	/*Converts current linear position to 2d coordinates for representing this job in the
	animation. This way provides a simple way to calculate final coordinates of the image from
	the speed value*/
	private Point convertPositionToCoord(double position) {
		//find between which couple of angle points the job must be placed
		int firstPointIndex = 0;
		double length = 0;
		for (firstPointIndex = 0; firstPointIndex < anglePoints.length - 1; firstPointIndex++) {
			/*calculate distance between points and add result to the length formerly calculated
			for comparison with position parameter*/
			Point firstPoint = anglePoints[firstPointIndex], nextPoint = anglePoints[firstPointIndex + 1];
			double intervalEnd = length + getEuclideanDistance(firstPoint, nextPoint);
			if (position < intervalEnd) {
				return getPointOnSegment(firstPoint, nextPoint, position - length);
			} else {
				length = intervalEnd;
			}
		}
		/*if execution gets here, job has exited this edge. Job must be routed to the
		next component*/
		return null;
	}

	//calculates euclidean distance between two 2d coordinates
	private double getEuclideanDistance(Point p0, Point p1) {
		//if x or y are equal in both the given points, calculate distance more rapidly
		if (p0.getX() == p1.getX()) {
			return Math.abs(p0.getY() - p1.getY());
		}
		if (p0.getY() == p1.getY()) {
			return Math.abs(p0.getX() - p1.getX());
		}
		//if points are on different x and y coords
		return Math.sqrt(Math.pow(p0.getX() - p1.getX(), 2) + Math.pow(p0.getY() - p1.getY(), 2));
	}

	//calculates coordinates of a point given distance from p0 on the segment p0-p1
	private Point getPointOnSegment(Point p0, Point p1, double distanceFromP0) {
		/*if the given points are on the same x or y coord, calculate resulting point in a more
		efficient way*/
		if (p0.getX() == p1.getX()) {
			if (p0.y <= p1.y) {
				return new Point((int) p0.getX(), (int) (p0.getY() + distanceFromP0));
			} else {
				return new Point((int) p0.getX(), (int) (p0.getY() - distanceFromP0));
			}
		}
		if (p0.getY() == p1.getY()) {
			if (p0.x <= p1.x) {
				return new Point((int) (p0.getX() + distanceFromP0), (int) p0.getY());
			} else {
				return new Point((int) (p0.getX() - distanceFromP0), (int) p0.getY());
			}
		}
		//else calculate a more accurate position
		double euclDist = getEuclideanDistance(p0, p1);
		int xDist = (int) (((p1.getX() - p0.getX()) * distanceFromP0) / euclDist);
		int yDist = (int) (((p1.getY() - p0.getY()) * distanceFromP0) / euclDist);
		return new Point(xDist + (int) p0.getX(), yDist + (int) p0.getY());
	}

	/**Puts another Job on this edge. The initial position is set to 0(e.g. the starting point
	 * of this edge)
	 * @param newJob: job to be added.
	 */
	public void addJob(JobAnimation newJob) {
		jobAnimations.add(newJob);
		newJob.setPosition(0);
		newJob.setRepaint(true);
		//centers job to starting point of this edge
		Rectangle jobSize = newJob.getBounds();
		Point p = new Point(anglePoints[0].x - jobSize.width / 2, anglePoints[0].y - jobSize.height / 2);
		newJob.getBounds().setLocation(p);
		newJob.resetTimeOfEntrance();
	}

	/**Refreshes appearence of the image to be rendered. In facts, recalculates all of the jobs'
	 * new positions.*/
	public void refresh() {
		for (int i = 0; i < jobAnimations.size(); i++) {
			JobAnimation ja = jobAnimations.get(i);
			//update position as time*speed
			ja.setPosition((System.currentTimeMillis() - ja.getTimeOfEntrance()) * ja.getSpeed());
			Point p = convertPositionToCoord(ja.getPosition());
			//tests if this job should be painted outside bounding box of this edge
			if (p != null) {
				boolean exceedsWidth = (p.getX() >= bounds.x + bounds.width), exceedsHeight = (p.getY() >= bounds.y + bounds.height);
				if (exceedsWidth || exceedsHeight) {
					routeJob(ja);
				}
				//set bounds of the job in order to be centered to the coords.
				Rectangle r = ja.getBounds();
				p.setLocation(p.getX() - r.getWidth() / 2, p.getY() - r.getHeight() / 2);
				r.setLocation(p);
			} else {
				routeJob(ja);
			}
		}
	}

	//deletes job from local list of jobs to be rendered and routes it to the next element
	//of the queue net.
	private void routeJob(JobAnimation toDelete) {
		int index = jobAnimations.indexOf(toDelete);
		jobAnimations.remove(index);
		toDelete.getNextElement().addJob(toDelete);
	}

	public void init() {
	}

	public void paint(Graphics g, ImageObserver io) {
		g.drawImage(bgImage, bounds.x, bounds.y, io);
	}

	public Image getBGImage() {
		return bgImage;
	}

	public void setBGImage(Image img) {
		bgImage = img;
	}

	public void setBounds(Rectangle r) {
		bounds = r;
		bgImage = toolkit.getEdgeIcon(bounds, anglePoints);
	}

	public Rectangle getBounds() {
		return bounds;
	}
}
