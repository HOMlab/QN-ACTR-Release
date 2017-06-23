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

package jmt.engine.jaba;

import java.awt.Rectangle;
import java.awt.geom.Area;
import java.awt.geom.Point2D;
import java.util.Vector;

/**
 * This class from the service demand and the saturation sectors
 * of the jaba engine create the list of Dominant, Dominate and Convex Hull
 * point.
 * 
 * @author Carlo Gimondi
 */
public class EngineConvex2D {

	//All the point of the category
	private Vector<Point2D> imperant;
	private Vector<Point2D> dominates;
	private Vector<Point2D> convex;
	private Vector<Point2D> points;

	//	All the point of the category
	private Vector<Point2D> allPoints;
	private Vector<Point2D> allConvex;
	private Vector<Point2D> allImperant;
	private Vector<Point2D> allDominates;

	// The filtered point in each category
	private Vector<Point2D> filtDominants;
	private Vector<Point2D> filtDominates;
	private Vector<DPoint> filtPoints;
	private Vector<Point2D> filtConvex;

	//The filtered area
	private Area filtArea;

	//private static final DecimalFormat formatter = new DecimalFormat("0");	

	/**
	 * Costructor of the life
	 * @param allPoints All the point of the graph
	 * @param sd3 The sector of saturation
	 */
	public EngineConvex2D(Vector<Point2D> allPoints, Vector<Object> sd3) {
		this.allPoints = (Vector<Point2D>) allPoints.clone();
		allConvex = new Vector<Point2D>();
		allImperant = (Vector<Point2D>) allPoints.clone();
		allDominates = new Vector<Point2D>();

		filtDominants = new Vector<Point2D>();
		filtDominates = new Vector<Point2D>();
		filtPoints = new Vector<DPoint>();
		filtConvex = new Vector<Point2D>();

		filtArea = new Area();

		separator();
		//calcConvex();
		calcConvex(sd3);

		dominates = (Vector<Point2D>) allDominates.clone();
		imperant = (Vector<Point2D>) allImperant.clone();
		convex = (Vector<Point2D>) allConvex.clone();
		points = (Vector<Point2D>) this.allPoints.clone();
	}

	/**
	 * Return the area in with the point are filtered
	 * @return The filtered area
	 */
	public Area getFilteredArea() {
		return (Area) filtArea.clone();
	}

	/**
	 * Add this area to the filt area
	 * @param xP1 The x of the first point
	 * @param yP1 The y of the first point
	 * @param xP2 The x of the second point
	 * @param yP2 The y of the second point
	 * @param error
	 */
	public void addFilterArea(double xP1, double yP1, double xP2, double yP2, double error) {
		//If area is 0 returns
		if ((xP1 == xP2) || (yP1 == yP2)) {
			return;
		}

		double x1 = Math.min(xP1, xP2);
		double y1 = Math.min(yP1, yP2);
		double x2 = Math.max(xP1, xP2);
		double y2 = Math.max(yP1, yP2);

		Rectangle r = new Rectangle((int) (x1 * 100), -(int) (y2 * 100), (int) Math.abs((x1 * 100) - (x2 * 100)), (int) Math.abs((y1 * 100)
				- (y2 * 100)));
		filtArea.add(new Area(r));

		if (xP1 < xP2) {
			x1 = x1 + (error / 2);
			x2 = x2 - (error / 2);
		}

		DPoint p;
		//Move from dominants vector the filtered points
		for (int k = 0; k < imperant.size(); k++) {
			p = (DPoint) imperant.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				imperant.removeElementAt(k);
				filtDominants.add(p);
				k--;
			}
		}

		//Move from dominates vector the filtered points
		for (int k = 0; k < dominates.size(); k++) {
			p = (DPoint) dominates.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				dominates.removeElementAt(k);
				filtDominates.add(p);
				k--;
			}
		}

		//Move from convex vector the filtered points
		for (int k = 0; k < convex.size(); k++) {
			p = (DPoint) convex.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				convex.removeElementAt(k);
				filtConvex.add(p);
				k--;
			}
		}

		//Move from allPoints vector the filtered points
		for (int k = 0; k < points.size(); k++) {
			p = (DPoint) points.get(k);
			p.setSelect(false);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				points.removeElementAt(k);
				filtPoints.add(p);
				k--;
			}
		}
	}

	/**
	 * Remove the selected area from the filtered area
	 * @param xP1 The x of the first point
	 * @param yP1 The y of the first point
	 * @param xP2 The x of the first second
	 * @param yP2 The y of the first second
	 * @param error
	 */
	public void addFreeArea(double xP1, double yP1, double xP2, double yP2, double error) {
		//If area is 0 returns
		if ((xP1 == xP2) || (yP1 == yP2)) {
			return;
		}

		double x1 = Math.min(xP1, xP2);
		double y1 = Math.min(yP1, yP2);
		double x2 = Math.max(xP1, xP2);
		double y2 = Math.max(yP1, yP2);

		Rectangle r = new Rectangle((int) (x1 * 100), -(int) (y2 * 100), (int) Math.abs((x1 * 100) - (x2 * 100)), (int) Math.abs((y1 * 100)
				- (y2 * 100)));
		filtArea.subtract(new Area(r));

		if (xP1 < xP2) {
			x1 = x1 + (error / 2);
			x2 = x2 - (error / 2);
		}

		DPoint p;
		//Move from dominants vector the filtered points
		for (int k = 0; k < filtDominants.size(); k++) {
			p = (DPoint) filtDominants.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				filtDominants.removeElementAt(k);
				imperant.add(p);
				k--;
			}
		}

		//Move from dominates vector the filtered points
		for (int k = 0; k < filtDominates.size(); k++) {
			p = (DPoint) filtDominates.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				filtDominates.removeElementAt(k);
				dominates.add(p);
				k--;
			}
		}

		//Move from convex vector the filtered points
		for (int k = 0; k < filtConvex.size(); k++) {
			p = (DPoint) filtConvex.get(k);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				filtConvex.removeElementAt(k);
				convex.add(p);
				k--;
			}
		}

		//Move from allPoints vector the filtered points
		for (int k = 0; k < filtPoints.size(); k++) {
			p = filtPoints.get(k);
			p.setSelect(false);
			if ((x2 > p.getX()) && (y2 > p.getY()) && (x1 < p.getX()) && (y1 < p.getY())) {
				filtPoints.removeElementAt(k);
				points.add(p);
				k--;
			}
		}
	}

	/**
	 * Return the no-filtered dominates point
	 */
	public Vector<Point2D> getDominates() {
		return dominates;
	}

	/**
	 * Return the no-filtered dominants point
	 */
	public Vector<Point2D> getDominants() {
		return imperant;
	}

	/**
	 * Return the no-filtered convex point
	 */
	public Vector<Point2D> getConvex() {
		return convex;
	}

	/**
	 * Return the no-filtered point
	 */
	public Vector<Point2D> getPoints() {
		return points;
	}

	/**
	 * Return all the dominates point
	 */
	public Vector<Point2D> getAllDominates() {
		return allDominates;
	}

	/**
	 * Return all the dominates point
	 */
	public Vector<Point2D> getAllDominants() {
		return allImperant;
	}

	/**
	 * Return all the convex hull point
	 */
	public Vector<Point2D> getAllConvex() {
		return allConvex;
	}

	/**
	 * Return all points
	 */
	public Vector<Point2D> getAllPoints() {
		return allPoints;
	}

	/**
	 * Return the filtered dominate points
	 */
	public Vector<Point2D> getFiltDominates() {
		return filtDominates;
	}

	/**
	 * Return the filtered dominant points
	 */
	public Vector<Point2D> getFiltDominants() {
		return filtDominants;
	}

	/**
	 * Return the filtered Convex hull point
	 */
	public Vector<Point2D> getFiltConvex() {
		return filtConvex;
	}

	/**
	 * Return the filtered points
	 */
	public Vector<DPoint> getFiltPoints() {
		return filtPoints;
	}

	/**
	 * This function separates the points in two partdominant and dominates 
	 *
	 */
	private void separator() {
		/* At the beginning dominants contains all point and diminates is empty
		 * when a dominated point is find it is isert in dominates vector and removed from
		 * doinants one.
		 */
		for (int i = 0; i < allPoints.size(); i++) {
			Point2D p1 = allPoints.get(i);
			for (int k = 0; k < allImperant.size(); k++) {
				Point2D p2 = allImperant.get(k);
				if ((p1.getX() < p2.getX()) && (p1.getY() < p2.getY())) {
					allDominates.add(p1);
					allImperant.remove(p1);
					break;
				}
			}
		}
		allDominates = orderByX(allDominates);
		allImperant = orderByX(allImperant);
	}

	/**
	 * This function return a vector that contains the same point of the parameter one but
	 * the points are orderet from the point whith the biggest X to the point whith the smallest X
	 * @param v The vector to order
	 * @return The vector ordered
	 */
	private Vector<Point2D> orderByX(Vector<Point2D> v) {
		Vector<Point2D> r = (Vector<Point2D>) v.clone();
		boolean again = true;

		while (again) {
			again = false;
			for (int k = 0; k < r.size() - 1; k++) {
				//Take the points
				Point2D p1 = r.get(k);
				Point2D p2 = r.get(k + 1);

				if (p1.getX() < p2.getX()) {
					//swap
					r.remove(k);
					r.insertElementAt(p2, k);
					r.remove(k + 1);
					r.insertElementAt(p1, k + 1);
					again = true;
				}
			}
		}
		return r;
	}

	/**
	 * Set the convex hull points from results of the jaba engine
	 * @param s3d The data from the jaba engine
	 */
	private void calcConvex(Vector<Object> s3d) {
		for (int i = 0; i < s3d.size(); i++) {
			// Current sector
			FinalSect2D sect = (FinalSect2D) s3d.get(i);
			//String pb11 = formatter.format(sect.getBeta11());
			//String pb12 = formatter.format(sect.getBeta1());
			//String pb21 = formatter.format(sect.getBeta22());
			//String pb22 = formatter.format(sect.getBeta2());

			for (int z = 0; z < sect.countStation(); z++) {
				Station2D d = (sect.getstation()).get(z);
				DPoint p;
				for (int k = 0; k < allImperant.size(); k++) {
					p = (DPoint) allImperant.get(k);
					int dx = (int) (d.getVert()).getX();
					int dy = (int) (d.getVert()).getY();
					int px = (int) (p.getX() * 100);
					int py = (int) (p.getY() * 100);
					if ((dx == px) && (dy == py)) {
						allConvex.remove(p);
						allConvex.add(p);
					}
				}
			}
		}
		allConvex = orderByX(allConvex);
	}

}
