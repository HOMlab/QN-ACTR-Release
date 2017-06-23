package jmt.engine.jaba;

// newPoint.java
// 
// Mark F. Hulber
// May 1996
//
//
// newPoint is an extension of the Java Point class.  Additions include 
//    methods for making comparisons between points including relative 
//    direction, magnitude, and angular computations.
//
//

import java.awt.Point;

public class newPoint extends Point {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public newPoint(int nx, int ny) {
		super(nx, ny);
	}

	public double length() {
		return Math.sqrt(Math.pow(getX(), 2) + Math.pow(getY(), 2));
	}

	public int classify(newPoint p0, newPoint p1) {

		newPoint a = new newPoint((int) (p1.getX() - p0.getX()), (int) (p1.getY() - p0.getY()));
		newPoint b = new newPoint((int) (getX() - p0.getX()), (int) (getY() - p0.getY()));

		double sa = a.getX() * b.getY() - b.getX() * a.getY();

		if (sa > 0.0) {
			return 0; // LEFT
		}
		if (sa < 0.0) {
			return 1; // RIGHT
		}
		if ((a.getX() * b.getX() < 0.0) || (a.getY() * b.getY() < 0.0)) {
			return 2; // BEHIND
		}
		if (a.length() < b.length()) {
			return 3; // BEYOND
		}
		if (p0.equals(this)) {
			return 4; // ORIGIN
		}
		if (p1.equals(this)) {
			return 5; // DESTINATION
		}
		return 6; // BETWEEN
	}

	public double polarAngle() {

		if ((x == 0.0) && (y == 0.0)) {
			return -1.0;
		}
		if (x == 0.0) {
			return ((y > 0.0) ? 90 : 270);
		}
		double theta = Math.atan((double) y / x);
		theta *= 360 / (2 * Math.PI);
		if (x > 0.0) {
			return ((y >= 0.0) ? theta : 360 + theta);
		} else {
			return (180 + theta);
		}
	}

	public int polarCmp(newPoint p, newPoint q) {

		newPoint vp = new newPoint(p.x - this.x, p.y - this.y);
		newPoint vq = new newPoint(q.x - this.x, q.y - this.y);

		double pPolar = vp.polarAngle();
		double qPolar = vq.polarAngle();

		if (pPolar < qPolar) {
			return -1;
		}
		if (pPolar > qPolar) {
			return 1;
		}
		if (vp.length() < vq.length()) {
			return -1;
		}
		if (vp.length() > vq.length()) {
			return 1;
		}
		return 0;
	}

}
