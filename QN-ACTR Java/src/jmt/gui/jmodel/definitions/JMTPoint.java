/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.gui.jmodel.definitions;

import java.awt.geom.Point2D;

/**
 * <p><b>Name:</b> JMTPoint</p> 
 * <p><b>Description:</b> 
 * A Point2D implementation class used to store position of a station 
 * </p>
 * <p><b>Date:</b> 21/feb/07
 * <b>Time:</b> 18:37:35</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTPoint extends Point2D {
	private double x, y;
	private boolean rotate;

	/**
	 * Builds a new JMTPoint at position (0,0) and no rotation.
	 */
	public JMTPoint() {
		x = 0;
		y = 0;
		rotate = false;
	}

	/**
	 * Builds a new JMTPoint
	 * @param x x coordinate
	 * @param y y coordinate
	 * @param rotate true if station is rotated
	 */
	public JMTPoint(double x, double y, boolean rotate) {
		this.x = x;
		this.y = y;
		this.rotate = rotate;
	}

	/**
	 * Builds a new JMTPoint from a given point
	 * @param point point for coordinates
	 * @param rotate true if station is rotated
	 */
	public JMTPoint(Point2D point, boolean rotate) {
		this.x = point.getX();
		this.y = point.getY();
		this.rotate = rotate;
	}

	/* (non-Javadoc)
	 * @see java.awt.geom.Point2D#getX()
	 */
	@Override
	public double getX() {
		return x;
	}

	/* (non-Javadoc)
	 * @see java.awt.geom.Point2D#getY()
	 */
	@Override
	public double getY() {
		return y;
	}

	/* (non-Javadoc)
	 * @see java.awt.geom.Point2D#setLocation(double, double)
	 */
	@Override
	public void setLocation(double x, double y) {
		this.x = x;
		this.y = y;
	}

	/* (non-Javadoc)
	 * @see java.awt.geom.Point2D#clone()
	 */
	@Override
	public Object clone() {
		return new JMTPoint(x, y, rotate);
	}

	/**
	 * @return true if station is rotated
	 */
	public boolean isRotate() {
		return rotate;
	}

	/**
	 * @param rotate true if station is rotated
	 */
	public void setRotate(boolean rotate) {
		this.rotate = rotate;
	}

	/**
	 * @param x x coordinate
	 */
	public void setX(double x) {
		this.x = x;
	}

	/**
	 * @param y y coordinate
	 */
	public void setY(double y) {
		this.y = y;
	}

}
