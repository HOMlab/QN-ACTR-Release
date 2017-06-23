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

import java.awt.geom.Point2D;

public class DPoint extends Point2D.Double {

	private String label;
	private boolean select;

	/**
	 * Make a DPoint from the coordinates
	 * @param x The x of the point
	 * @param y The y of the point
	 */
	public DPoint(double x, double y) {
		super(x, y);
		label = "";
		select = false;
	}

	/**
	 * Make a DPoint from coordinates and station's name
	 * @param x The x of the point
	 * @param y The y of the point
	 * @param label The name of the point
	 */
	public DPoint(double x, double y, String label) {
		super(x, y);
		this.label = label;
		select = false;
	}

	/**
	 * Return the label of the point
	 * @return The laberl
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * Return true if the point is selected
	 * @return True if the point is selected
	 */
	public boolean isSelect() {
		return select;
	}

	/**
	 * Set the selection value of the point
	 * @param isSelect If the point is selected or not
	 */
	public void setSelect(boolean isSelect) {
		select = isSelect;
	}

	public double polarAngle() {

		if ((x == 0.0) && (y == 0.0)) {
			return -1.0;
		}
		if (x == 0.0) {
			return ((y > 0.0) ? Math.PI / 2 : (Math.PI / 2) * 3);
		}
		double theta = Math.atan(y / x);
		//  theta -=0.061;
		if (x > 0.0) {
			return ((y >= 0.0) ? theta : (2 * Math.PI) + theta);
		} else {
			return theta;
		}
	}

	/**
	 * Control if the integer part of the coordinates are equals
	 * @return true if the integer part of the coordinates are equals
	 */
	public boolean intEquals(DPoint p) {
		int xP1 = (int) x;
		int yP1 = (int) y;
		int xP2 = (int) p.getX();
		int yP2 = (int) p.getY();

		if ((xP1 == xP2) && (yP1 == yP2)) {
			return true;
		}
		return false;
	}
}