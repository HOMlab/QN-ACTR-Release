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
package jmt.framework.gui.graph;

/**
 * <p><b>Name:</b> MeasureValue</p> 
 * <p><b>Description:</b> 
 * This interface is used to specify a value with upper and lower bounds
 * </p>
 * <p><b>Date:</b> 23/gen/07
 * <b>Time:</b> 11:45:01</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public interface MeasureValue {
	/**
	 * @return the upper bound of value. Infinity if not set
	 */
	public double getUpperBound();

	/**
	 * @return the lower bound of value. Infinity if not set
	 */
	public double getLowerBound();

	/**
	 * @return the mean value
	 */
	public double getMeanValue();
}
