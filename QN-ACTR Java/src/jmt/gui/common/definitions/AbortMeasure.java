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

package jmt.gui.common.definitions;

/**
 * <p>Title: Abort Measure Interface</p>
 * <p>Description: This simple interface is used to specify an object able to abort simulation
 * measures. This is needed to add a layer of indirection in ResultsWindow dependances.</p>
 * 
 * @author Bertoli Marco
 *         Date: 26-set-2005
 *         Time: 12.06.45
 */
public interface AbortMeasure {
	/**
	 * Aborts a measure, given its index
	 * @param index index of the measure to be aborted
	 */
	public void abortMeasure(int index);
}
