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
package jmt.engine.jwat.filters;

import jmt.engine.jwat.Observation;

public class FilterOnNumeric implements FilterOnVariable {
	private double minV;
	private double maxV;
	private int index;

	public FilterOnNumeric(double min, double max, int nVar) {
		minV = min;
		maxV = max;
		index = nVar;
	}

	public boolean isMatching(Observation o, int pos) {
		if (o.getIndex(index) >= minV && o.getIndex(index) <= maxV) {
			return true;
		}
		return false;
	}

}
