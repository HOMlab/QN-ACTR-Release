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

/**
 * Description: 
 * @author Brambilla Davide Matr 667986, Fumagalli Claudio 667971
 * Class created 23-ott-2006 10.19.03 Darksch
 * 
 */
public class IntervalFilter implements FilterOnVariable {
	private int min, max;

	/**
	 * Il valore di indice della prima osservazione e' 0
	 * @param obsMin 
	 * @param obsMax
	 */
	public IntervalFilter(int obsMin, int obsMax) {
		min = obsMin;
		max = obsMax;
	}

	public boolean isMatching(Observation o, int pos) {
		if (pos < min || pos > max) {
			return false;
		} else {
			return true;
		}
	}

}
