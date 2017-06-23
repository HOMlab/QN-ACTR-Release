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

public class TrimmingFilter implements FilterOnVariable {
	private int index;
	private boolean up; //true elimina da index in poi altrimenti al contrario

	public TrimmingFilter(int index, boolean b) {
		this.index = index;
		up = b;
	}

	/**
	 * Ritorna true se l'osservazione matcha il filtro
	 */
	public boolean isMatching(Observation o, int pos) {
		if (up) {
			if (pos < index) {
				return true;
			} else {
				return false;
			}
		} else {
			if (pos > index) {
				return true;
			} else {
				return false;
			}
		}
	}

}
