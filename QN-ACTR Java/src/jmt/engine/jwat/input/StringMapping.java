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
package jmt.engine.jwat.input;

import java.util.ArrayList;

public class StringMapping extends VariableMapping {

	@Override
	public double convertToDouble(String val) {
		return valMap.size();
	}

	//Update 28/10/2006: + funzione utilizzata per sampling che data una stringa restituisce tutti indice stringhe che la 
	//					   contengono come sottostringa
	public int[] getMatchingStringList(String s) {
		ArrayList<Integer> v = new ArrayList<Integer>(); //<Integer>
		for (int i = 0; i < valMap.size(); i++) {
			if (((String) valMap.get(i).getValue()).indexOf(s) != -1) {
				v.add(new Integer(i));
			}
		}
		int[] res = new int[v.size()];
		for (int i = 0; i < v.size(); i++) {
			res[i] = v.get(i).intValue();
		}
		v.clear();
		v = null;
		return res;
	}
}
