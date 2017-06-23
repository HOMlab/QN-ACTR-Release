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
package jmt.engine.jwat;

import jmt.engine.jwat.input.StringMapping;
import jmt.engine.jwat.input.VariableMapping;

public class VariableString extends VariableNumber {
	// Utilizzata per mantenere l'associazione ra numero e stringa

	public VariableString(Observation[] valObs, String vName, int pos, short type, VariableMapping varMapping) {
		super(valObs, vName, pos, type, varMapping);
	}

	/**
	 * Returns Index-th value of the variable
	 * @param Index index of variable's value desired
	 * @return value of Index-th value
	 * @throws ArrayIndexOutOfBoundsException throws if Index is < 0 or > numbero of observations
	 */
	public String getOriginalValue(int Index) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return (String) mapping.getValue(obsValue[Index].getIndex(nVar));
		}
	}

	/**
	 * Returns the index-th value of the var-th variable of the observaion
	 * @param Index index of variable's value desired
	 * @param var variable index
	 * @return index-th value of the var-th variable
	 * @throws ArrayIndexOutOfBoundsException throws if Index is < 0 or > numbero of observations 
	 * or var < 0 or > number of variables ( elements of observation )
	 */
	public String getOriginalValue(int Index, int var) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return (String) mapping.getValue(obsValue[Index].getIndex(var));
		}
	}

	//UPDATE 28/10/2006: + funzione per ottenere liste dai valori matchabili
	public int[] getListOfMatching(String s) {
		return ((StringMapping) mapping).getMatchingStringList(s);
	}
}
