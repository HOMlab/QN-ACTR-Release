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

import java.util.Date;

import jmt.engine.jwat.input.VariableMapping;

public class VariableDate extends VariableNumber {

	public VariableDate(Observation[] valObs, String vName, int pos, short type, VariableMapping varMapping) {
		super(valObs, vName, pos, type, varMapping);
		mapping = varMapping;
	}

	/**
	 * Returns Index-th value of the variable
	 * @param Index index of variable's value desired
	 * @return value of Index-th value
	 * @throws ArrayIndexOutOfBoundsException throws if Index is < 0 or > numbero of observations
	 */
	public Date getOriginalValue(int Index) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return (Date) mapping.getValue(obsValue[Index].getIndex(nVar));
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
	public Date getOriginalValue(int Index, int var) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return (Date) mapping.getValue(obsValue[Index].getIndex(var));
		}
	}

}
