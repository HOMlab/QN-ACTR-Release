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

package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;

/**
* TODO rivedere la descrizione
* 
* interface for all distributions parameters. You have to
* extend this class for every new distribution that you have.
*
* <br><br>Copyright (c) 2003
* <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
* @author Federico Granata (modified by Fabrizio Frontera - ffrontera@yahoo.it)
* @author Bertoli Marco, 10-oct-2005
*
*/
public abstract class AbstractParameter {
	/**
	 * check method definition.
	 * It provide a method for the user to verify if the parameter that he is using
	 * is correct.
	 *
	 * @return boolean indicating whether or not the parameter is right.
	 *
	 */
	public boolean check() {
		return true;
	}

	/**
	 * Sets mean for a given distribution parameter. This method is required to adjust distributions
	 * mean values into Load Dependent Service Time Strategy
	 * <br>Author: Bertoli Marco
	 * @param meanValue new mean value for this distribution
	 * @throws IncorrectDistributionParameterException if mean value is invalid for this distribution
	 */
	public void setMean(double meanValue) throws IncorrectDistributionParameterException {
		throw new IncorrectDistributionParameterException("Cannot set mean value for this distribution");
	}
}
