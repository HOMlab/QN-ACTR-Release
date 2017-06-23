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
 * This is the parameter that should be passed to the Constant
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 */

public class ConstantDistrPar extends AbstractParameter implements Parameter {

	private double t;

	/**
	 * It creates a new constant parameter. It accepts a double greater
	 * than zero and uses this number as the constant value that will be returned.
	 *
	 * @param t Parameter of the constant distribution. It is the value which is always returned. It must be greater than zero.
	 * @throws IncorrectDistributionParameterException if the provided parameter is not greater than zero.
	 */
	public ConstantDistrPar(double t) throws IncorrectDistributionParameterException {
		this.t = t;
		if (!check()) {
			throw new IncorrectDistributionParameterException("t < 0");
		}
	}

	/**
	 * It creates a new constant parameter. It accepts a Double greater
	 * than zero and uses this number as the constant value that will be returned.
	 *
	 * @param t Parameter of the constant distribution. It is the value which is always returned. It must be greater than zero.
	 * @throws IncorrectDistributionParameterException if the provided parameter is not greater than zero.
	 */
	public ConstantDistrPar(Double t) throws IncorrectDistributionParameterException {
		this(t.doubleValue());
	}

	/**
	 * It controls whether the parameter is correct or not.
	 * For the constant distribution, the parameter is correct if it is >= than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */
	@Override
	public boolean check() {
		if (t < 0) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It returns the value of the parameter for the constant distribution, that is
	 * the constant value provided by the user.
	 *
	 * @return the value always returned by the distribution.
	 *
	 */
	public double getParameterValue() {
		return t;
	}

	/**
	 * It allow the user to change the value of the parameter of the constant distribution.
	 * It verify that the new value is correct (must be greater than zero).
	 *
	 * @param t double indicating the new value of the parameter
	 * @throws IncorrectDistributionParameterException when the provided parameter is not greater than zero.
	 *
	 */
	public void setParameterValue(double t) throws IncorrectDistributionParameterException {
		if (!(t < 0)) {
			this.t = t;
		} else {
			throw new IncorrectDistributionParameterException("t < 0");
		}
	}

	/**
	 * Sets mean for a given distribution parameter. This method is required to adjust distributions
	 * mean values into Load Dependent Service Time Strategy
	 * @param meanValue new mean value for this distribution
	 * Author: Bertoli Marco
	 * @throws IncorrectDistributionParameterException if mean value is invalid for this distribution
	 */
	@Override
	public void setMean(double meanValue) throws IncorrectDistributionParameterException {
		if (meanValue < 0 || Double.isInfinite(meanValue)) {
			throw new IncorrectDistributionParameterException("Mean value must be finite and greater than zero");
		}
		t = meanValue;
	}

} // end ConstantDistrPar
