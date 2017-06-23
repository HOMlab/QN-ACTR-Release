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
 *
 * This is the parameter that should be passed to the Exponential
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 */

public class ExponentialPar extends AbstractParameter implements Parameter {

	private double lambda;

	/**
	 * It creates a new empty exponential parameter that must be set to be used.
	 *
	 */

	public ExponentialPar() {
	}

	/**
	 * It creates a new exponential parameter using the value provided by the user.
	 * @param lambda double containing the lambda parameter.
	 *
	 * @throws IncorrectDistributionParameterException if the value provided is not greater than zero.
	 *
	 */
	public ExponentialPar(double lambda) throws IncorrectDistributionParameterException {
		setLambda(lambda);

	}

	public ExponentialPar(Double wlambda) throws IncorrectDistributionParameterException {
		this(wlambda.doubleValue());

	}

	/**
	 * It returns the value of the parameter lambda of the exponential distribution.
	 *
	 * @return double with lamda
	 *
	 */

	public double getLambda() {
		return lambda;
	}

	/**
	 * it change the value of lambda.
	 * It allow the user to modify the value of the parameter lamda of the
	 * exponential distribution and verify if the value is correct that is if it is
	 * greater than zero.
	 *
	 * @param lambda double indicating the new value of the parameter lambda
	 * @throws IncorrectDistributionParameterException if the provided value is not greater than zero.
	 *
	 */

	public void setLambda(double lambda) throws IncorrectDistributionParameterException {
		if (lambda > 0) //verify that the parameter lambda is gtz, as required.
		{
			this.lambda = lambda;
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

	/**
	 * It controls if the parameter is correct or not.
	 * For the exponential distribution, the parameter is right if lambda is greater
	 * than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if (lambda > 0) {
			return true;
		} else {
			return false;
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
		if (meanValue <= 0 || Double.isInfinite(meanValue)) {
			throw new IncorrectDistributionParameterException("Mean value must be finite and greater than zero");
		}
		lambda = 1 / meanValue;
	}

} // end ExponentialPar
